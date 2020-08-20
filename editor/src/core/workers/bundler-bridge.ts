import { Machine, interpret, send, assign, actions, Interpreter } from 'xstate'
import { FileContent } from './common/worker-types'
import { TypeDefinitions } from '../shared/npm-dependency-types'
import { ProjectContents, UIJSFile, CodeFile } from '../shared/project-file-types'
import {
  InitCompleteMessage,
  OutgoingWorkerMessage,
  createInitTSWorkerMessage,
  BuildResultMessage,
  createUpdateFileMessage,
  UpdateProcessedMessage,
} from './ts/ts-worker'
import utils from '../../utils/utils'
import { workerForFile } from './utils'

export interface BundlerContext {
  queuedUpdateFiles: { [key: string]: FileContent }
  fileQueuedForProcessing: null | {
    fileName: string
    fileContent: FileContent
  }
}

type BundlerSchema = {
  states: {
    worker: {
      states: {
        uninitialized: {}
        initializing: {}
        initagain: {}
        idle: {}
        processing: {}
        error: {}
      }
    }
    queue: {
      states: {
        ready: {}
        pushing: {}
        popping: {}
      }
    }
  }
}

interface InitializeEvent {
  type: 'INITIALIZE'
  payload: {
    typeDefinitions: TypeDefinitions
    projectContents: ProjectContents
    jobID: string | null
  }
}
function initializeEvent(
  typeDefinitions: TypeDefinitions,
  projectContents: ProjectContents,
  jobID: string | null,
): InitializeEvent {
  return {
    type: 'INITIALIZE',
    payload: {
      typeDefinitions,
      projectContents,
      jobID: jobID,
    },
  }
}

interface PushEvent {
  type: 'PUSH'
  payload: {
    fileName: string
    fileContent: FileContent
  }
}
function pushEvent(fileName: string, fileContent: FileContent): PushEvent {
  return {
    type: 'PUSH',
    payload: {
      fileName,
      fileContent,
    },
  }
}

interface PopEvent {
  type: 'POP'
}

interface ProcessFileFromQueueEvent {
  type: 'PROCESS_FILE_FROM_QUEUE'
}

type BundlerEvent = InitializeEvent | PushEvent | PopEvent | ProcessFileFromQueueEvent

function popFromQueue(context: BundlerContext): BundlerContext {
  const filenames = Object.keys(context.queuedUpdateFiles)
  const firstItemFilename = filenames[0]
  const content = context.queuedUpdateFiles[firstItemFilename]
  const updatedQueue = {
    ...context.queuedUpdateFiles,
  }
  delete updatedQueue[firstItemFilename]
  return {
    ...context,
    queuedUpdateFiles: updatedQueue,
    fileQueuedForProcessing: {
      fileName: firstItemFilename,
      fileContent: content,
    },
  }
}

function pushToQueue(context: BundlerContext, event: any): BundlerContext {
  const updatedQueue = {
    ...context.queuedUpdateFiles,
    [event.payload.fileName]: event.payload.fileContent,
  }
  return {
    ...context,
    queuedUpdateFiles: updatedQueue,
  }
}

function emptyContext(context: BundlerContext, event: any): BundlerContext {
  return {
    queuedUpdateFiles: {},
    fileQueuedForProcessing: null,
  }
}

export const bundlerMachine = Machine<BundlerContext, BundlerSchema, BundlerEvent>({
  id: 'bundlermachine',
  type: 'parallel',
  // the initial context. very similar to a redux-like state object.
  // the context object is immutable, and you must use Actions (side effects) to change its value
  // the Action to change the context's value is called `assign`. dont ask me why.
  context: {
    queuedUpdateFiles: {},
    fileQueuedForProcessing: null,
  },
  // the top level state machine has two sub-machines: worker, and queue.
  states: {
    // the worker state machine is responsible for communicating with the worker via Promises
    worker: {
      initial: 'uninitialized' as const,
      states: {
        // it starts in an unitialized state, the only valid message it can respond to is INITIALIZE
        uninitialized: {
          // so what is happening here? `on` is an object that is in the shape of
          // `{[eventName: string]: TransitionObject}
          // INITIALIZE is the event we are listening to, and as a
          // response we transition to a new state called `initializing`
          // we could add configuration here, fire Actions (side effects)
          // and optionally we can omit `target`, which means we do not switch state, just fire side effects for example.
          on: {
            INITIALIZE: {
              target: 'initializing',
            },
          },
        },
        // This state invokes the initializeWorkerPromise promise. (passed in as configuration)
        // when the promise resolves, the onDone handler will be executed
        initializing: {
          entry: [assign(emptyContext)],
          exit: [],
          on: {
            INITIALIZE: {
              target: 'initagain',
              actions: [],
            },
          },
          invoke: {
            id: 'initializeBundler',
            // this here is a string key, we expect the same string to be in the config object.
            // instead of a string key, this could be a function call that returns a Promise, for faster prototyping
            src: 'initializeWorkerPromise',
            // when the promise resolves, the onDone event is fired.
            onDone: {
              target: 'idle',
              actions: [],
            },
            onError: {
              target: 'error',
              actions: [
                actions.log(
                  (context, event) =>
                    `Bundler Fatal Error: the worker initialization promise rejected. Error: ${JSON.stringify(
                      event.data,
                    )}`,
                ),
              ],
            },
          },
        },
        // If there is another init request in the initialization state, we have to send another init
        // request to the worker. However, we can't just transition from initialization state to
        // the same state (there is no such transition), but we have to go to a different state and
        // then automatically go to the initializing state again, so there is a real state transition.
        // This initagain state is that temporary state.
        initagain: {
          on: {
            '': {
              target: 'initializing',
            },
          },
        },
        idle: {
          // when we enter the idle state, fire a POP event, just in case the queue is not empty
          entry: [send('POP')],
          on: {
            // when a PUSH event enters the system, put a POP event to the back of the State Machine's event queue.
            PUSH: {
              // the POP event will make our queue state machine to fire a PROCESS_FILE_FROM_QUEUE event
              actions: [send('POP')],
            },
            // the PROCESS_FILE_FROM_QUEUE event means the queue was not empty,
            // and now there is a file ready to be processed, let's switch states!
            PROCESS_FILE_FROM_QUEUE: {
              target: 'processing',
            },
            INITIALIZE: {
              target: 'initializing',
            },
          },
        },
        processing: {
          on: {
            INITIALIZE: {
              target: 'initializing',
            },
          },
          invoke: {
            id: 'sendUpdateFileMessage',
            // let's call the updateFileWorkerPromise, and wait until it resolves
            src: 'updateFileWorkerPromise',
            onDone: {
              // success! the promise resolved, we can go back to idle
              target: 'idle',
              // but before we go,...
              actions: [
                assign({
                  // let's delete the property named fileQueuedForProcessing from context
                  fileQueuedForProcessing: (context, event) => {
                    // instead of this callback function, I could just write `null` here, except typescript complained
                    return null as any
                  },
                }),
              ],
            },
            onError: {
              target: 'error',
              actions: [
                actions.log(
                  (context, event) =>
                    `Bundler Fatal Error: The update file promise rejected. Error: ${JSON.stringify(
                      event.data,
                    )}`,
                ),
              ],
            },
          },
        },
        error: {},
      },
    },
    // queue is a state machine implementation of Rheese's queueing code
    queue: {
      initial: 'ready',
      states: {
        ready: {
          on: {
            PUSH: {
              target: 'pushing',
            },
            POP: {
              target: 'popping',
              // we only process popping on the condition that the queue is not 0
              cond: (context) => Object.keys(context.queuedUpdateFiles).length !== 0,
            },
          },
        },
        pushing: {
          // entry describes a list of actions (side effects) to be executed when we step into this state
          // it has a dual, called `exit`
          entry: [
            // assign is like React.setState for the `context` object. here we describe that we want to set
            // the value of queuedUpdateFiles to include event.payload.fileContent.
            assign((context, event) => pushToQueue(context, event)),
          ],
          on: {
            // the `''` event is a special event, that is fired when we step into this state. since I am
            // describing a transition with a target as the handler,
            // it basically means "as soon as we step into the state called `pushing`, move to a new state"
            '': {
              target: 'ready',
            },
          },
        },
        popping: {
          entry: [
            assign((context) => popFromQueue(context)),
            // we fire the PROCESS_FILE_FROM_QUEUE, letting the bundler state machine know it can start working!
            send('PROCESS_FILE_FROM_QUEUE'),
          ],
          on: {
            // same as in pushing, I should remove this copy paste, either by making this object a global const,
            // or by coming up with a better representation than the empty/ready states.
            '': [
              {
                target: 'ready',
              },
            ],
          },
        },
      },
    },
  },
})

export interface BundlerWorker {
  addMessageListener(listener: (ev: MessageEvent) => any): void
  removeMessageListener(listener: (ev: MessageEvent) => any): void
  postMessage(message: any): void
}

export class RealBundlerWorker implements BundlerWorker {
  worker = workerForFile('editor/tsWorker.js')

  addMessageListener = (listener: (ev: MessageEvent) => any): void => {
    this.worker.addEventListener('message', listener)
  }

  removeMessageListener = (listener: (ev: MessageEvent) => any): void => {
    this.worker.removeEventListener('message', listener)
  }

  postMessage = (message: any): void => {
    this.worker.postMessage(message)
  }
}

export class NewBundlerWorker {
  private worker: BundlerWorker
  private stateMachine: Interpreter<BundlerContext, BundlerSchema, BundlerEvent>
  constructor(worker: BundlerWorker) {
    this.worker = worker
    this.stateMachine = interpret(
      bundlerMachine.withConfig({
        services: {
          updateFileWorkerPromise: (context, event) => {
            if (context.fileQueuedForProcessing == null) {
              return Promise.reject(
                'Bundler Bridge Error: context.fileQueuedForProcessing cannot be null',
              )
            }
            return sendIdGuardedUpdateFilePromise(
              this.worker,
              context.fileQueuedForProcessing.fileName,
              context.fileQueuedForProcessing.fileContent,
            )
          },
          initializeWorkerPromise: (context, event) =>
            sendIdGuardedinitializeWorkerPromise(
              this.worker,
              event.payload.typeDefinitions,
              event.payload.projectContents,
              event.payload.jobID,
            ),
        },
      }),
    )
    this.stateMachine.start()
  }

  sendInitMessage(
    typeDefinitions: TypeDefinitions,
    projectContents: ProjectContents,
    jobID: string | null,
  ) {
    this.stateMachine.send(initializeEvent(typeDefinitions, projectContents, jobID))
  }

  sendUpdateFileMessage(filename: string, content: FileContent) {
    this.stateMachine.send(pushEvent(filename, content))
  }

  // TODO KILLME
  addBundleResultEventListener(handler: (e: MessageEvent) => void) {
    this.worker.addMessageListener(handler)
  }

  // TODO KILLME
  removeBundleResultEventListener(handler: (e: MessageEvent) => void) {
    this.worker.removeMessageListener(handler)
  }
}

function sendIdGuardedinitializeWorkerPromise(
  worker: BundlerWorker,
  typeDefinitions: TypeDefinitions,
  projectContents: ProjectContents,
  jobID: string | null,
): Promise<InitCompleteMessage> {
  const generatedJobID = jobID == null ? utils.generateUUID() : jobID

  return new Promise((resolve, reject) => {
    function handleMessage(event: MessageEvent) {
      const data: OutgoingWorkerMessage = event.data
      switch (data.type) {
        case 'initcomplete':
          if (data.jobID === generatedJobID) {
            worker.removeMessageListener(handleMessage)
            resolve(data)
          }
      }
    }
    worker.addMessageListener(handleMessage)
    worker.postMessage(
      createInitTSWorkerMessage(typeDefinitions, projectContents, 'build', generatedJobID),
    )
  })
}

function sendIdGuardedUpdateFilePromise(
  worker: BundlerWorker,
  filename: string,
  content: string | UIJSFile | CodeFile,
): Promise<BuildResultMessage | UpdateProcessedMessage> {
  const generatedJobID = utils.generateUUID()
  return new Promise((resolve, reject) => {
    function handleMessage(event: MessageEvent) {
      const data: OutgoingWorkerMessage = event.data
      switch (data.type) {
        case 'build':
          if (data.jobID === generatedJobID) {
            worker.removeMessageListener(handleMessage)
            resolve(data)
          }
          break
        case 'updateprocessed':
          if (data.jobID === generatedJobID) {
            worker.removeMessageListener(handleMessage)
            resolve(data)
          }
          break
      }
    }
    worker.addMessageListener(handleMessage)
    worker.postMessage(createUpdateFileMessage(filename, content, generatedJobID))
  })
}
