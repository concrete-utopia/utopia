import { interpret } from 'xstate'
import { bundlerMachine } from './bundler-bridge'
import utils from '../../utils/utils'
import type { InitCompleteMessage, BuildResultMessage } from './common/worker-types'

const initEvent = {
  type: 'INITIALIZE',
  payload: {
    typeDefinitions: {},
    projectContents: {},
    jobID: null,
  },
} as const

const updateFileEvent = {
  type: 'PUSH',
  payload: {
    fileName: 'test_filename.js',
    fileContent: '',
  },
} as const

describe('Bundler State Machine', () => {
  it('initializes', async () => {
    const initReadyPromise = Promise.resolve<InitCompleteMessage>({
      type: 'initcomplete',
      jobID: 'no-id',
    })
    const mockInitService = jest.fn(() => initReadyPromise)
    const mockUpdateFileService = jest.fn(() =>
      Promise.resolve<BuildResultMessage>({
        type: 'build',
        exportsInfo: [],
        jobID: 'no-id',
        buildResult: {},
        buildType: 'full-build',
      }),
    )

    const stateMachine = interpret(
      bundlerMachine.withConfig({
        services: {
          initializeWorkerPromise: mockInitService,
          updateFileWorkerPromise: mockUpdateFileService,
        },
      }),
    )
    stateMachine.start()
    stateMachine.send(initEvent)

    expect(mockInitService.mock.calls.length).toBe(1)

    await initReadyPromise

    expect(stateMachine.state.matches('worker.idle')).toBeTruthy()
  })

  it('starts the worker when it receives a file to update', async () => {
    const initReadyPromise = Promise.resolve<InitCompleteMessage>({
      type: 'initcomplete',
      jobID: 'no-id',
    })
    const mockInitService = jest.fn(() => initReadyPromise)
    const updateFilePromise = utils.defer<BuildResultMessage>()
    const mockUpdateFileService = jest.fn(() => updateFilePromise)

    const stateMachine = interpret(
      bundlerMachine.withConfig({
        services: {
          initializeWorkerPromise: mockInitService,
          updateFileWorkerPromise: mockUpdateFileService,
        },
      }),
    ).onTransition((state) => {
      // enable this to listenin in on the state transitions
      // console.log('TRANSITION', state.event.type, state.toStrings())
    })
    stateMachine.start()
    stateMachine.send(initEvent)

    expect(mockInitService.mock.calls.length).toBe(1)

    await initReadyPromise
    // the worker should be in the Idle state now

    stateMachine.send(updateFileEvent)
    expect(mockUpdateFileService.mock.calls.length).toBe(1)
    updateFilePromise.resolve({
      type: 'build',
      exportsInfo: [],
      jobID: 'no-id',
      buildResult: {},
      buildType: 'full-build',
    })
    await updateFilePromise
    // check that the worker goes back to idle after the bundler promise resolves
    expect(stateMachine.state.matches('worker.idle')).toBeTruthy()
  })

  it('handles a long queue', async () => {
    const initReadyPromise = Promise.resolve<InitCompleteMessage>({
      type: 'initcomplete',
      jobID: 'no-id',
    })
    const mockInitService = jest.fn(() => initReadyPromise)
    const mockUpdateFileService = jest.fn()

    const stateMachine = interpret(
      bundlerMachine.withConfig({
        services: {
          initializeWorkerPromise: mockInitService,
          updateFileWorkerPromise: mockUpdateFileService,
        },
      }),
    ).onTransition((state) => {
      // enable this to listenin in on the state transitions
      // console.log('TRANSITION', state.event, state.toStrings(), state.context)
    })
    stateMachine.start()
    stateMachine.send(initEvent)

    expect(mockInitService.mock.calls.length).toBe(1)

    await initReadyPromise
    // the worker should be in the Idle state now

    // let's quickly push 4 jobs to the queue

    const promise1 = utils.defer()
    mockUpdateFileService.mockImplementationOnce(() => promise1)
    stateMachine.send({
      type: 'PUSH',
      payload: {
        fileName: 'test_filename1.js',
        fileContent: '',
      },
    })

    const promise2 = utils.defer()
    mockUpdateFileService.mockImplementationOnce(() => promise2)
    stateMachine.send({
      type: 'PUSH',
      payload: {
        fileName: 'test_filename2.js',
        fileContent: '',
      },
    })
    const promise3 = utils.defer()
    mockUpdateFileService.mockImplementationOnce(() => promise3)
    stateMachine.send({
      type: 'PUSH',
      payload: {
        fileName: 'test_filename3.js',
        fileContent: '',
      },
    })
    const promise4 = utils.defer()
    mockUpdateFileService.mockImplementationOnce(() => promise4)
    stateMachine.send({
      type: 'PUSH',
      payload: {
        fileName: 'test_filename4.js',
        fileContent: '',
      },
    })

    // at this point the worker should have only received the first updateFile message
    expect(mockUpdateFileService.mock.calls.length).toBe(1)
    expect(stateMachine.state.matches('worker.processing')).toBeTruthy()

    // let's simulate the bundler "processed" the first file separately from the rest
    promise1.resolve()
    await promise1

    // now the queue should be 2 remaining files, one file loaded in "up next", and weWantToEmit should be true
    expect(stateMachine.state.context.fileQueuedForProcessing).toEqual({
      fileContent: '',
      fileName: 'test_filename2.js',
    })
    expect(stateMachine.state.context.queuedUpdateFiles).toEqual({
      'test_filename3.js': '',
      'test_filename4.js': '',
    })

    // we let it work down the queue
    promise2.resolve()
    await promise2
    promise3.resolve()
    await promise3
    promise4.resolve()
    await promise4

    expect(mockUpdateFileService.mock.calls.length).toBe(4)
    expect(mockUpdateFileService.mock.calls[0][0].fileQueuedForProcessing.fileName).toEqual(
      'test_filename1.js',
    )
    expect(mockUpdateFileService.mock.calls[1][0].fileQueuedForProcessing.fileName).toEqual(
      'test_filename2.js',
    )
    expect(mockUpdateFileService.mock.calls[2][0].fileQueuedForProcessing.fileName).toEqual(
      'test_filename3.js',
    )
    expect(mockUpdateFileService.mock.calls[3][0].fileQueuedForProcessing.fileName).toEqual(
      'test_filename4.js',
    )
    expect(stateMachine.state.matches('worker.idle')).toBeTruthy()
  })

  it('goes to a simple error state in case the init promise throws', (done) => {
    const initReadyPromise = Promise.reject('testing a promise error handler, ignore me')
    const mockInitService = jest.fn(() => initReadyPromise)

    const mockUpdateFileService = jest.fn()

    const stateMachine = interpret(
      bundlerMachine.withConfig({
        services: {
          initializeWorkerPromise: mockInitService,
          updateFileWorkerPromise: mockUpdateFileService,
        },
      }),
    ).onTransition((state) => {
      // enable this to listenin in on the state transitions
      // console.log('TRANSITION', state.event.type, state.toStrings())
    })
    stateMachine.start()
    stateMachine.send(initEvent)

    expect(mockInitService.mock.calls.length).toBe(1)

    initReadyPromise.catch(() => {
      expect(stateMachine.state.matches('worker.error')).toBeTruthy()
      done()
    })
  })

  it('goes to a simple error state in case the update file promise throws', (done) => {
    const initReadyPromise = Promise.resolve<InitCompleteMessage>({
      type: 'initcomplete',
      jobID: 'no-id',
    })
    const mockInitService = jest.fn(() => initReadyPromise)

    const mockUpdateFileService = jest.fn()

    const stateMachine = interpret(
      bundlerMachine.withConfig({
        services: {
          initializeWorkerPromise: mockInitService,
          updateFileWorkerPromise: mockUpdateFileService,
        },
      }),
    ).onTransition((state) => {
      // enable this to listenin in on the state transitions
      // console.log('TRANSITION', state.event.type, state.toStrings())
    })
    stateMachine.start()
    stateMachine.send(initEvent)

    expect(mockInitService.mock.calls.length).toBe(1)

    void initReadyPromise.then(() => {
      // the worker should be in the Idle state now

      const updateFilePromiseWillFail = utils.defer<BuildResultMessage>()
      mockUpdateFileService.mockImplementationOnce(() => updateFilePromiseWillFail)

      stateMachine.send(updateFileEvent)
      expect(mockUpdateFileService.mock.calls.length).toBe(1)
      updateFilePromiseWillFail.reject('I am testing the error branch, ignore me')
      updateFilePromiseWillFail.catch(async () => {
        // check that the worker goes back to idle after the bundler promise rejects
        expect(stateMachine.state.matches('worker.error')).toBeTruthy()
        done()
      })
    })
  })
})
