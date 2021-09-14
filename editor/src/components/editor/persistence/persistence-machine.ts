import {
  actions,
  assign,
  createMachine,
  DoneInvokeEvent,
  interpret,
  Interpreter,
  send,
} from 'xstate'
import type { Model } from 'xstate/lib/model.types'
import {
  assetToSave,
  AssetToSave,
  createNewProjectID,
  downloadAssetsFromProject,
  loadProject as loadServerProject,
  saveAssets,
  saveThumbnail,
  updateSavedProject,
} from '../server'
const { choose } = actions
import localforage from 'localforage'
import {
  fetchLocalProject as loadLocalProject,
  localProjectKey,
  deleteLocalProject,
} from '../../../common/persistence'
import { arrayContains, NO_OP, projectURLForProject } from '../../../core/shared/utils'
import { checkProjectOwnership } from '../../../common/server'
import { PersistentModel, createNewProjectName } from '../store/editor-state'
import { addFileToProjectContents, getAllProjectAssetFiles } from '../../assets'
import { getFileExtension } from '../../../core/shared/file-utils'
import { AssetFile, ImageFile, ProjectFile } from '../../../core/shared/project-file-types'
import { assetFile, imageFile, isImageFile } from '../../../core/model/project-file-utils'
import { EditorAction, EditorDispatch } from '../action-types'
import {
  setForkedFromProjectID,
  setProjectID,
  setProjectName,
  showToast,
  updateFile,
} from '../actions/action-creators'
import { notice } from '../../common/notice'
import { getPNGBufferOfElementWithID } from '../screenshot-utils'
import { defaultProject } from '../../../sample-projects/sample-project-utils'
import {
  PersistenceBackendAPI,
  ProjectLoadResult,
  ProjectModel,
  ProjectWithFileChanges,
} from './persistence-types'

interface NewEvent {
  type: 'NEW'
}

function newEvent(): NewEvent {
  return {
    type: 'NEW',
  }
}

interface ProjectIdCreatedEvent {
  type: 'PROJECT_ID_CREATED'
  projectId: string
}

function projectIdCreatedEvent(projectId: string): ProjectIdCreatedEvent {
  return {
    type: 'PROJECT_ID_CREATED',
    projectId: projectId,
  }
}

interface NewProjectCreatedEvent {
  type: 'NEW_PROJECT_CREATED'
  projectId: string
  projectModel: ProjectModel
}

function newProjectCreatedEvent(
  projectId: string,
  projectModel: ProjectModel,
): NewProjectCreatedEvent {
  return {
    type: 'NEW_PROJECT_CREATED',
    projectId: projectId,
    projectModel: projectModel,
  }
}

interface LoadEvent {
  type: 'LOAD'
  projectId: string
}

function loadEvent(projectId: string): LoadEvent {
  return {
    type: 'LOAD',
    projectId: projectId,
  }
}

interface LoadCompleteEvent {
  type: 'LOAD_COMPLETE'
  projectId: string
  projectModel: ProjectModel
}

function loadCompleteEvent(projectId: string, projectModel: ProjectModel): LoadCompleteEvent {
  return {
    type: 'LOAD_COMPLETE',
    projectId: projectId,
    projectModel: projectModel,
  }
}

interface LoadFailedEvent {
  type: 'LOAD_FAILED'
}

function loadFailedEvent(): LoadFailedEvent {
  return {
    type: 'LOAD_FAILED',
  }
}

interface SaveEvent {
  type: 'SAVE'
  projectModel: ProjectModel
}

function saveEvent(projectModel: ProjectModel): SaveEvent {
  return {
    type: 'SAVE',
    projectModel: projectModel,
  }
}

interface SaveCompleteEvent {
  type: 'SAVE_COMPLETE'
  saveResult: ProjectWithFileChanges
}

function saveCompleteEvent(saveResult: ProjectWithFileChanges): SaveCompleteEvent {
  return {
    type: 'SAVE_COMPLETE',
    saveResult: saveResult,
  }
}

interface InnerSaveEvent {
  type: 'INNER_SAVE'
  projectModel: ProjectModel
}

function innerSave(projectModel: ProjectModel): InnerSaveEvent {
  return {
    type: 'INNER_SAVE',
    projectModel: projectModel,
  }
}

interface ForkEvent {
  type: 'FORK'
}

function forkEvent(): ForkEvent {
  return {
    type: 'FORK',
  }
}

interface DownloadAssetsCompleteEvent {
  type: 'DOWNLOAD_ASSETS_COMPLETE'
  downloadAssetsResult: ProjectWithFileChanges
}

function downloadAssetsCompleteEvent(
  downloadAssetsResult: ProjectWithFileChanges,
): DownloadAssetsCompleteEvent {
  return {
    type: 'DOWNLOAD_ASSETS_COMPLETE',
    downloadAssetsResult: downloadAssetsResult,
  }
}

interface CheckOwnershipCompleteEvent {
  type: 'CHECK_OWNERSHIP_COMPLETE'
  isOwner: boolean
}

function checkOwnershipCompleteEvent(isOwner: boolean): CheckOwnershipCompleteEvent {
  return {
    type: 'CHECK_OWNERSHIP_COMPLETE',
    isOwner: isOwner,
  }
}

type CoreEvent =
  | NewEvent
  | ProjectIdCreatedEvent
  | NewProjectCreatedEvent
  | LoadEvent
  | LoadCompleteEvent
  | LoadFailedEvent
  | CheckOwnershipCompleteEvent
  | SaveEvent
  | SaveCompleteEvent
  | ForkEvent
  | DownloadAssetsCompleteEvent
  | InnerSaveEvent

interface BackendCreateProjectIdEvent {
  type: 'BACKEND_CREATE_PROJECT_ID'
}

function backendCreateProjectIdEvent(): BackendCreateProjectIdEvent {
  return {
    type: 'BACKEND_CREATE_PROJECT_ID',
  }
}

interface BackendDownloadAssetsEvent {
  type: 'BACKEND_DOWNLOAD_ASSETS'
  projectId: string
  projectModel: ProjectModel
}

function backendDownloadAssetsEvent(
  projectId: string,
  projectModel: ProjectModel,
): BackendDownloadAssetsEvent {
  return {
    type: 'BACKEND_DOWNLOAD_ASSETS',
    projectId: projectId,
    projectModel: projectModel,
  }
}

interface BackendServerSaveEvent {
  type: 'BACKEND_SERVER_SAVE'
  projectId: string
  projectModel: ProjectModel
}

function backendServerSaveEvent(
  projectId: string,
  projectModel: ProjectModel,
): BackendServerSaveEvent {
  return {
    type: 'BACKEND_SERVER_SAVE',
    projectId: projectId,
    projectModel: projectModel,
  }
}

interface BackendLocalSaveEvent {
  type: 'BACKEND_LOCAL_SAVE'
  projectId: string
  projectModel: ProjectModel
}

function backendLocalSaveEvent(
  projectId: string,
  projectModel: ProjectModel,
): BackendLocalSaveEvent {
  return {
    type: 'BACKEND_LOCAL_SAVE',
    projectId: projectId,
    projectModel: projectModel,
  }
}

interface BackendLoadEvent {
  type: 'BACKEND_LOAD'
  projectId: string
}

function backendLoadEvent(projectId: string): BackendLoadEvent {
  return {
    type: 'BACKEND_LOAD',
    projectId: projectId,
  }
}

interface BackendCheckOwnershipEvent {
  type: 'BACKEND_CHECK_OWNERSHIP'
  projectId: string
}

function backendCheckOwnershipEvent(projectId: string): BackendCheckOwnershipEvent {
  return {
    type: 'BACKEND_CHECK_OWNERSHIP',
    projectId: projectId,
  }
}

interface BackendErrorEvent {
  type: 'BACKEND_ERROR'
  message: string
}

function backendErrorEvent(message: string): BackendErrorEvent {
  return {
    type: 'BACKEND_ERROR',
    message: message,
  }
}

type BackendEvent =
  | BackendCreateProjectIdEvent
  | BackendDownloadAssetsEvent
  | BackendServerSaveEvent
  | BackendLocalSaveEvent
  | BackendLoadEvent
  | BackendCheckOwnershipEvent
  | BackendErrorEvent

interface UserLogInEvent {
  type: 'USER_LOG_IN'
}

function userLogInEvent(): UserLogInEvent {
  return {
    type: 'USER_LOG_IN',
  }
}

interface UserLogOutEvent {
  type: 'USER_LOG_OUT'
}

function userLogOutEvent(): UserLogOutEvent {
  return {
    type: 'USER_LOG_OUT',
  }
}

type UserEvent = UserLogInEvent | UserLogOutEvent

type PersistenceEvent = CoreEvent | BackendEvent | UserEvent

interface PersistenceContext {
  projectId?: string
  project?: ProjectModel
  queuedSave?: ProjectModel
  projectOwned: boolean
  loggedIn: boolean
}

// Core States
const Empty = 'empty'
const Ready = 'ready'
const CreatingNew = 'creating'
const Loading = 'loading'
const Saving = 'saving'
const Forking = 'forking'

// InternalCreatingNewStates
const CreatingProjectId = 'creating-project-id'
const ProjectIdCreated = 'project-id-created'
const ProjectCreated = 'project-created'

// InternalLoadingStates
const LoadingProject = 'loading-project'
const CheckingOwnership = 'checking-ownership'
const ProjectLoaded = 'project-loaded'

// InternalForkingStates
const DownloadingAssets = 'downloading-assets'
const ProjectForked = 'project-forked'

// Backend States
const BackendIdle = 'idle'
const BackendCreatingProjectId = 'creating-project-id'
const BackendCheckingOwnership = 'checking-ownership'
const BackendDownloadingAssets = 'downloading-assets'
const BackendServerSaving = 'server-saving'
const BackendLocalSaving = 'local-saving'
const BackendLoading = 'loading'

// User States
const LoggedOut = 'logged-out'
const LoggedIn = 'logged-in'

const queuePush = assign<PersistenceContext, SaveEvent>((_context, event) => {
  return {
    queuedSave: event.projectModel,
  }
})
const checkQueue = choose<PersistenceContext, PersistenceEvent>([
  {
    cond: (context) => context.queuedSave != null,
    actions: send((context) => saveEvent(context.queuedSave!)),
  },
])
// The queue clearing has to be handled separately as assign actions are batched and handled before other actions
// until xstate v5: https://xstate.js.org/docs/guides/actions.html#action-order
const queueClear = assign<PersistenceContext, PersistenceEvent>({
  queuedSave: undefined,
})

function createNewProjectModel(): Promise<ProjectModel> {
  return Promise.resolve({
    name: createNewProjectName(),
    content: defaultProject(),
  })
}

export const persistenceMachine = createMachine<
  Model<PersistenceContext, PersistenceEvent>,
  PersistenceContext,
  PersistenceEvent
>({
  id: 'persistence-parallel',
  type: 'parallel',
  context: { projectOwned: false, loggedIn: false },
  states: {
    // Backend Communication
    backend: {
      id: 'backend',
      initial: BackendIdle,
      states: {
        [BackendIdle]: {
          on: {
            BACKEND_CREATE_PROJECT_ID: BackendCreatingProjectId,
            BACKEND_CHECK_OWNERSHIP: BackendCheckingOwnership,
            BACKEND_DOWNLOAD_ASSETS: BackendDownloadingAssets,
            BACKEND_SERVER_SAVE: BackendServerSaving,
            BACKEND_LOCAL_SAVE: BackendLocalSaving,
            BACKEND_LOAD: BackendLoading,
          },
        },
        [BackendCreatingProjectId]: {
          invoke: {
            id: 'create-project-id',
            src: 'getNewProjectId',
            onDone: {
              target: BackendIdle,
              actions: [
                send((_, event: DoneInvokeEvent<string>) => projectIdCreatedEvent(event.data)),
              ],
            },
            onError: {
              target: BackendIdle,
              actions: send((_, event) => backendErrorEvent(event.data)),
            },
          },
        },
        [BackendCheckingOwnership]: {
          invoke: {
            id: 'check-ownership',
            src: 'checkProjectOwned',
            onDone: {
              target: BackendIdle,
              actions: [
                send((_, event: DoneInvokeEvent<boolean>) =>
                  checkOwnershipCompleteEvent(event.data),
                ),
              ],
            },
            onError: {
              target: BackendIdle,
              actions: send((_, event) => backendErrorEvent(event.data)),
            },
          },
        },
        [BackendDownloadingAssets]: {
          invoke: {
            id: 'download-assets',
            src: 'downloadAssets',
            onDone: {
              target: BackendIdle,
              actions: [
                send((_, event: DoneInvokeEvent<ProjectWithFileChanges>) =>
                  downloadAssetsCompleteEvent(event.data),
                ),
              ],
            },
            onError: {
              target: BackendIdle,
              actions: send((_, event) => backendErrorEvent(event.data)),
            },
          },
        },
        [BackendServerSaving]: {
          invoke: {
            id: 'server-save-project',
            src: 'saveProjectToServer',
            onDone: {
              target: BackendIdle,
              actions: send((_, event: DoneInvokeEvent<ProjectWithFileChanges>) =>
                saveCompleteEvent(event.data),
              ),
            },
            onError: {
              target: BackendIdle,
              actions: send((_, event) => backendErrorEvent(event.data)),
            },
          },
        },
        [BackendLocalSaving]: {
          invoke: {
            id: 'local-save-project',
            src: 'saveProjectLocally',
            onDone: {
              target: BackendIdle,
              actions: send((_, event: DoneInvokeEvent<ProjectWithFileChanges>) =>
                saveCompleteEvent(event.data),
              ),
            },
            onError: {
              target: BackendIdle,
              actions: send((_, event) => backendErrorEvent(event.data)),
            },
          },
        },
        [BackendLoading]: {
          invoke: {
            id: 'load-project',
            src: 'loadProject',
            onDone: {
              target: BackendIdle,
              actions: [
                send((_, event: DoneInvokeEvent<ProjectLoadResult>) => {
                  if (event.data.type === 'PROJECT_LOAD_SUCCESS') {
                    return loadCompleteEvent(event.data.projectId, event.data.projectModel)
                  } else {
                    return loadFailedEvent()
                  }
                }),
              ],
            },
            onError: {
              target: BackendIdle,
              actions: send((_, event) => backendErrorEvent(event.data)),
            },
          },
        },
      },
    },

    // User Log In State
    user: {
      id: 'user',
      initial: LoggedOut,
      states: {
        [LoggedOut]: {
          on: {
            USER_LOG_IN: {
              target: LoggedIn,
              actions: assign({ loggedIn: (_context, _event) => true }),
            },
          },
        },
        [LoggedIn]: {
          on: {
            USER_LOG_OUT: {
              target: LoggedOut,
              actions: assign({ loggedIn: (_context, _event) => false }),
            },
          },
        },
      },
    },

    // Core Machine
    core: {
      id: 'persistence-core',
      initial: Empty,
      states: {
        // Idle states
        [Empty]: {
          on: {
            NEW: CreatingNew,
            LOAD: Loading,
          },
        },
        [Ready]: {
          entry: checkQueue,
          exit: queueClear,
          on: {
            NEW: CreatingNew,
            LOAD: Loading,
            SAVE: [
              {
                cond: (context, _) => context.projectOwned,
                target: Saving,
              },
              { target: Forking },
            ],
            FORK: Forking,
            USER_LOG_IN: [
              {
                cond: (context, _) => context.projectOwned,
                actions: send((context, _) => saveEvent(context.project!)),
              },
            ],
          },
        },

        // Intermediate states
        [CreatingNew]: {
          initial: CreatingProjectId,
          states: {
            [CreatingProjectId]: {
              entry: [
                assign((_, event) => {
                  return {
                    projectId: undefined,
                    project: undefined,
                    queuedSave: undefined,
                    projectOwned: true,
                  }
                }),
                send(backendCreateProjectIdEvent()),
              ],
              on: {
                PROJECT_ID_CREATED: {
                  target: ProjectIdCreated,
                  actions: assign({
                    projectId: (_, event) => event.projectId,
                  }),
                },
              },
            },
            [ProjectIdCreated]: {
              invoke: {
                id: 'create-new-project-model',
                src: createNewProjectModel,
                onDone: {
                  actions: send((context, event: DoneInvokeEvent<ProjectModel>) =>
                    newProjectCreatedEvent(context.projectId!, event.data),
                  ),
                },
              },
              on: {
                NEW_PROJECT_CREATED: {
                  target: ProjectCreated,
                  actions: assign((_, event) => {
                    return {
                      project: (event as NewProjectCreatedEvent).projectModel,
                    }
                  }),
                },
              },
            },
            [ProjectCreated]: { type: 'final' },
          },
          onDone: {
            actions: send((context, _) => innerSave(context.project!)),
          },
          on: {
            INNER_SAVE: Saving,
            SAVE: {
              actions: queuePush,
            },
            BACKEND_ERROR: Ready,
          },
        },
        [Loading]: {
          initial: LoadingProject,
          states: {
            [LoadingProject]: {
              entry: choose([
                {
                  cond: (_, event) => event.type === 'LOAD',
                  actions: send((_, event) => backendLoadEvent((event as LoadEvent).projectId)),
                },
              ]),
              on: {
                LOAD_COMPLETE: {
                  target: CheckingOwnership,
                  actions: assign((_, event) => {
                    return {
                      projectId: event.projectId,
                      project: event.projectModel,
                      queuedSave: undefined,
                    }
                  }),
                },
              },
            },
            [CheckingOwnership]: {
              entry: send((context, _) => backendCheckOwnershipEvent(context.projectId!)),
              on: {
                CHECK_OWNERSHIP_COMPLETE: {
                  target: ProjectLoaded,
                  actions: assign({
                    projectOwned: (_, event) => event.isOwner,
                  }),
                },
              },
            },
            [ProjectLoaded]: { type: 'final' },
          },
          onDone: Ready,
          on: {
            BACKEND_ERROR: Ready,
            LOAD_FAILED: {
              target: Empty,
              actions: assign((_context, _event) => {
                return {
                  projectId: undefined,
                  project: undefined,
                  queuedSave: undefined,
                  projectOwned: false,
                }
              }),
            },
          },
        },
        [Saving]: {
          entry: choose([
            {
              cond: (context, _) => context.loggedIn,
              actions: send((context, event) =>
                backendServerSaveEvent(context.projectId!, (event as SaveEvent).projectModel),
              ),
            },
            {
              actions: send((context, event) =>
                backendLocalSaveEvent(context.projectId!, (event as SaveEvent).projectModel),
              ),
            },
          ]),
          on: {
            SAVE: {
              actions: queuePush,
            },
            SAVE_COMPLETE: {
              target: Ready,
              actions: assign((_context, event) => {
                return {
                  projectOwned: true,
                  project: event.saveResult.projectModel,
                }
              }),
            },
            BACKEND_ERROR: Ready,
          },
        },
        [Forking]: {
          initial: DownloadingAssets,
          states: {
            [DownloadingAssets]: {
              entry: choose([
                {
                  cond: (context, _) => context.project != null && context.projectId != null,
                  actions: send((context, _) =>
                    backendDownloadAssetsEvent(context.projectId!, context.project!),
                  ),
                },
              ]),
              on: {
                DOWNLOAD_ASSETS_COMPLETE: {
                  target: CreatingProjectId,
                  actions: assign({
                    project: (_, event) => event.downloadAssetsResult.projectModel,
                  }),
                },
              },
            },
            [CreatingProjectId]: {
              entry: send(backendCreateProjectIdEvent()),
              on: {
                PROJECT_ID_CREATED: {
                  target: ProjectForked,
                  actions: assign({
                    projectId: (_, event) => event.projectId,
                  }),
                },
              },
            },
            [ProjectForked]: { type: 'final' },
          },
          onDone: {
            actions: [send((context, _) => innerSave(context.project!))],
          },
          on: {
            INNER_SAVE: Saving,
            SAVE: {
              actions: queuePush,
            },
            BACKEND_ERROR: Ready,
          },
        },
      },
    },
  },
})

let interpreter: Interpreter<PersistenceContext, any, PersistenceEvent> | null = null

let SaveThrottle = 30000
let lastSavedTS = 0
let throttledSaveTimeoutId: NodeJS.Timer | null = null
let waitingThrottledSaveEvent: SaveEvent | null

function clearThrottledSave(): void {
  if (throttledSaveTimeoutId != null) {
    clearTimeout(throttledSaveTimeoutId)
    throttledSaveTimeoutId = null
  }

  waitingThrottledSaveEvent = null
}

function sendThrottledSave(): void {
  if (waitingThrottledSaveEvent != null) {
    interpreter?.send(waitingThrottledSaveEvent)
  }
  clearThrottledSave()
}

export function initialisePersistence(
  dispatch: EditorDispatch,
  onProjectNotFound: () => void,
  onCreatedOrLoadedProject: (
    projectId: string,
    projectName: string,
    project: PersistentModel,
  ) => void,
) {
  clearThrottledSave()
  lastSavedTS = 0
  let queuedActions: Array<EditorAction> = [] // Queue up actions during events and transitions, then dispatch when ready
  if (interpreter != null) {
    interpreter.stop()
  }

  interpreter = interpret(persistenceMachine)

  interpreter.onTransition((state, event) => {
    if (state.changed) {
      switch (event.type) {
        case 'NEW_PROJECT_CREATED':
          onCreatedOrLoadedProject(
            event.projectId,
            event.projectModel.name,
            event.projectModel.content,
          )
          break
        case 'LOAD_COMPLETE':
          onCreatedOrLoadedProject(
            event.projectId,
            event.projectModel.name,
            event.projectModel.content,
          )
          break
        case 'PROJECT_ID_CREATED':
          queuedActions.push(setProjectID(event.projectId))
          break
        case 'LOAD_FAILED':
          onProjectNotFound()
          break
        case 'DOWNLOAD_ASSETS_COMPLETE': {
          if (state.matches({ core: { [Forking]: CreatingProjectId } })) {
            queuedActions.push(setForkedFromProjectID(state.context.projectId!))
            queuedActions.push(setProjectName(`${state.context.project!.name} (forked)`))
            queuedActions.push(showToast(notice('Project successfully forked!')))
          }

          const updateFileActions = event.downloadAssetsResult.filesWithFileNames.map(
            ({ fileName, file }) => updateFile(fileName, file, true),
          )
          queuedActions.push(...updateFileActions)
          break
        }
        case 'SAVE_COMPLETE':
          const updateFileActions = event.saveResult.filesWithFileNames.map(({ fileName, file }) =>
            updateFile(fileName, file, true),
          )
          queuedActions.push(...updateFileActions)
          lastSavedTS = Date.now()
          // TODO Show toasts after:
          // [ ] first local save
          // [ ] first server save
          break
      }

      if (state.matches({ core: Ready })) {
        if (queuedActions.length > 0) {
          const actionsToDispatch = queuedActions
          const projectIdOrNameChanged = actionsToDispatch.some(
            (action) => action.action === 'SET_PROJECT_ID' || action.action === 'SET_PROJECT_NAME',
          )
          if (projectIdOrNameChanged) {
            pushProjectURLToBrowserHistory(state.context.projectId!, state.context.project!.name)
          }
          queuedActions = []
          dispatch(actionsToDispatch)
        }
      }
    }
  })

  interpreter.start()
}

function getRemainingSaveDelay(): number {
  return Math.max(0, lastSavedTS + SaveThrottle - Date.now())
}

function shouldThrottle(forced: boolean): boolean {
  return !forced && getRemainingSaveDelay() > 0
}

export function pushProjectURLToBrowserHistory(projectId: string, projectName: string): void {
  // Make sure we don't replace the query params
  const queryParams = window.top.location.search
  const projectURL = projectURLForProject(projectId, projectName)
  const title = `Utopia ${projectName}`
  window.top.history.pushState({}, title, `${projectURL}${queryParams}`)
}

// API

export function save(projectName: string, project: PersistentModel, forced: boolean = false): void {
  const eventToFire = saveEvent({ name: projectName, content: project })

  if (shouldThrottle(forced)) {
    waitingThrottledSaveEvent = eventToFire
    throttledSaveTimeoutId = setTimeout(sendThrottledSave, getRemainingSaveDelay())
  } else {
    interpreter?.send(eventToFire)
  }
}

window.addEventListener('beforeunload', (e) => {
  if (!isSafeToClose()) {
    sendThrottledSave()
    e.preventDefault()
    e.returnValue = ''
  }
})

function isSafeToClose(): boolean {
  return waitingThrottledSaveEvent == null
}

export function load(projectId: string): void {
  interpreter?.send(loadEvent(projectId))
}

export function createNew(): void {
  interpreter?.send(newEvent())
}

export function fork(): void {
  interpreter?.send(forkEvent())
}

export function login(): void {
  interpreter?.send(userLogInEvent())
}

export function logout(): void {
  interpreter?.send(userLogOutEvent())
}

// For testing purposes only
export function setSaveThrottle(delay: number): void {
  SaveThrottle = delay
}

export class PersistenceMachine {
  private interpreter: Interpreter<PersistenceContext, any, PersistenceEvent>
  private lastSavedTS: number = 0
  private throttledSaveTimeoutId: NodeJS.Timer | null = null
  private waitingThrottledSaveEvent: SaveEvent | null = null
  private queuedActions: Array<EditorAction> = [] // Queue up actions during events and transitions, then dispatch when ready

  constructor(
    backendAPI: PersistenceBackendAPI,
    dispatch: EditorDispatch,
    onProjectNotFound: () => void,
    onCreatedOrLoadedProject: (
      projectId: string,
      projectName: string,
      project: PersistentModel,
    ) => void,
    private saveThrottle: number = 30000,
  ) {
    this.interpreter = interpret(
      persistenceMachine.withConfig({
        services: {
          getNewProjectId: backendAPI.getNewProjectId,
          checkProjectOwned: (_, event) => {
            if (event.type === 'BACKEND_CHECK_OWNERSHIP') {
              return backendAPI.checkProjectOwned(event.projectId)
            } else {
              throw new Error(
                `Incorrect event type triggered check ownership, ${JSON.stringify(event)}`,
              )
            }
          },
          downloadAssets: (_, event) => {
            if (event.type === 'BACKEND_DOWNLOAD_ASSETS') {
              return backendAPI.downloadAssets(event.projectId, event.projectModel)
            } else {
              throw new Error(
                `Incorrect event type triggered asset download, ${JSON.stringify(event)}`,
              )
            }
          },
          saveProjectToServer: (context, event) => {
            if (
              event.type === 'BACKEND_SERVER_SAVE' &&
              event.projectModel != null &&
              context.projectId != null
            ) {
              return backendAPI.saveProjectToServer(context.projectId, event.projectModel)
            } else {
              throw new Error(
                `Unable to save project with ID ${context.projectId} after event ${JSON.stringify(
                  event,
                )}`,
              )
            }
          },
          saveProjectLocally: (context, event) => {
            if (
              event.type === 'BACKEND_LOCAL_SAVE' &&
              event.projectModel != null &&
              context.projectId != null
            ) {
              return backendAPI.saveProjectLocally(context.projectId, event.projectModel)
            } else {
              throw new Error(
                `Unable to save project with ID ${context.projectId} after event ${JSON.stringify(
                  event,
                )}`,
              )
            }
          },
          loadProject: (_, event) => {
            if (event.type === 'BACKEND_LOAD') {
              return backendAPI.loadProject(event.projectId)
            } else {
              throw new Error(`Invalid event type triggered project load ${JSON.stringify(event)}`)
            }
          },
        },
      }),
    )

    this.interpreter.onTransition((state, event) => {
      if (state.changed) {
        switch (event.type) {
          case 'NEW_PROJECT_CREATED':
            onCreatedOrLoadedProject(
              event.projectId,
              event.projectModel.name,
              event.projectModel.content,
            )
            break
          case 'LOAD_COMPLETE':
            onCreatedOrLoadedProject(
              event.projectId,
              event.projectModel.name,
              event.projectModel.content,
            )
            break
          case 'PROJECT_ID_CREATED':
            this.queuedActions.push(setProjectID(event.projectId))
            break
          case 'LOAD_FAILED':
            onProjectNotFound()
            break
          case 'DOWNLOAD_ASSETS_COMPLETE': {
            if (state.matches({ core: { [Forking]: CreatingProjectId } })) {
              this.queuedActions.push(setForkedFromProjectID(state.context.projectId!))
              this.queuedActions.push(setProjectName(`${state.context.project!.name} (forked)`))
              this.queuedActions.push(showToast(notice('Project successfully forked!')))
            }

            const updateFileActions = event.downloadAssetsResult.filesWithFileNames.map(
              ({ fileName, file }) => updateFile(fileName, file, true),
            )
            this.queuedActions.push(...updateFileActions)
            break
          }
          case 'SAVE_COMPLETE':
            const updateFileActions = event.saveResult.filesWithFileNames.map(
              ({ fileName, file }) => updateFile(fileName, file, true),
            )
            this.queuedActions.push(...updateFileActions)
            this.lastSavedTS = Date.now()
            // TODO Show toasts after:
            // [ ] first local save
            // [ ] first server save
            break
        }

        if (state.matches({ core: Ready })) {
          if (this.queuedActions.length > 0) {
            const actionsToDispatch = this.queuedActions
            const projectIdOrNameChanged = actionsToDispatch.some(
              (action) =>
                action.action === 'SET_PROJECT_ID' || action.action === 'SET_PROJECT_NAME',
            )
            if (projectIdOrNameChanged) {
              pushProjectURLToBrowserHistory(state.context.projectId!, state.context.project!.name)
            }
            this.queuedActions = []
            dispatch(actionsToDispatch)
          }
        }
      }
    })

    this.interpreter.start()

    window.addEventListener('beforeunload', (e) => {
      // FIXME bind this?
      if (!this.isSafeToClose()) {
        this.sendThrottledSave()
        e.preventDefault()
        e.returnValue = ''
      }
    })
  }

  private isSafeToClose(): boolean {
    return this.waitingThrottledSaveEvent == null
  }

  private clearThrottledSave(): void {
    if (this.throttledSaveTimeoutId != null) {
      clearTimeout(this.throttledSaveTimeoutId)
      this.throttledSaveTimeoutId = null
    }

    this.waitingThrottledSaveEvent = null
  }

  private sendThrottledSave(): void {
    if (this.waitingThrottledSaveEvent != null) {
      this.interpreter.send(this.waitingThrottledSaveEvent)
    }
    this.clearThrottledSave()
  }

  private getRemainingSaveDelay(): number {
    return Math.max(0, this.lastSavedTS + this.saveThrottle - Date.now())
  }

  private shouldThrottle(forced: boolean): boolean {
    return !forced && this.getRemainingSaveDelay() > 0
  }

  // API

  save(projectName: string, project: PersistentModel, forced: boolean = false): void {
    const eventToFire = saveEvent({ name: projectName, content: project })

    if (this.shouldThrottle(forced)) {
      this.waitingThrottledSaveEvent = eventToFire
      this.throttledSaveTimeoutId = setTimeout(sendThrottledSave, getRemainingSaveDelay())
    } else {
      this.interpreter.send(eventToFire)
    }
  }

  load(projectId: string): void {
    this.interpreter.send(loadEvent(projectId))
  }

  createNew(): void {
    this.interpreter.send(newEvent())
  }

  fork(): void {
    this.interpreter.send(forkEvent())
  }

  login(): void {
    this.interpreter.send(userLogInEvent())
  }

  logout(): void {
    this.interpreter.send(userLogOutEvent())
  }

  stop(): void {
    // FIXME Destructor?
  }
}

// createNewProjectFromImportedProject
// Error handling for:
// [ ] Failed Save
// [ ] Failed Fork
// [ ] Failed (non-404) Load
// [ ] Failed New
