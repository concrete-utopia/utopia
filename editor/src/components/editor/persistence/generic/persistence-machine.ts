import type { DoneInvokeEvent } from 'xstate'
import { actions, assign, createMachine, send } from 'xstate'
import type { Model } from 'xstate/lib/model.types'
import type {
  PersistenceBackendAPI,
  PersistenceContext,
  ProjectLoadResult,
  ProjectModel,
  ProjectWithFileChanges,
} from './persistence-types'
const { choose } = actions

// Keep this file as simple as possible so that it can be used in https://stately.ai/viz
// To use this in the visualiser, copy everything from the generic files into one, and call createPersistenceMachine(VisualiserBackend)

interface NewEvent<ModelType> {
  type: 'NEW'
  projectModel: ProjectModel<ModelType>
}

export function newEvent<ModelType>(projectModel: ProjectModel<ModelType>): NewEvent<ModelType> {
  return {
    type: 'NEW',
    projectModel: projectModel,
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

interface NewProjectCreatedEvent<ModelType> {
  type: 'NEW_PROJECT_CREATED'
  projectId: string
  projectModel: ProjectModel<ModelType>
}

function newProjectCreatedEvent<ModelType>(
  projectId: string,
  projectModel: ProjectModel<ModelType>,
): NewProjectCreatedEvent<ModelType> {
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

export function loadEvent(projectId: string): LoadEvent {
  return {
    type: 'LOAD',
    projectId: projectId,
  }
}

interface LoadCompleteEvent<ModelType> {
  type: 'LOAD_COMPLETE'
  projectId: string
  projectModel: ProjectModel<ModelType>
}

function loadCompleteEvent<ModelType>(
  projectId: string,
  projectModel: ProjectModel<ModelType>,
): LoadCompleteEvent<ModelType> {
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

export interface SaveEvent<ModelType> {
  type: 'SAVE'
  projectModel: ProjectModel<ModelType>
}

export function saveEvent<ModelType>(projectModel: ProjectModel<ModelType>): SaveEvent<ModelType> {
  return {
    type: 'SAVE',
    projectModel: projectModel,
  }
}

interface SaveCompleteEvent<ModelType, FileType> {
  type: 'SAVE_COMPLETE'
  saveResult: ProjectWithFileChanges<ModelType, FileType>
}

function saveCompleteEvent<ModelType, FileType>(
  saveResult: ProjectWithFileChanges<ModelType, FileType>,
): SaveCompleteEvent<ModelType, FileType> {
  return {
    type: 'SAVE_COMPLETE',
    saveResult: saveResult,
  }
}

interface InnerSaveEvent<ModelType> {
  type: 'INNER_SAVE'
  projectModel: ProjectModel<ModelType>
}

function innerSave<ModelType>(projectModel: ProjectModel<ModelType>): InnerSaveEvent<ModelType> {
  return {
    type: 'INNER_SAVE',
    projectModel: projectModel,
  }
}

interface ForkEvent<ModelType> {
  type: 'FORK'
  projectModel: ProjectModel<ModelType>
}

export function forkEvent<ModelType>(projectModel: ProjectModel<ModelType>): ForkEvent<ModelType> {
  return {
    type: 'FORK',
    projectModel: projectModel,
  }
}

interface DownloadAssetsCompleteEvent<ModelType, FileType> {
  type: 'DOWNLOAD_ASSETS_COMPLETE'
  downloadAssetsResult: ProjectWithFileChanges<ModelType, FileType>
}

function downloadAssetsCompleteEvent<ModelType, FileType>(
  downloadAssetsResult: ProjectWithFileChanges<ModelType, FileType>,
): DownloadAssetsCompleteEvent<ModelType, FileType> {
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

type CoreEvent<ModelType, FileType> =
  | NewEvent<ModelType>
  | ProjectIdCreatedEvent
  | NewProjectCreatedEvent<ModelType>
  | LoadEvent
  | LoadCompleteEvent<ModelType>
  | LoadFailedEvent
  | CheckOwnershipCompleteEvent
  | SaveEvent<ModelType>
  | SaveCompleteEvent<ModelType, FileType>
  | ForkEvent<ModelType>
  | DownloadAssetsCompleteEvent<ModelType, FileType>
  | InnerSaveEvent<ModelType>

interface BackendCreateProjectIdEvent {
  type: 'BACKEND_CREATE_PROJECT_ID'
}

function backendCreateProjectIdEvent(): BackendCreateProjectIdEvent {
  return {
    type: 'BACKEND_CREATE_PROJECT_ID',
  }
}

interface BackendDownloadAssetsEvent<ModelType> {
  type: 'BACKEND_DOWNLOAD_ASSETS'
  projectId: string
  projectModel: ProjectModel<ModelType>
}

function backendDownloadAssetsEvent<ModelType>(
  projectId: string,
  projectModel: ProjectModel<ModelType>,
): BackendDownloadAssetsEvent<ModelType> {
  return {
    type: 'BACKEND_DOWNLOAD_ASSETS',
    projectId: projectId,
    projectModel: projectModel,
  }
}

interface BackendServerSaveEvent<ModelType> {
  type: 'BACKEND_SERVER_SAVE'
  projectId: string
  projectModel: ProjectModel<ModelType>
}

function backendServerSaveEvent<ModelType>(
  projectId: string,
  projectModel: ProjectModel<ModelType>,
): BackendServerSaveEvent<ModelType> {
  return {
    type: 'BACKEND_SERVER_SAVE',
    projectId: projectId,
    projectModel: projectModel,
  }
}

interface BackendLocalSaveEvent<ModelType> {
  type: 'BACKEND_LOCAL_SAVE'
  projectId: string
  projectModel: ProjectModel<ModelType>
}

function backendLocalSaveEvent<ModelType>(
  projectId: string,
  projectModel: ProjectModel<ModelType>,
): BackendLocalSaveEvent<ModelType> {
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
  error: Error | string
}

function backendErrorEvent(error: Error | string): BackendErrorEvent {
  return {
    type: 'BACKEND_ERROR',
    error: error,
  }
}

type BackendEvent<ModelType> =
  | BackendCreateProjectIdEvent
  | BackendDownloadAssetsEvent<ModelType>
  | BackendServerSaveEvent<ModelType>
  | BackendLocalSaveEvent<ModelType>
  | BackendLoadEvent
  | BackendCheckOwnershipEvent
  | BackendErrorEvent

interface UserLogInEvent {
  type: 'USER_LOG_IN'
}

export function userLogInEvent(): UserLogInEvent {
  return {
    type: 'USER_LOG_IN',
  }
}

interface UserLogOutEvent {
  type: 'USER_LOG_OUT'
}

export function userLogOutEvent(): UserLogOutEvent {
  return {
    type: 'USER_LOG_OUT',
  }
}

type UserEvent = UserLogInEvent | UserLogOutEvent

export type PersistenceEvent<ModelType, FileType> =
  | CoreEvent<ModelType, FileType>
  | BackendEvent<ModelType>
  | UserEvent

// Core States
export const Empty = 'empty'
export const Ready = 'ready'
export const CreatingNew = 'creating'
export const Loading = 'loading'
export const Saving = 'saving'
export const Forking = 'forking'

// InternalCreatingNewStates
export const CreatingProjectId = 'creating-project-id'
export const ProjectCreated = 'project-created'

// InternalLoadingStates
export const LoadingProject = 'loading-project'
export const CheckingOwnership = 'checking-ownership'
export const ProjectLoaded = 'project-loaded'

// InternalForkingStates
export const DownloadingAssets = 'downloading-assets'
export const ProjectForked = 'project-forked'

// Backend States
export const BackendIdle = 'idle'
export const BackendCreatingProjectId = 'creating-project-id'
export const BackendCheckingOwnership = 'checking-ownership'
export const BackendDownloadingAssets = 'downloading-assets'
export const BackendServerSaving = 'server-saving'
export const BackendLocalSaving = 'local-saving'
export const BackendLoading = 'loading'

// User States
export const LoggedOut = 'logged-out'
export const LoggedIn = 'logged-in'

export function createPersistenceMachine<ModelType, FileType>(
  backendAPI: PersistenceBackendAPI<ModelType, FileType>,
) {
  const queuePush = assign<PersistenceContext<ModelType>, SaveEvent<ModelType>>(
    (_context, event) => {
      return {
        queuedSave: event.projectModel,
      }
    },
  )
  const checkQueue = choose<PersistenceContext<ModelType>, PersistenceEvent<ModelType, FileType>>([
    {
      cond: (context) => context.queuedSave != null,
      actions: send((context) => saveEvent(context.queuedSave!)),
    },
  ])
  // The queue clearing has to be handled separately as assign actions are batched and handled before other actions
  // until xstate v5: https://xstate.js.org/docs/guides/actions.html#action-order
  const queueClear = assign<PersistenceContext<ModelType>, PersistenceEvent<ModelType, FileType>>({
    queuedSave: undefined,
  })

  const maybeRollback = choose<
    PersistenceContext<ModelType>,
    PersistenceEvent<ModelType, FileType>
  >([
    {
      cond: (_, event) => event.type === 'BACKEND_ERROR',
      actions: assign<PersistenceContext<ModelType>, PersistenceEvent<ModelType, FileType>>({
        projectId: (context) => context.rollbackProjectId,
        project: (context) => context.rollbackProject,
      }),
    },
    {
      actions: assign<PersistenceContext<ModelType>, PersistenceEvent<ModelType, FileType>>({
        rollbackProjectId: (context) => context.projectId,
        rollbackProject: (context) => context.project,
      }),
    },
  ])

  const logError = (
    context: PersistenceContext<ModelType>,
    event: PersistenceEvent<ModelType, FileType>,
  ) => {
    if (event.type === 'BACKEND_ERROR') {
      console.error(event.error)
    }
  }

  return createMachine<
    Model<PersistenceContext<ModelType>, PersistenceEvent<ModelType, FileType>>,
    PersistenceContext<ModelType>,
    PersistenceEvent<ModelType, FileType>
  >(
    {
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
                    send((_, event: DoneInvokeEvent<ProjectWithFileChanges<ModelType, FileType>>) =>
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
                  actions: send(
                    (_, event: DoneInvokeEvent<ProjectWithFileChanges<ModelType, FileType>>) =>
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
                  actions: send(
                    (_, event: DoneInvokeEvent<ProjectWithFileChanges<ModelType, FileType>>) =>
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
                    send((_, event: DoneInvokeEvent<ProjectLoadResult<ModelType>>) => {
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
              entry: [checkQueue, maybeRollback],
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
                        project: (event as NewEvent<ModelType>).projectModel,
                        queuedSave: undefined,
                        projectOwned: true,
                      }
                    }),
                    send(backendCreateProjectIdEvent()),
                  ],
                  on: {
                    PROJECT_ID_CREATED: {
                      actions: [
                        assign({ projectId: (_, event) => event.projectId }),
                        send((context, event) =>
                          newProjectCreatedEvent(event.projectId, context.project!),
                        ),
                      ],
                    },
                    NEW_PROJECT_CREATED: ProjectCreated,
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
                BACKEND_ERROR: {
                  target: Ready,
                  actions: logError,
                },
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
                BACKEND_ERROR: {
                  target: Ready,
                  actions: logError,
                },
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
                    backendServerSaveEvent(
                      context.projectId!,
                      (event as SaveEvent<ModelType>).projectModel,
                    ),
                  ),
                },
                {
                  actions: send((context, event) =>
                    backendLocalSaveEvent(
                      context.projectId!,
                      (event as SaveEvent<ModelType>).projectModel,
                    ),
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

                BACKEND_ERROR: {
                  target: Ready,
                  actions: logError,
                },
              },
            },
            [Forking]: {
              initial: DownloadingAssets,
              states: {
                [DownloadingAssets]: {
                  entry: choose([
                    {
                      cond: (context, _) => context.project != null,
                      actions: send((context, event) =>
                        backendDownloadAssetsEvent(
                          context.projectId!,
                          (event as ForkEvent<ModelType>).projectModel,
                        ),
                      ),
                    },
                  ]),
                  on: {
                    DOWNLOAD_ASSETS_COMPLETE: {
                      target: CreatingProjectId,
                      actions: assign({
                        project: (_, event) => {
                          return {
                            name: `${event.downloadAssetsResult.projectModel.name} (forked)`,
                            content: event.downloadAssetsResult.projectModel.content,
                          }
                        },
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
                BACKEND_ERROR: {
                  target: Ready,
                  actions: logError,
                },
              },
            },
          },
        },
      },
    },
    {
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
    },
  )
}
