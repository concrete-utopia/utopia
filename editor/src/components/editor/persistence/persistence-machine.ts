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
  updateSavedProject,
} from '../server'
const { choose } = actions
import localforage from 'localforage'
import {
  fetchLocalProject as loadLocalProject,
  localProjectKey,
  deleteProject as deleteLocalProject,
} from '../../../common/persistence'
import { arrayContains } from '../../../core/shared/utils'
import { checkProjectOwnership } from '../../../common/server'
import { PersistentModel } from '../store/editor-state'
import {
  addFileToProjectContents,
  AssetFileWithFileName,
  getAllProjectAssetFiles,
} from '../../assets'
import { getFileExtension } from '../../../core/shared/file-utils'
import { AssetFile, ImageFile } from '../../../core/shared/project-file-types'
import { assetFile, imageFile, isImageFile } from '../../../core/model/project-file-utils'
import { EditorDispatch } from '../action-types'

// Backend Calls

export interface LocalProject {
  model: PersistentModel
  createdAt: string
  lastModified: string
  thumbnail: string
  name: string
}

export async function projectIsStoredLocally(projectId: string): Promise<boolean> {
  const keys = await localforage.keys().catch(() => [])
  const targetKey = localProjectKey(projectId)
  return arrayContains(keys, targetKey)
}

async function getNewProjectId(): Promise<string> {
  return createNewProjectID()
}

async function checkProjectOwned(projectId: string): Promise<boolean> {
  const existsLocally = await projectIsStoredLocally(projectId)
  if (existsLocally) {
    return true
  } else {
    const ownerState = await checkProjectOwnership(projectId)
    return ownerState === 'unowned' || ownerState.isOwner
  }
}

interface ProjectModelWithId {
  projectId: string
  projectModel: ProjectModel
}

interface ProjectLoadSuccess extends ProjectModelWithId {
  type: 'PROJECT_LOAD_SUCCESS'
}

interface ProjectNotFount {
  type: 'PROJECT_NOT_FOUND'
}

type ProjectLoadResult = ProjectLoadSuccess | ProjectNotFount

async function loadProject(projectId: string): Promise<ProjectLoadResult> {
  const serverProject = await loadServerProject(projectId)

  switch (serverProject.type) {
    case 'ProjectLoaded':
      return {
        type: 'PROJECT_LOAD_SUCCESS',
        projectId: projectId,
        projectModel: {
          content: serverProject.content,
          name: serverProject.title,
        },
      }
    case 'ProjectNotFound':
      const localProject = (await loadLocalProject(projectId)) as LocalProject | null
      if (localProject == null) {
        return {
          type: 'PROJECT_NOT_FOUND',
        }
      } else {
        return {
          type: 'PROJECT_LOAD_SUCCESS',
          projectId: projectId,
          projectModel: {
            content: localProject.model,
            name: localProject.name,
          },
        }
      }
    default:
      throw new Error(`Invalid project load response: ${serverProject}`)
  }
}

async function saveProjectToServer(
  projectId: string,
  projectModel: ProjectModel,
): Promise<ProjectModelWithId> {
  await updateSavedProject(projectId, projectModel.content, projectModel.name)

  return {
    projectId: projectId,
    projectModel: projectModel,
  }
}

async function saveProjectLocally(
  projectId: string,
  projectModel: ProjectModel,
): Promise<ProjectModelWithId> {
  const existing = await localforage.getItem<LocalProject | null>(localProjectKey(projectId))
  const existingThumbnail = existing == null ? '' : existing.thumbnail
  const now = new Date().toISOString()
  const createdAt = existing == null ? now : existing.createdAt
  const modifiedAt = now

  const localProject: LocalProject = {
    model: projectModel.content,
    createdAt: createdAt,
    lastModified: modifiedAt,
    thumbnail: existingThumbnail,
    name: projectModel.name,
  }

  await localforage.setItem(localProjectKey(projectId), localProject)

  return {
    projectId: projectId,
    projectModel: projectModel,
  }
}

interface DownloadedAssetsResult {
  assetsWithFileNames: Array<AssetFileWithFileName>
  projectModel: ProjectModel
}

async function downloadAssets(
  projectId: string,
  projectModel: ProjectModel,
): Promise<DownloadedAssetsResult> {
  const allProjectAssets = getAllProjectAssetFiles(projectModel.content.projectContents)
  const allProjectAssetsDownloaded = await downloadAssetsFromProject(projectId, allProjectAssets)
  const updatedProjectContents = allProjectAssetsDownloaded.reduce(
    (workingProjectContents, { fileName: assetPath, file: asset }) => {
      return addFileToProjectContents(workingProjectContents, assetPath, asset)
    },
    projectModel.content.projectContents,
  )
  const updatedProjectModel: ProjectModel = {
    name: projectModel.name,
    content: {
      ...projectModel.content,
      projectContents: updatedProjectContents,
    },
  }

  return {
    assetsWithFileNames: allProjectAssetsDownloaded,
    projectModel: updatedProjectModel,
  }
}

function scrubBase64FromFile(file: ImageFile | AssetFile): ImageFile | AssetFile {
  if (isImageFile(file)) {
    return imageFile(undefined, undefined, file.width, file.height, file.hash)
  } else {
    return assetFile(undefined)
  }
}

async function uploadAssets(projectId: string, projectModel: ProjectModel): Promise<ProjectModel> {
  const allProjectAssets = getAllProjectAssetFiles(projectModel.content.projectContents)
  let assetsToUpload: Array<AssetToSave> = []

  const updatedProjectContents = allProjectAssets.reduce(
    (workingProjectContents, { fileName: assetPath, file: asset }) => {
      const fileType = getFileExtension(assetPath)
      if (asset.base64 != null) {
        assetsToUpload.push(assetToSave(fileType, asset.base64, assetPath))
      }
      const updatedFile = scrubBase64FromFile(asset)
      return addFileToProjectContents(workingProjectContents, assetPath, updatedFile)
    },
    projectModel.content.projectContents,
  )

  await saveAssets(projectId, assetsToUpload)

  const updatedProjectModel: ProjectModel = {
    name: projectModel.name,
    content: {
      ...projectModel.content,
      projectContents: updatedProjectContents,
    },
  }

  return updatedProjectModel
}

// End Backend Calls

interface NewEvent {
  type: 'NEW'
  projectModel: ProjectModel
}

function newEvent(projectModel: ProjectModel): NewEvent {
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
}

function saveCompleteEvent(): SaveCompleteEvent {
  return {
    type: 'SAVE_COMPLETE',
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
  downloadedAssetsResult: DownloadedAssetsResult
}

function downloadAssetsCompleteEvent(
  downloadedAssetsResult: DownloadedAssetsResult,
): DownloadAssetsCompleteEvent {
  return {
    type: 'DOWNLOAD_ASSETS_COMPLETE',
    downloadedAssetsResult: downloadedAssetsResult,
  }
}

interface UploadAssetsEvent {
  type: 'UPLOAD_ASSETS'
  projectModel: ProjectModel
}

function uploadAssetsEvent(projectModel: ProjectModel): UploadAssetsEvent {
  return {
    type: 'UPLOAD_ASSETS',
    projectModel: projectModel,
  }
}

interface SkipUploadAssetsEvent {
  type: 'SKIP_UPLOAD_ASSETS'
  projectModel: ProjectModel
}

function skipUploadAssetsEvent(projectModel: ProjectModel): SkipUploadAssetsEvent {
  return {
    type: 'SKIP_UPLOAD_ASSETS',
    projectModel: projectModel,
  }
}

interface UploadAssetsCompleteEvent {
  type: 'UPLOAD_ASSETS_COMPLETE'
  projectModel: ProjectModel
}

function uploadAssetsCompleteEvent(projectModel: ProjectModel): UploadAssetsCompleteEvent {
  return {
    type: 'UPLOAD_ASSETS_COMPLETE',
    projectModel: projectModel,
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
  | LoadEvent
  | LoadCompleteEvent
  | LoadFailedEvent
  | CheckOwnershipCompleteEvent
  | SaveEvent
  | SaveCompleteEvent
  | ForkEvent
  | DownloadAssetsCompleteEvent
  | UploadAssetsEvent
  | SkipUploadAssetsEvent
  | UploadAssetsCompleteEvent
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

interface BackendUploadAssetsEvent {
  type: 'BACKEND_UPLOAD_ASSETS'
  projectId: string
  projectModel: ProjectModel
}

function backendUploadAssetsEvent(
  projectId: string,
  projectModel: ProjectModel,
): BackendUploadAssetsEvent {
  return {
    type: 'BACKEND_UPLOAD_ASSETS',
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
  | BackendUploadAssetsEvent
  | BackendServerSaveEvent
  | BackendLocalSaveEvent
  | BackendLoadEvent
  | BackendCheckOwnershipEvent
  | BackendErrorEvent

interface UserLogInEvent {
  type: 'USER_LOG_IN'
}

interface UserLogOutEvent {
  type: 'USER_LOG_OUT'
}

type UserEvent = UserLogInEvent | UserLogOutEvent

type PersistenceEvent = CoreEvent | BackendEvent | UserEvent

interface ProjectModel {
  content: PersistentModel
  name: string
}

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

// InternalLoadingStates
const LoadingProject = 'loading-project'
const CheckingOwnership = 'checking-ownership'
const ProjectLoaded = 'project-loaded'

// InternalSavingStates
const CheckLoggedIn = 'check-logged-in'
const UploadingAssets = 'uploading-assets'
const SavingProjectToServer = 'saving-project-to-server'
const SavingProjectLocally = 'saving-project-locally'
const ProjectSaved = 'project-saved'

// InternalForkingStates
const DownloadingAssets = 'downloading-assets'
const ProjectForked = 'project-forked'

// Backend States
const BackendIdle = 'idle'
const BackendCreatingProjectId = 'creating-project-id'
const BackendCheckingOwnership = 'checking-ownership'
const BackendDownloadingAssets = 'downloading-assets'
const BackendUploadingAssets = 'uploading-assets'
const BackendServerSaving = 'server-saving'
const BackendDeletingLocal = 'deleting-local'
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

const persistenceMachine = createMachine<
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
            BACKEND_UPLOAD_ASSETS: BackendUploadingAssets,
            BACKEND_SERVER_SAVE: BackendServerSaving,
            BACKEND_LOCAL_SAVE: BackendLocalSaving,
            BACKEND_LOAD: BackendLoading,
          },
        },
        [BackendCreatingProjectId]: {
          invoke: {
            id: 'create-project-id',
            src: getNewProjectId,
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
            src: (_, event) => {
              if (event.type === 'BACKEND_CHECK_OWNERSHIP') {
                return checkProjectOwned(event.projectId)
              } else {
                throw new Error(
                  `Incorrect event type triggered check ownership, ${JSON.stringify(event)}`,
                )
              }
            },
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
            src: (_, event) => {
              if (event.type === 'BACKEND_DOWNLOAD_ASSETS') {
                return downloadAssets(event.projectId, event.projectModel)
              } else {
                throw new Error(
                  `Incorrect event type triggered asset download, ${JSON.stringify(event)}`,
                )
              }
            },
            onDone: {
              target: BackendIdle,
              actions: [
                send((_, event: DoneInvokeEvent<DownloadedAssetsResult>) =>
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
        [BackendUploadingAssets]: {
          invoke: {
            id: 'upload-assets',
            src: (_, event) => {
              if (event.type === 'BACKEND_UPLOAD_ASSETS') {
                return uploadAssets(event.projectId, event.projectModel)
              } else {
                throw new Error(
                  `Incorrect event type triggered asset upload, ${JSON.stringify(event)}`,
                )
              }
            },
            onDone: {
              target: BackendIdle,
              actions: [
                send((_, event: DoneInvokeEvent<ProjectModel>) =>
                  uploadAssetsCompleteEvent(event.data),
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
            src: (context, event) => {
              if (
                event.type === 'BACKEND_SERVER_SAVE' &&
                event.projectModel != null &&
                context.projectId != null
              ) {
                return saveProjectToServer(context.projectId, event.projectModel)
              } else {
                throw new Error(
                  `Unable to save project with ID ${context.projectId} after event ${JSON.stringify(
                    event,
                  )}`,
                )
              }
            },
            onDone: {
              target: BackendDeletingLocal,
              actions: [
                assign((_, event: DoneInvokeEvent<ProjectModelWithId>) => {
                  return {
                    project: event.data.projectModel,
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
        [BackendDeletingLocal]: {
          invoke: {
            id: 'delete-local-save',
            src: (context, _) => deleteLocalProject(context.projectId!),
            onDone: {
              target: BackendIdle,
              actions: send(saveCompleteEvent()),
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
            src: (context, event) => {
              if (
                event.type === 'BACKEND_LOCAL_SAVE' &&
                event.projectModel != null &&
                context.projectId != null
              ) {
                return saveProjectLocally(context.projectId, event.projectModel)
              } else {
                throw new Error(
                  `Unable to save project with ID ${context.projectId} after event ${JSON.stringify(
                    event,
                  )}`,
                )
              }
            },
            onDone: {
              target: BackendIdle,
              actions: [
                assign((_, event: DoneInvokeEvent<ProjectModelWithId>) => {
                  return {
                    project: event.data.projectModel,
                  }
                }),
                send(saveCompleteEvent()),
              ],
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
            src: (_, event) => {
              if (event.type === 'BACKEND_LOAD') {
                return loadProject(event.projectId)
              } else {
                throw new Error(
                  `Invalid event type triggered project load ${JSON.stringify(event)}`,
                )
              }
            },
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
                    project: (event as NewEvent).projectModel,
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
            [ProjectIdCreated]: { type: 'final' },
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
          },
        },
        [Saving]: {
          initial: CheckLoggedIn,
          states: {
            [CheckLoggedIn]: {
              entry: choose([
                {
                  cond: (context, _) => context.loggedIn,
                  actions: send((_, event) => uploadAssetsEvent((event as SaveEvent).projectModel)),
                },
                {
                  actions: send((_, event) =>
                    skipUploadAssetsEvent((event as SaveEvent).projectModel),
                  ),
                },
              ]),
              on: {
                UPLOAD_ASSETS: UploadingAssets,
                SKIP_UPLOAD_ASSETS: SavingProjectLocally,
              },
            },
            [UploadingAssets]: {
              entry: send((context, event) =>
                backendUploadAssetsEvent(
                  context.projectId!,
                  (event as UploadAssetsEvent).projectModel,
                ),
              ),
              on: {
                UPLOAD_ASSETS_COMPLETE: SavingProjectToServer,
              },
            },
            [SavingProjectLocally]: {
              entry: send((context, event) =>
                backendLocalSaveEvent(
                  context.projectId!,
                  (event as SkipUploadAssetsEvent).projectModel,
                ),
              ),
              on: {
                SAVE_COMPLETE: ProjectSaved,
              },
            },
            [SavingProjectToServer]: {
              entry: send((context, event) =>
                backendServerSaveEvent(
                  context.projectId!,
                  (event as UploadAssetsCompleteEvent).projectModel,
                ),
              ),
              on: {
                SAVE_COMPLETE: ProjectSaved,
              },
            },
            [ProjectSaved]: {
              type: 'final',
            },
          },
          on: {
            SAVE: {
              actions: queuePush,
            },
            BACKEND_ERROR: Ready,
          },
          onDone: {
            target: Ready,
            actions: assign({ projectOwned: (_context, _event) => true }),
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
                    project: (_, event) => event.downloadedAssetsResult.projectModel,
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

export function initialisePersistence(dispatch: EditorDispatch) {
  if (interpreter != null) {
    interpreter.stop()
  }

  interpreter = interpret(persistenceMachine)

  interpreter.onTransition((state, event) => {
    switch (state.value) {
      case ProjectForked:
        // TODO Update forked project name and forkedFromProjectId
        break
      default:
        switch (event.type) {
          case 'LOAD_FAILED':
            // TODO Handle 404s
            break
          case 'UPLOAD_ASSETS_COMPLETE':
            // TODO Clean model after asset upload
            break
          case 'SAVE_COMPLETE':
            // TODO Show toasts after:
            // [ ] first local save
            // [ ] first server save
            // [ ] fork
            break
        }
    }
  })

  interpreter.start()
}

// TODO Claim ownership of project before uploading assets
// TODO Thumbnails
// TODO Throttling
