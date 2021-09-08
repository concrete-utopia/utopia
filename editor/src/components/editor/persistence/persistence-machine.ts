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
  deleteProject as deleteLocalProject,
} from '../../../common/persistence'
import { arrayContains, projectURLForProject } from '../../../core/shared/utils'
import { checkProjectOwnership } from '../../../common/server'
import { PersistentModel } from '../store/editor-state'
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

// Backend Calls

let _lastThumbnailGenerated: number = 0
const THUMBNAIL_THROTTLE = 300000

async function generateThumbnail(force: boolean): Promise<Buffer | null> {
  const now = Date.now()
  if (now - _lastThumbnailGenerated > THUMBNAIL_THROTTLE || force) {
    _lastThumbnailGenerated = now
    return getPNGBufferOfElementWithID('canvas-root', { width: 1152, height: 720 })
  } else {
    return Promise.resolve(null)
  }
}

export async function updateRemoteThumbnail(projectId: string, force: boolean): Promise<void> {
  const buffer = await generateThumbnail(force)
  if (buffer != null) {
    await saveThumbnail(buffer, projectId)
  }
}

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

interface FileWithFileName {
  fileName: string
  file: ProjectFile
}

function fileWithFileName(fileName: string, file: ProjectFile): FileWithFileName {
  return {
    fileName: fileName,
    file: file,
  }
}

interface ProjectWithFileChanges {
  filesWithFileNames: Array<FileWithFileName>
  projectModel: ProjectModel
}

function projectWithFileChanges(
  filesWithFileNames: Array<FileWithFileName>,
  projectModel: ProjectModel,
): ProjectWithFileChanges {
  return {
    filesWithFileNames: filesWithFileNames,
    projectModel: projectModel,
  }
}

async function saveProjectToServer(
  projectId: string,
  projectModel: ProjectModel,
): Promise<ProjectWithFileChanges> {
  const { assetsToUpload, projectWithChanges } = prepareAssetsForUploading(projectModel)

  await updateSavedProject(
    projectId,
    projectWithChanges.projectModel.content,
    projectWithChanges.projectModel.name,
  )
  if (assetsToUpload.length > 0) {
    await saveAssets(projectId, assetsToUpload)
  }

  updateRemoteThumbnail(projectId, false)
  deleteLocalProject(projectId)

  return projectWithChanges
}

async function saveProjectLocally(
  projectId: string,
  projectModel: ProjectModel,
): Promise<ProjectWithFileChanges> {
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

  return projectWithFileChanges([], projectModel)
}

async function downloadAssets(
  projectId: string,
  projectModel: ProjectModel,
): Promise<ProjectWithFileChanges> {
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

  return projectWithFileChanges(allProjectAssetsDownloaded, updatedProjectModel)
}

function scrubBase64FromFile(file: ImageFile | AssetFile): ImageFile | AssetFile {
  if (isImageFile(file)) {
    return imageFile(undefined, undefined, file.width, file.height, file.hash)
  } else {
    return assetFile(undefined)
  }
}

interface PreparedProject {
  assetsToUpload: Array<AssetToSave>
  projectWithChanges: ProjectWithFileChanges
}

function prepareAssetsForUploading(projectModel: ProjectModel): PreparedProject {
  const allProjectAssets = getAllProjectAssetFiles(projectModel.content.projectContents)
  let assetsToUpload: Array<AssetToSave> = []
  let updatedAssets: Array<FileWithFileName> = []

  const updatedProjectContents = allProjectAssets.reduce(
    (workingProjectContents, { fileName: assetPath, file: asset }) => {
      const fileType = getFileExtension(assetPath)
      if (asset.base64 != null) {
        const updatedFile = scrubBase64FromFile(asset)
        assetsToUpload.push(assetToSave(fileType, asset.base64, assetPath))
        updatedAssets.push(fileWithFileName(assetPath, updatedFile))
        return addFileToProjectContents(workingProjectContents, assetPath, updatedFile)
      } else {
        return workingProjectContents
      }
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
    assetsToUpload: assetsToUpload,
    projectWithChanges: projectWithFileChanges(updatedAssets, updatedProjectModel),
  }
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

interface UserLogOutEvent {
  type: 'USER_LOG_OUT'
}

type UserEvent = UserLogInEvent | UserLogOutEvent

type PersistenceEvent = CoreEvent | BackendEvent | UserEvent

interface ProjectModel {
  name: string
  content: PersistentModel
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
let queuedActions: Array<EditorAction> = [] // Queue up actions during events and transitions, then dispatch when ready

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
  if (waitingThrottledSaveEvent) {
    interpreter?.send(waitingThrottledSaveEvent)
  }
  clearThrottledSave()
}

export function initialisePersistence(dispatch: EditorDispatch, onProjectNotFound: () => void) {
  clearThrottledSave()
  queuedActions = [] // TODO This should really be part of the state machine most likely
  if (interpreter != null) {
    interpreter.stop()
  }

  interpreter = interpret(persistenceMachine)

  interpreter.onTransition((state, event) => {
    if (state.changed) {
      if (state.matches(`${Forking}.${DownloadingAssets}`)) {
        queuedActions.push(setForkedFromProjectID(state.context.projectId!))
        queuedActions.push(setProjectName(`${state.context.project!.name} (forked)`))
        queuedActions.push(showToast(notice('Project successfully forked!')))
      } else if (state.matches(Ready)) {
        const actionsToDispatch = queuedActions
        const projectIdChanged = actionsToDispatch.some(
          (action) => action.action === 'SET_PROJECT_ID',
        )
        if (projectIdChanged) {
          pushProjectURLToBrowserHistory(state.context.projectId!, state.context.project!.name)
        }
        queuedActions = []
        dispatch(actionsToDispatch)
      } else {
        switch (event.type) {
          case 'PROJECT_ID_CREATED':
            queuedActions.push(setProjectID(event.projectId))
            // TODO Update URL
            break
          case 'LOAD_FAILED':
            onProjectNotFound()
            break
          case 'LOAD_COMPLETE':
            // pull in the contents of the load function from actions.ts, or otherwise call it from here
            break
          case 'DOWNLOAD_ASSETS_COMPLETE': {
            const updateFileActions = event.downloadAssetsResult.filesWithFileNames.map(
              ({ fileName, file }) => updateFile(fileName, file, true),
            )
            queuedActions.push(...updateFileActions)
            break
          }
          case 'SAVE_COMPLETE':
            const updateFileActions = event.saveResult.filesWithFileNames.map(
              ({ fileName, file }) => updateFile(fileName, file, true),
            )
            queuedActions.push(...updateFileActions)
            lastSavedTS = Date.now()
            // TODO Show toasts after:
            // [ ] first local save
            // [ ] first server save
            break
        }
      }
    }
  })

  interpreter.start()
}

function getRemainingSaveDelay(): number {
  return Math.min(0, Date.now() - SaveThrottle - lastSavedTS)
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
  sendThrottledSave()
  e.preventDefault()
  e.returnValue = ''
})

export function load(projectId: string): void {
  interpreter?.send(loadEvent(projectId))
}

export function createNew(projectName: string, project: PersistentModel): void {
  interpreter?.send(newEvent({ name: projectName, content: project }))
}

export function fork(): void {
  interpreter?.send(forkEvent())
}

// For testing purposes only
export function setSaveThrottle(delay: number): void {
  SaveThrottle = delay
}

// createNewProjectFromImportedProject
// loadingSampleProjects?
// loading and creating new are still a shit show
