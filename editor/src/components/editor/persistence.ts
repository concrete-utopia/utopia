import * as localforage from 'localforage'
import * as R from 'ramda'
import {
  fetchLocalProject as fetchLocalProjectCommon,
  localProjectKey,
} from '../../common/persistence'
import { checkProjectOwnership } from '../../common/server'
import Utils from '../../utils/utils'
import { EditorDispatch } from './action-types'
import {
  load,
  loadSampleProject,
  newProject,
  setProjectID,
  showToast,
  setSaveError,
} from './actions/actions'
import {
  createNewProjectID,
  loadProject,
  saveImagesFromProject,
  saveThumbnail,
  updateSavedProject,
} from './server'
import { createNewProjectName, PersistentModel } from './store/editor-state'
import { UtopiaTsWorkers } from '../../core/workers/common/worker-types'
import { arrayContains, projectURLForProject } from '../../core/shared/utils'
import { getPNGBufferOfElementWithID } from './screenshot-utils'

const SAVE_THROTTLE_DELAY = 30000

interface NeverSaved {
  type: 'never-saved'
}

interface SaveInProgress {
  type: 'save-in-progress'
  remote: boolean
  queuedModelChange: PersistentModel | null
  queuedNameChange: string | null
}

interface Saved {
  type: 'saved'
  remote: boolean
  timestamp: number
  projectId: string
  projectName: string
  dispatch: EditorDispatch
  queuedModelChange: PersistentModel | null
  queuedNameChange: string | null
  setTimeoutId: NodeJS.Timer | null
}

interface SaveError {
  type: 'save-error'
  remote: boolean
  projectId: string
  projectName: string
  dispatch: EditorDispatch
  queuedModelChange: PersistentModel | null
  queuedNameChange: string | null
  setTimeoutId: NodeJS.Timer
  errorCount: number
  timestamp: number
}

type SaveState = NeverSaved | SaveInProgress | Saved | SaveError

function isNeverSaved(saveState: SaveState): saveState is NeverSaved {
  return saveState.type === 'never-saved'
}

function isSaveInProgress(saveState: SaveState): saveState is SaveInProgress {
  return saveState.type === 'save-in-progress'
}

function isSaveError(saveState: SaveState): saveState is SaveError {
  return saveState.type === 'save-error'
}

function isSaved(saveState: SaveState): saveState is Saved {
  return saveState.type === 'saved'
}

function neverSaved(): NeverSaved {
  return {
    type: 'never-saved',
  }
}

function saveInProgress(
  remote: boolean,
  queuedModelChange: PersistentModel | null,
  queuedNameChange: string | null,
): SaveInProgress {
  return {
    type: 'save-in-progress',
    remote: remote,
    queuedModelChange: queuedModelChange,
    queuedNameChange: queuedNameChange,
  }
}

function saved(
  remote: boolean,
  timestamp: number,
  projectId: string,
  projectName: string,
  dispatch: EditorDispatch,
  queuedModelChange: PersistentModel | null,
  queuedNameChange: string | null,
  setTimeoutId: NodeJS.Timer | null,
): Saved {
  return {
    type: 'saved',
    remote: remote,
    timestamp: timestamp,
    projectId: projectId,
    projectName: projectName,
    dispatch: dispatch,
    queuedModelChange: queuedModelChange,
    queuedNameChange: queuedNameChange,
    setTimeoutId: setTimeoutId,
  }
}

function saveError(
  remote: boolean,
  projectId: string,
  projectName: string,
  dispatch: EditorDispatch,
  queuedModelChange: PersistentModel | null,
  queuedNameChange: string | null,
  setTimeoutId: NodeJS.Timer,
  errorCount: number,
  timestamp: number,
): SaveError {
  return {
    type: 'save-error',
    remote: remote,
    projectId: projectId,
    projectName: projectName,
    dispatch: dispatch,
    queuedModelChange: queuedModelChange,
    queuedNameChange: queuedNameChange,
    setTimeoutId: setTimeoutId,
    errorCount: errorCount,
    timestamp: timestamp,
  }
}

let _saveState: SaveState = neverSaved()

window.addEventListener('beforeunload', (e) => {
  if (!isSafeToClose(_saveState)) {
    forceServerSave()
    e.preventDefault()
    e.returnValue = ''
  }
})

export interface LocalProject {
  model: PersistentModel
  createdAt: string
  lastModified: string
  thumbnail: string
  name: string
}

export function isLocal(): boolean {
  return isNeverSaved(_saveState) || !_saveState.remote
}

export function createNewProject(dispatch: EditorDispatch, renderEditorRoot: () => void) {
  _lastThumbnailGenerated = 0
  _saveState = neverSaved()
  newProject(dispatch, renderEditorRoot)
}

export async function createNewProjectFromSampleProject(
  projectId: string,
  dispatch: EditorDispatch,
  workers: UtopiaTsWorkers,
  renderEditorRoot: () => void,
) {
  _saveState = saved(true, Date.now(), projectId, projectId, dispatch, null, null, null)
  _lastThumbnailGenerated = 0
  await loadSampleProject(projectId, dispatch, workers, renderEditorRoot)
}

export function pushProjectURLToBrowserHistory(
  title: string,
  projectId: string,
  projectName: string,
): void {
  window.top.history.pushState({}, title, projectURLForProject(projectId, projectName))
}

function onFirstSaveCompleted(projectId: string, name: string, dispatch: EditorDispatch) {
  dispatch([setProjectID(projectId)], 'everyone')
  pushProjectURLToBrowserHistory(`Utopia ${projectId}`, projectId, name)
}

export async function saveToServer(
  dispatch: EditorDispatch,
  projectId: string | null,
  projectName: string,
  modelChange: PersistentModel | null,
  nameChange: string | null,
  force: boolean,
) {
  if (isLocal() || projectId == null) {
    if (modelChange != null) {
      await saveLocalProjectToServer(dispatch, projectId, projectName, modelChange, nameChange)
    }
  } else if (isSaveInProgress(_saveState)) {
    _saveState = saveInProgress(true, modelChange, nameChange)
  } else {
    if (force) {
      await serverSaveInner(dispatch, projectId, projectName, modelChange, nameChange, true)
    } else {
      await throttledServerSaveInner(dispatch, projectId, projectName, modelChange, nameChange)
    }
  }
}

async function checkCanSaveProject(projectId: string | null): Promise<boolean> {
  if (projectId == null) {
    return true
  } else {
    const ownerState = await checkProjectOwnership(projectId)
    return ownerState === 'unowned' || ownerState.isOwner
  }
}

function waitTimeForSaveState(): number {
  switch (_saveState.type) {
    case 'never-saved':
      return 0
    case 'saved':
      return SAVE_THROTTLE_DELAY
    case 'save-in-progress':
      return SAVE_THROTTLE_DELAY
    case 'save-error':
      return SAVE_THROTTLE_DELAY * _saveState.errorCount
    default:
      const _exhaustiveCheck: never = _saveState
      throw new Error(`Unhandled saveState type ${JSON.stringify(_saveState)}`)
  }
}

async function throttledServerSaveInner(
  dispatch: EditorDispatch,
  projectId: string,
  projectName: string,
  modelChange: PersistentModel | null,
  nameChange: string | null,
) {
  switch (_saveState.type) {
    case 'never-saved':
      await serverSaveInner(dispatch, projectId, projectName, modelChange, nameChange, false)
      break
    case 'saved':
    case 'save-error':
      if (_saveState.setTimeoutId != null) {
        clearTimeout(_saveState.setTimeoutId)
      }
      const timeSinceLastSave = Date.now() - _saveState.timestamp
      const waitTime = waitTimeForSaveState() - timeSinceLastSave
      if (waitTime <= 0) {
        await serverSaveInner(dispatch, projectId, projectName, modelChange, nameChange, false)
      } else {
        const setTimeoutId = setTimeout(() => {
          throttledServerSaveInner(dispatch, projectId, projectName, modelChange, nameChange)
        }, waitTime)
        _saveState = saved(
          _saveState.remote,
          _saveState.timestamp,
          projectId,
          projectName,
          dispatch,
          modelChange,
          nameChange,
          setTimeoutId,
        )
      }
      break
    case 'save-in-progress':
      _saveState = saveInProgress(_saveState.remote, modelChange, nameChange)
      break
    default:
      const _exhaustiveCheck: never = _saveState
      throw new Error(`Unhandled saveState type ${JSON.stringify(_saveState)}`)
  }
}

async function serverSaveInner(
  dispatch: EditorDispatch,
  projectId: string,
  projectName: string,
  modelChange: PersistentModel | null,
  nameChange: string | null,
  forceThumbnail: boolean,
) {
  const priorErrorCount = isSaveError(_saveState) ? _saveState.errorCount : 0
  _saveState = saveInProgress(true, null, null)
  const name = nameChange ?? projectName
  try {
    const isOwner = await checkCanSaveProject(projectId)
    if (isOwner) {
      await updateSavedProject(projectId, modelChange, name)
      dispatch([setSaveError(false)], 'everyone')
      updateRemoteThumbnail(projectId, forceThumbnail)
      maybeTriggerQueuedSave(dispatch, projectId, projectName, _saveState)
      _saveState = saved(_saveState.remote, Date.now(), projectId, name, dispatch, null, null, null)
    } else {
      if (modelChange != null) {
        await saveLocalProjectToServer(dispatch, projectId, projectName, modelChange, nameChange)
      }
    }
  } catch (e) {
    const newErrorCount = priorErrorCount + 1
    const setTimeoutId = setTimeout(() => {
      throttledServerSaveInner(dispatch, projectId, name, modelChange, nameChange)
    }, SAVE_THROTTLE_DELAY * newErrorCount)
    _saveState = saveError(
      _saveState.remote,
      projectId,
      name,
      dispatch,
      modelChange,
      nameChange,
      setTimeoutId,
      newErrorCount,
      Date.now(),
    )
    dispatch([setSaveError(true)], 'everyone')
  }
}

async function saveLocalProjectToServer(
  dispatch: EditorDispatch,
  currentProjectId: string | null,
  projectName: string,
  modelChange: PersistentModel,
  nameChange: string | null,
) {
  _saveState = saveInProgress(true, null, null)
  const name = nameChange ?? projectName
  const isOwner = await checkCanSaveProject(currentProjectId)
  const projectId =
    isOwner && currentProjectId != null
      ? stripOldLocalSuffix(currentProjectId)
      : await createNewProjectID()
  await updateSavedProject(projectId, modelChange, name)
  if (currentProjectId != null) {
    localforage.removeItem(localProjectKey(currentProjectId))
  }

  if (currentProjectId != null) {
    if (isOwner) {
      dispatch(
        [
          showToast({
            message: 'Project successfully uploaded!',
          }),
        ],
        'everyone',
      )
    } else {
      dispatch(
        [
          showToast({
            message: 'Project successfully forked!',
          }),
        ],
        'everyone',
      )
    }
  }

  updateRemoteThumbnail(projectId, false)
  onFirstSaveCompleted(projectId, name, dispatch)
  maybeTriggerQueuedSave(dispatch, projectId, name, _saveState)
}

function maybeTriggerQueuedSave(
  dispatch: EditorDispatch,
  projectId: string,
  projectName: string,
  saveState: SaveInProgress,
) {
  const queuedModelChange = saveState.queuedModelChange
  const queuedNameChange = saveState.queuedNameChange
  if (queuedModelChange != null || queuedNameChange != null) {
    _saveState = saveInProgress(saveState.remote, null, null)
    if (saveState.remote) {
      throttledServerSaveInner(
        dispatch,
        projectId,
        projectName,
        queuedModelChange,
        queuedNameChange,
      )
    } else {
      localSaveInner(dispatch, projectId, projectName, queuedModelChange, queuedNameChange)
    }
  } else {
    _saveState = saved(
      saveState.remote,
      Date.now(),
      projectId,
      projectName,
      dispatch,
      null,
      null,
      null,
    )
  }
}

export async function updateRemoteThumbnail(projectId: string, force: boolean): Promise<void> {
  const buffer = await generateThumbnail(force)
  if (buffer != null) {
    await saveThumbnail(buffer, projectId)
  }
}

function stripOldLocalSuffix(id: string): string {
  return R.replace('-cached', '', R.replace('unsaved-', '', id))
}

export async function saveToLocalStorage(
  dispatch: EditorDispatch,
  projectId: string | null,
  projectName: string,
  model: PersistentModel | null,
  name: string | null,
) {
  if (isSaveInProgress(_saveState)) {
    _saveState = saveInProgress(false, model, name)
  } else {
    localSaveInner(dispatch, projectId, projectName, model, name)
  }
}

export async function localSaveInner(
  dispatch: EditorDispatch,
  projectId: string | null,
  projectName: string,
  modelChange: PersistentModel | null,
  nameChange: string | null,
) {
  const alreadyExists = projectId != null && (await projectIsStoredLocally(projectId))
  const isForked = !alreadyExists && !(await checkCanSaveProject(projectId))
  const projectIdToUse = projectId == null || isForked ? await createNewProjectID() : projectId
  const isFirstSave = !alreadyExists

  try {
    _saveState = saveInProgress(false, null, null)
    //const buffer = await generateThumbnail(false)
    //const newThumbnail = buffer == null ? null : `${THUMBNAIL_BASE64_PREFIX}${buffer.toString('base64')}`
    const newThumbnail = null
    const existing = await localforage.getItem<LocalProject | null>(localProjectKey(projectIdToUse))
    const existingThumbnail = existing == null ? '' : existing.thumbnail
    const now = new Date().toISOString()
    const thumbnail = newThumbnail || existingThumbnail
    const createdAt = existing == null ? now : existing.createdAt
    const modifiedAt = now

    const modelToStore: PersistentModel = Utils.forceNotNull(
      'Trying to save with no model at all',
      existing == null ? modelChange : Utils.defaultIfNull(existing.model, modelChange),
    )
    const name = nameChange ?? existing?.name ?? projectName

    const localProject: LocalProject = {
      model: modelToStore,
      createdAt: createdAt,
      lastModified: modifiedAt,
      thumbnail: thumbnail,
      name: name,
    }

    localforage.setItem(localProjectKey(projectIdToUse), localProject)
    if (isFirstSave) {
      onFirstSaveCompleted(projectIdToUse, name, dispatch)
      dispatch(
        [
          showToast({
            message: 'Locally cached project. Sign in to share!',
          }),
        ],
        'everyone',
      )
    }
    maybeTriggerQueuedSave(dispatch, projectIdToUse, name, _saveState)
  } catch (e) {
    console.error(e)
    if (isSaveInProgress(_saveState)) {
      maybeTriggerQueuedSave(dispatch, projectIdToUse, projectName, _saveState)
    }
  }
}

export async function loadFromServer(
  projectId: string,
  dispatch: EditorDispatch,
  workers: UtopiaTsWorkers,
  renderEditorRoot: () => void,
) {
  const project = await loadProject(projectId)
  if (project.type === 'ProjectLoaded') {
    _saveState = saved(true, Date.now(), projectId, project.title, dispatch, null, null, null)
    _lastThumbnailGenerated = 0
    await load(dispatch, project.content, project.title, projectId, workers, renderEditorRoot)
  } else {
    console.error(`Invalid project load response: ${project}`)
  }
}

export async function projectIsStoredLocally(projectId: string): Promise<boolean> {
  const keys = await localforage.keys()
  const targetKey = localProjectKey(projectId)
  return arrayContains(keys, targetKey)
}

async function fetchLocalProject(projectId: string): Promise<LocalProject> {
  return fetchLocalProjectCommon(projectId) as Promise<LocalProject>
}

export async function loadFromLocalStorage(
  projectId: string,
  dispatch: EditorDispatch,
  shouldUploadToServer: boolean,
  workers: UtopiaTsWorkers,
  renderEditorRoot: () => void,
) {
  const localProject = await fetchLocalProject(projectId)
  if (localProject == null) {
    console.error(`Failed to load project with ID ${projectId} from local storage`)
  } else {
    let projectName = localProject.name ?? createNewProjectName() // Should never be null now, but just in case someone has very old local projects lying about
    _saveState = saved(false, Date.now(), projectId, projectName, dispatch, null, null, null)
    _lastThumbnailGenerated = 0
    await load(dispatch, localProject.model, projectName, projectId, workers, renderEditorRoot)
    if (shouldUploadToServer) {
      // Upload the project now that the user has signed in
      saveImagesFromProject(projectId, localProject.model).then((modelWithReplacedImages) => {
        saveToServer(dispatch, projectId, projectName, modelWithReplacedImages, projectName, false)
      })
    }
  }
}

export async function forceServerSave() {
  if ((_saveState.type === 'saved' || _saveState.type === 'save-error') && _saveState.remote) {
    serverSaveInner(
      _saveState.dispatch,
      _saveState.projectId,
      _saveState.projectName,
      _saveState.queuedModelChange,
      _saveState.queuedNameChange,
      true,
    )
  }
}

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

function isSafeToClose(saveState: SaveState) {
  return (
    _saveState.type === 'never-saved' ||
    (_saveState.type === 'saved' && _saveState.setTimeoutId == null)
  )
}
