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
import { PersistentModel } from './store/editor-state'
import { UtopiaTsWorkers } from '../../core/workers/common/worker-types'
import { arrayContains } from '../../core/shared/utils'
import { editorDispatch } from './store/dispatch'
const domtoimage = require('domtoimage')

const SAVE_THROTTLE_DELAY = 30000
const SAVE_RETRY_DELAY = 5000

interface NeverSaved {
  type: 'never-saved'
}

interface SaveInProgress {
  type: 'save-in-progress'
  remote: boolean
  queuedModelSave: PersistentModel | null
  queuedNameSave: string | null
}

interface Saved {
  type: 'saved'
  remote: boolean
  timestamp: number
  projectId: string
  dispatch: EditorDispatch
  queuedModelSave: PersistentModel | null
  queuedNameSave: string | null
  setTimeoutId: NodeJS.Timer | null
}

interface SaveError {
  type: 'save-error'
  remote: boolean
  projectId: string
  dispatch: EditorDispatch
  queuedModelSave: PersistentModel | null
  queuedNameSave: string | null
  setTimeoutId: NodeJS.Timer
}

type SaveState = NeverSaved | SaveInProgress | Saved | SaveError

function isNeverSaved(saveState: SaveState): saveState is NeverSaved {
  return saveState.type === 'never-saved'
}

function isSaveInProgress(saveState: SaveState): saveState is SaveInProgress {
  return saveState.type === 'save-in-progress'
}

function neverSaved(): NeverSaved {
  return {
    type: 'never-saved',
  }
}

function saveInProgress(
  remote: boolean,
  queuedModelSave: PersistentModel | null,
  queuedNameSave: string | null,
): SaveInProgress {
  return {
    type: 'save-in-progress',
    remote: remote,
    queuedModelSave: queuedModelSave,
    queuedNameSave: queuedNameSave,
  }
}

function saved(
  remote: boolean,
  timestamp: number,
  projectId: string,
  dispatch: EditorDispatch,
  queuedModelSave: PersistentModel | null,
  queuedNameSave: string | null,
  setTimeoutId: NodeJS.Timer | null,
): Saved {
  return {
    type: 'saved',
    remote: remote,
    timestamp: timestamp,
    projectId: projectId,
    dispatch: dispatch,
    queuedModelSave: queuedModelSave,
    queuedNameSave: queuedNameSave,
    setTimeoutId: setTimeoutId,
  }
}

function saveError(
  remote: boolean,
  projectId: string,
  dispatch: EditorDispatch,
  queuedModelSave: PersistentModel | null,
  queuedNameSave: string | null,
  setTimeoutId: NodeJS.Timer,
): SaveError {
  return {
    type: 'save-error',
    remote: remote,
    projectId: projectId,
    dispatch: dispatch,
    queuedModelSave: queuedModelSave,
    queuedNameSave: queuedNameSave,
    setTimeoutId: setTimeoutId,
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
  name: string | null
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
  _saveState = saved(true, Date.now(), projectId, dispatch, null, null, null)
  _lastThumbnailGenerated = 0
  await loadSampleProject(projectId, dispatch, workers, renderEditorRoot)
}

function onFirstSaveCompleted(projectId: string, dispatch: EditorDispatch) {
  dispatch([setProjectID(projectId)], 'everyone')
  window.top.history.pushState({}, `Utopia ${projectId}`, `/project/${projectId}/`)
}

export async function saveToServer(
  dispatch: EditorDispatch,
  projectId: string | null,
  model: PersistentModel | null,
  name: string | null,
  force: boolean,
) {
  if (isLocal() || projectId == null) {
    if (model != null) {
      await saveLocalProjectToServer(dispatch, projectId, model, name)
    }
  } else if (isSaveInProgress(_saveState)) {
    _saveState = saveInProgress(true, model, name)
  } else {
    if (force) {
      await serverSaveInner(dispatch, projectId, model, name, true)
    } else {
      await throttledServerSaveInner(dispatch, projectId, model, name)
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

async function throttledServerSaveInner(
  dispatch: EditorDispatch,
  projectId: string,
  model: PersistentModel | null,
  name: string | null,
) {
  switch (_saveState.type) {
    case 'never-saved':
      await serverSaveInner(dispatch, projectId, model, name, false)
      break
    case 'saved':
      if (_saveState.setTimeoutId != null) {
        clearTimeout(_saveState.setTimeoutId)
      }
      const timeLeftSinceLastSave = Date.now() - _saveState.timestamp
      if (timeLeftSinceLastSave >= SAVE_THROTTLE_DELAY) {
        await serverSaveInner(dispatch, projectId, model, name, false)
      } else {
        const setTimeoutId = setTimeout(() => {
          throttledServerSaveInner(dispatch, projectId, model, name)
        }, SAVE_THROTTLE_DELAY - timeLeftSinceLastSave)
        _saveState = saved(
          _saveState.remote,
          _saveState.timestamp,
          projectId,
          dispatch,
          model,
          name,
          setTimeoutId,
        )
      }
      break
    case 'save-error':
      if (_saveState.setTimeoutId != null) {
        clearTimeout(_saveState.setTimeoutId)
      }
      await serverSaveInner(dispatch, projectId, model, name, false)
      break
    case 'save-in-progress':
      _saveState = saveInProgress(_saveState.remote, model, name)
      break
    default:
      const _exhaustiveCheck: never = _saveState
      throw new Error(`Unhandled saveState type ${JSON.stringify(_saveState)}`)
  }
}

async function serverSaveInner(
  dispatch: EditorDispatch,
  projectId: string,
  model: PersistentModel | null,
  name: string | null,
  forceThumbnail: boolean,
) {
  _saveState = saveInProgress(true, null, null)
  try {
    const isOwner = await checkCanSaveProject(projectId)
    if (isOwner) {
      await updateSavedProject(projectId, model, name)
      dispatch([setSaveError(false)], 'everyone')
      updateRemoteThumbnail(projectId, forceThumbnail)
      maybeTriggerQueuedSave(dispatch, projectId, _saveState)
      _saveState = saved(_saveState.remote, Date.now(), projectId, dispatch, null, null, null)
    } else {
      if (model != null) {
        await saveLocalProjectToServer(dispatch, projectId, model, name)
      }
    }
  } catch (e) {
    const setTimeoutId = setTimeout(() => {
      throttledServerSaveInner(dispatch, projectId, model, name)
    }, SAVE_RETRY_DELAY)
    _saveState = saveError(_saveState.remote, projectId, dispatch, model, name, setTimeoutId)
    dispatch([setSaveError(true)], 'everyone')
  }
}

async function saveLocalProjectToServer(
  dispatch: EditorDispatch,
  currentProjectId: string | null,
  model: PersistentModel,
  name: string | null,
) {
  _saveState = saveInProgress(true, null, null)
  const isOwner = await checkCanSaveProject(currentProjectId)
  const projectId =
    isOwner && currentProjectId != null
      ? stripOldLocalSuffix(currentProjectId)
      : await createNewProjectID()
  await updateSavedProject(projectId, model, name)
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
  onFirstSaveCompleted(projectId, dispatch)
  maybeTriggerQueuedSave(dispatch, projectId, _saveState)
}

function maybeTriggerQueuedSave(
  dispatch: EditorDispatch,
  projectId: string,
  saveState: SaveInProgress,
) {
  const queuedModelSave = saveState.queuedModelSave
  const queuedNameSave = saveState.queuedNameSave
  if (queuedModelSave != null || queuedNameSave != null) {
    _saveState = saveInProgress(saveState.remote, null, null)
    if (saveState.remote) {
      throttledServerSaveInner(dispatch, projectId, queuedModelSave, queuedNameSave)
    } else {
      localSaveInner(dispatch, projectId, queuedModelSave, queuedNameSave)
    }
  } else {
    _saveState = saved(saveState.remote, Date.now(), projectId, dispatch, null, null, null)
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
  model: PersistentModel | null,
  name: string | null,
) {
  if (isSaveInProgress(_saveState)) {
    _saveState = saveInProgress(false, model, name)
  } else {
    localSaveInner(dispatch, projectId, model, name)
  }
}

export async function localSaveInner(
  dispatch: EditorDispatch,
  projectId: string | null,
  model: PersistentModel | null,
  name: string | null,
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
      existing == null ? model : Utils.defaultIfNull(existing.model, model),
    )
    const nameToStore: string = Utils.defaultIfNull<string>(
      'Unnamed',
      existing == null ? name : Utils.defaultIfNull(existing.name, name),
    )
    const localProject: LocalProject = {
      model: modelToStore,
      createdAt: createdAt,
      lastModified: modifiedAt,
      thumbnail: thumbnail,
      name: nameToStore,
    }

    localforage.setItem(localProjectKey(projectIdToUse), localProject)
    if (isFirstSave) {
      onFirstSaveCompleted(projectIdToUse, dispatch)
      dispatch(
        [
          showToast({
            message: 'Locally cached project. Sign in to share!',
          }),
        ],
        'everyone',
      )
    }
    maybeTriggerQueuedSave(dispatch, projectIdToUse, _saveState)
  } catch (e) {
    console.error(e)
    if (isSaveInProgress(_saveState)) {
      maybeTriggerQueuedSave(dispatch, projectIdToUse, _saveState)
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
    _saveState = saved(true, Date.now(), projectId, dispatch, null, null, null)
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
    _saveState = saved(false, Date.now(), projectId, dispatch, null, null, null)
    _lastThumbnailGenerated = 0
    let projectName = Utils.defaultIfNull<string>('Unnamed', localProject.name)
    await load(dispatch, localProject.model, projectName, projectId, workers, renderEditorRoot)
    if (shouldUploadToServer) {
      // Upload the project now that the user has signed in
      saveImagesFromProject(projectId, localProject.model).then((modelWithReplacedImages) => {
        saveToServer(dispatch, projectId, modelWithReplacedImages, projectName, false)
      })
    }
  }
}

export async function forceServerSave() {
  if ((_saveState.type === 'saved' || _saveState.type === 'save-error') && _saveState.remote) {
    serverSaveInner(
      _saveState.dispatch,
      _saveState.projectId,
      _saveState.queuedModelSave,
      _saveState.queuedNameSave,
      true,
    )
  }
}

let _lastThumbnailGenerated: number = 0
const THUMBNAIL_THROTTLE = 300000
const THUMBNAIL_BASE64_PREFIX = 'data:image/png;base64,'

async function generateThumbnail(force: boolean): Promise<Buffer | null> {
  const now = Date.now()
  const canvasNode = document.getElementById('canvas-root')
  if ((now - _lastThumbnailGenerated > THUMBNAIL_THROTTLE || force) && canvasNode != null) {
    _lastThumbnailGenerated = now
    const png = await domtoimage.toPng(canvasNode, { width: 1152, height: 720 })
    // Kill me now
    const stripped = png.replace(THUMBNAIL_BASE64_PREFIX, '')
    const pngBuffer = Buffer.from(stripped, 'base64')
    return pngBuffer
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
