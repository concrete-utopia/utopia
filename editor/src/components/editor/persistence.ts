import * as localforage from 'localforage'
import {
  fetchLocalProject as fetchLocalProjectCommon,
  localProjectKey,
} from '../../common/persistence'
import { checkProjectOwnership } from '../../common/server'
import Utils from '../../utils/utils'
import { EditorDispatch } from './action-types'
import { load, loadSampleProject, newProject } from './actions/actions'
import {
  setProjectID,
  showToast,
  setSaveError,
  setForkedFromProjectID,
} from './actions/action-creators'
import {
  createNewProjectID,
  loadProject,
  saveAssets,
  saveImagesFromProject,
  saveThumbnail,
  updateSavedProject,
} from './server'
import {
  createNewProjectName,
  EditorState,
  PersistentModel,
  persistentModelForProjectContents,
  persistentModelFromEditorModel,
} from './store/editor-state'
import { UtopiaTsWorkers } from '../../core/workers/common/worker-types'
import { arrayContains, NO_OP, projectURLForProject } from '../../core/shared/utils'
import { getPNGBufferOfElementWithID } from './screenshot-utils'
import { ProjectImportSuccess } from '../../core/model/project-import'
import { CURRENT_PROJECT_VERSION } from './actions/migrations/migrations'
import { notice } from '../common/notice'
import { replaceAll } from '../../core/shared/string-utils'
import { isLoggedIn, isNotLoggedIn, LoginState } from '../../common/user'

interface NeverSaved {
  type: 'never-saved'
}

interface SaveInProgress {
  type: 'save-in-progress'
  remote: boolean
  queuedModelChange: PersistentModel | null
  queuedNameChange: string | null
  triggerNextImmediately: boolean
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
  setTimeoutId: NodeJS.Timer | null
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
  triggerNextImmediately: boolean,
): SaveInProgress {
  return {
    type: 'save-in-progress',
    remote: remote,
    queuedModelChange: queuedModelChange,
    queuedNameChange: queuedNameChange,
    triggerNextImmediately: triggerNextImmediately,
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
  setTimeoutId: NodeJS.Timer | null,
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

function clearScheduledSave() {
  if (isSaved(_saveState) || isSaveError(_saveState)) {
    if (_saveState.setTimeoutId != null) {
      clearTimeout(_saveState.setTimeoutId)
    }
  }
}

export function clearSaveState(): void {
  clearScheduledSave()
  _saveState = neverSaved()
}

window.addEventListener('beforeunload', (e) => {
  if (!isSafeToClose()) {
    forceQueuedSave()
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

export function createNewProject(dispatch: EditorDispatch, renderEditorRoot: () => void): void {
  _lastThumbnailGenerated = 0
  _saveState = neverSaved()
  newProject(dispatch, renderEditorRoot)
}

export async function createNewProjectFromImportedProject(
  importedProject: ProjectImportSuccess,
  workers: UtopiaTsWorkers,
  dispatch: EditorDispatch,
  renderEditorRoot: () => void,
): Promise<void> {
  _lastThumbnailGenerated = 0
  _saveState = neverSaved()
  const projectId = await createNewProjectID()
  const persistentModel = persistentModelForProjectContents(importedProject.contents)

  await serverSaveInner(
    dispatch,
    projectId,
    importedProject.projectName,
    persistentModel,
    importedProject.projectName,
    true,
  )
  await saveAssets(projectId, importedProject.assetsToUpload)
  load(dispatch, persistentModel, importedProject.projectName, projectId, workers, renderEditorRoot)
}

export async function createNewProjectFromSampleProject(
  projectId: string,
  dispatch: EditorDispatch,
  workers: UtopiaTsWorkers,
  renderEditorRoot: () => void,
): Promise<void> {
  _saveState = saved(true, Date.now(), projectId, projectId, dispatch, null, null, null)
  _lastThumbnailGenerated = 0
  await loadSampleProject(projectId, dispatch, workers, renderEditorRoot)
}

export function pushProjectURLToBrowserHistory(
  title: string,
  projectId: string,
  projectName: string,
): void {
  // Make sure we don't replace the query params
  const queryParams = window.top.location.search
  const projectURL = projectURLForProject(projectId, projectName)
  window.top.history.pushState({}, title, `${projectURL}${queryParams}`)
}

function onFirstSaveCompleted(projectId: string, name: string, dispatch: EditorDispatch): void {
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
): Promise<void> {
  if (isLocal() || projectId == null) {
    if (modelChange != null) {
      const projectIDToUse = projectId == null ? await createNewProjectID() : projectId
      await serverSaveInner(dispatch, projectIDToUse, projectName, modelChange, nameChange, false)
    }
  } else if (isSaveInProgress(_saveState)) {
    _saveState = saveInProgress(
      true,
      modelChange,
      nameChange,
      _saveState.triggerNextImmediately || force,
    )
  } else {
    if (force) {
      await serverSaveInner(dispatch, projectId, projectName, modelChange, nameChange, true)
    } else {
      await throttledServerSaveInner(dispatch, projectId, projectName, modelChange, nameChange)
    }
  }
}

export async function triggerForkProject(
  dispatch: EditorDispatch,
  editor: EditorState,
  loginState: LoginState,
): Promise<void> {
  const oldProjectId = editor.id
  const newProjectId = await createNewProjectID()
  const updatedEditor = {
    ...editor,
    forkedFromProjectId: oldProjectId,
    id: newProjectId,
  }
  await save(updatedEditor, dispatch, loginState, 'both', true)
  dispatch([setProjectID(newProjectId), setForkedFromProjectID(oldProjectId)])
}

async function checkCanSaveProject(projectId: string | null): Promise<boolean> {
  if (projectId == null) {
    return true
  } else {
    const ownerState = await checkProjectOwnership(projectId)
    return ownerState === 'unowned' || ownerState.isOwner
  }
}

let BaseSaveWaitTime = 30000

// For testing purposes only
export function setBaseSaveWaitTime(delay: number): void {
  BaseSaveWaitTime = delay
}

function waitTimeForSaveState(): number {
  switch (_saveState.type) {
    case 'never-saved':
      return 0
    case 'saved':
      return BaseSaveWaitTime
    case 'save-in-progress':
      return BaseSaveWaitTime
    case 'save-error':
      return BaseSaveWaitTime * _saveState.errorCount
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
): Promise<void> {
  switch (_saveState.type) {
    case 'never-saved':
      await serverSaveInner(dispatch, projectId, projectName, modelChange, nameChange, false)
      break
    case 'saved':
    case 'save-error':
      clearScheduledSave()
      const timeSinceLastSave = Date.now() - _saveState.timestamp
      const waitTime = waitTimeForSaveState() - timeSinceLastSave
      if (waitTime <= 0) {
        await serverSaveInner(dispatch, projectId, projectName, modelChange, nameChange, false)
      } else {
        const setTimeoutId = setTimeout(() => {
          throttledServerSaveInner(dispatch, projectId, projectName, modelChange, nameChange)
        }, waitTime)
        _saveState = {
          ..._saveState,
          queuedModelChange: modelChange ?? _saveState.queuedModelChange,
          queuedNameChange: nameChange ?? _saveState.queuedNameChange,
          setTimeoutId: setTimeoutId,
        }
      }
      break
    case 'save-in-progress':
      _saveState = saveInProgress(
        _saveState.remote,
        modelChange,
        nameChange,
        _saveState.triggerNextImmediately,
      )
      break
    default:
      const _exhaustiveCheck: never = _saveState
      throw new Error(`Unhandled saveState type ${JSON.stringify(_saveState)}`)
  }
}

function updateModelWithForkedId(
  model: PersistentModel,
  originalProjectId: string,
): PersistentModel {
  return {
    ...model,
    forkedFromProjectId: originalProjectId,
  }
}

export type SaveType = 'model' | 'name' | 'both'

export async function save(
  state: EditorState,
  dispatch: EditorDispatch,
  loginState: LoginState,
  saveType: SaveType,
  forceServerSave: boolean,
): Promise<void> {
  const modelChange =
    saveType === 'model' || saveType === 'both' ? persistentModelFromEditorModel(state) : null
  const nameChange = saveType === 'name' || saveType === 'both' ? state.projectName : null
  try {
    if (isLoggedIn(loginState)) {
      return saveToServer(
        dispatch,
        state.id,
        state.projectName,
        modelChange,
        nameChange,
        forceServerSave,
      )
    } else {
      return saveToLocalStorage(dispatch, state.id, state.projectName, modelChange, nameChange)
    }
  } catch (error) {
    console.error('Save not successful', error)
    return
  }
}

async function serverSaveInner(
  dispatch: EditorDispatch,
  currentProjectId: string,
  projectName: string,
  modelChange: PersistentModel | null,
  nameChange: string | null,
  forceThumbnail: boolean,
): Promise<void> {
  const priorErrorCount = isSaveError(_saveState) ? _saveState.errorCount : 0
  const isFirstSave = isLocal()

  clearScheduledSave()

  _saveState = saveInProgress(true, null, null, false)
  const name = nameChange ?? projectName
  try {
    const isOwner = await checkCanSaveProject(currentProjectId)
    const isFork = !isOwner
    const originalProjectId = stripOldLocalSuffix(currentProjectId)
    const projectId =
      isOwner && currentProjectId != null ? originalProjectId : await createNewProjectID()

    const modelWithForkedId: PersistentModel | null =
      isFork && modelChange != null
        ? updateModelWithForkedId(modelChange, originalProjectId)
        : modelChange

    await updateSavedProject(projectId, modelWithForkedId, name)
    dispatch([setSaveError(false)], 'everyone')
    updateRemoteThumbnail(projectId, forceThumbnail)
    maybeTriggerQueuedSave(dispatch, projectId, projectName, _saveState)

    if (isFirstSave) {
      dispatch([showToast(notice('Project successfully uploaded!'))], 'everyone')
    }
    if (isFork) {
      dispatch([showToast(notice('Project successfully forked!'))], 'everyone')
    }

    if (isFirstSave || isFork) {
      onFirstSaveCompleted(projectId, name, dispatch)
      localforage.removeItem(localProjectKey(currentProjectId))
    }
  } catch (e) {
    _saveState = saveError(
      _saveState.remote,
      currentProjectId,
      name,
      dispatch,
      modelChange,
      nameChange,
      null,
      priorErrorCount + 1,
      Date.now(),
    )
    throttledServerSaveInner(dispatch, currentProjectId, name, modelChange, nameChange)
    dispatch([setSaveError(true)], 'everyone')
  }
}

function maybeTriggerQueuedSave(
  dispatch: EditorDispatch,
  projectId: string,
  projectName: string,
  saveState: SaveInProgress,
): void {
  const queuedModelChange = saveState.queuedModelChange
  const queuedNameChange = saveState.queuedNameChange
  const force = saveState.triggerNextImmediately
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
  if (queuedModelChange != null || queuedNameChange != null) {
    if (saveState.remote) {
      saveToServer(dispatch, projectId, projectName, queuedModelChange, queuedNameChange, force)
    } else {
      localSaveInner(dispatch, projectId, projectName, queuedModelChange, queuedNameChange)
    }
  }
}

export async function updateRemoteThumbnail(projectId: string, force: boolean): Promise<void> {
  const buffer = await generateThumbnail(force)
  if (buffer != null) {
    await saveThumbnail(buffer, projectId)
  }
}

function stripOldLocalSuffix(id: string): string {
  return replaceAll(replaceAll(id, 'unsaved-', ''), '-cached', '')
}

export async function saveToLocalStorage(
  dispatch: EditorDispatch,
  projectId: string | null,
  projectName: string,
  model: PersistentModel | null,
  name: string | null,
): Promise<void> {
  if (isSaveInProgress(_saveState)) {
    _saveState = saveInProgress(false, model, name, _saveState.triggerNextImmediately)
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
): Promise<void> {
  const alreadyExists = projectId != null && (await projectIsStoredLocally(projectId))
  const isForked = !alreadyExists && !(await checkCanSaveProject(projectId))
  const projectIdToUse = projectId == null || isForked ? await createNewProjectID() : projectId
  const isFirstSave = !alreadyExists

  try {
    _saveState = saveInProgress(false, null, null, false)
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
      dispatch([showToast(notice('Locally cached project. Sign in to share!'))], 'everyone')
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
  renderProjectNotFound: () => void,
): Promise<void> {
  const project = await loadProject(projectId)
  switch (project.type) {
    case 'ProjectLoaded':
      _saveState = saved(true, Date.now(), projectId, project.title, dispatch, null, null, null)
      _lastThumbnailGenerated = 0
      await load(dispatch, project.content, project.title, projectId, workers, renderEditorRoot)
      break
    case 'ProjectNotFound':
      renderProjectNotFound()
      break
    default:
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
): Promise<void> {
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

export async function forceQueuedSave(): Promise<void> {
  if ((_saveState.type === 'saved' || _saveState.type === 'save-error') && _saveState.remote) {
    await serverSaveInner(
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

export function isSafeToClose(): boolean {
  return (
    _saveState.type === 'never-saved' ||
    (_saveState.type === 'saved' && _saveState.setTimeoutId == null)
  )
}
