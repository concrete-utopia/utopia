import { PersistentModel, StoryboardFilePath } from './store/editor-state'
import {
  saveToServer,
  setBaseSaveWaitTime,
  clearSaveState,
  LocalProject,
  loadFromLocalStorage,
  forceQueuedSave,
  triggerForkProject,
} from './persistence'
import { fastForEach, NO_OP } from '../../core/shared/utils'
import { createPersistentModel, delay } from '../../utils/utils.test-utils'
import { generateUID } from '../../core/shared/uid-utils'
import { AssetFile, isAssetFile, ProjectFile, TextFile } from '../../core/shared/project-file-types'
import { AssetToSave, SaveProjectResponse } from './server'
import { localProjectKey } from '../../common/persistence'
import { MockUtopiaTsWorkers } from '../../core/workers/workers'
import {
  addFileToProjectContents,
  AssetFileWithFileName,
  getContentsTreeFileFromString,
} from '../assets'
import { forceNotNull } from '../../core/shared/optional-utils'
import { assetFile } from '../../core/model/project-file-utils'
import { loggedInUser, notLoggedIn } from '../../common/user'
import { EditorAction, EditorDispatch } from './action-types'

let mockSaveLog: { [key: string]: Array<PersistentModel> } = {}
let mockDownloadedAssetsLog: { [projectId: string]: Array<string> } = {}
let mockUploadedAssetsLog: { [projectId: string]: Array<string> } = {}
let mockProjectsToError: Set<string> = new Set<string>()

const base64Contents = 'data:asset/xyz;base64,SomeBase64'
const AssetFileWithBase64 = assetFile(base64Contents)
const AssetFileWithoutBase64 = assetFile(undefined)

jest.mock('./server', () => ({
  updateSavedProject: async (
    projectId: string,
    persistentModel: PersistentModel | null,
    name: string | null,
  ): Promise<SaveProjectResponse> => {
    if (mockProjectsToError.has(projectId)) {
      return Promise.reject(`Deliberately failing for ${projectId}`)
    }

    if (persistentModel != null) {
      let currentLog = mockSaveLog[projectId] ?? []
      currentLog.push(persistentModel)
      mockSaveLog[projectId] = currentLog
    }

    return Promise.resolve({ id: projectId, ownerId: 'Owner' })
  },
  saveAssets: async (projectId: string, assets: Array<AssetToSave>): Promise<void> => {
    const uploadedAssets = assets.map((a) => a.fileName)
    mockUploadedAssetsLog[projectId] = uploadedAssets
    return Promise.resolve()
  },
  downloadAssetsFromProject: async (
    projectId: string | null,
    allProjectAssets: Array<AssetFileWithFileName>,
  ): Promise<Array<AssetFileWithFileName>> => {
    const downloadedAssets = allProjectAssets
      .filter((a) => a.file.base64 == null)
      .map((a) => a.fileName)
    mockDownloadedAssetsLog[projectId!] = downloadedAssets
    return allProjectAssets.map((assetWithFile) => ({
      ...assetWithFile,
      file: AssetFileWithBase64,
    }))
  },
  createNewProjectID: async (): Promise<string> => {
    return randomProjectID()
  },
  assetToSave: (fileType: string, base64: string, fileName: string): AssetToSave => {
    return {
      fileType: fileType,
      base64: base64,
      fileName: fileName,
    }
  },
}))

jest.mock('./actions/actions', () => ({
  ...(jest.requireActual('./actions/actions') as any), // This pattern allows us to only mock a single function https://jestjs.io/docs/en/jest-object#jestrequireactualmodulename
  load: async (): Promise<void> => {
    return Promise.resolve()
  },
}))

jest.mock('../../common/server', () => ({
  checkProjectOwnership: async (projectId: string) => ({
    isOwner: true,
  }),
}))
jest.setTimeout(10000)

let localProjects: { [key: string]: LocalProject } = {}
function addLocalProject(id: string, model: PersistentModel) {
  const now = new Date().toISOString()
  localProjects[localProjectKey(id)] = {
    model: model,
    createdAt: now,
    lastModified: now,
    thumbnail: '',
    name: ProjectName,
  }
}

jest.mock('localforage', () => ({
  getItem: async (id: string): Promise<LocalProject | null> => {
    return Promise.resolve(localProjects[id])
  },
  setItem: async (id: string, project: LocalProject) => {
    localProjects[id] = project
  },
  removeItem: async (id: string) => {
    delete localProjects[id]
  },
  keys: async () => {
    return Object.keys(localProjects)
  },
}))

let allProjectIds: Array<string> = []
function randomProjectID(): string {
  const newId = generateUID(allProjectIds)
  allProjectIds.push(newId)
  return newId
}

const ProjectName = 'Project Name'
const ModelChange = createPersistentModel()

export function updateModel(model: PersistentModel): PersistentModel {
  const oldFile = forceNotNull(
    'Unexpectedly null.',
    getContentsTreeFileFromString(model.projectContents, StoryboardFilePath),
  )
  const updatedFile = {
    ...oldFile,
    lastRevisedTime: Date.now(),
  }
  return {
    ...model,
    projectContents: addFileToProjectContents(
      model.projectContents,
      StoryboardFilePath,
      updatedFile,
    ),
  }
}

describe('Saving to the server', () => {
  describe('Forced saving', () => {
    it('Saves to the server', async () => {
      clearSaveState()
      const projectId = randomProjectID()
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, true)
      expect(mockSaveLog[projectId]).toEqual([ModelChange])
    })

    it('Clears a throttled save', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      const secondRevision = updateModel(ModelChange)
      await Promise.all([
        saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false),
        saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false),
        saveToServer(NO_OP, projectId, ProjectName, secondRevision, null, true),
      ])
      expect(mockSaveLog[projectId]).toEqual([ModelChange, secondRevision])
    })

    it('Queues the save to be immediately triggered if a save is in progress', async () => {
      clearSaveState()
      setBaseSaveWaitTime(1000)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      await Promise.all([
        saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false),
        saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, true),
      ])
      expect(mockSaveLog[projectId]).toEqual([ModelChange, firstRevision])
    })

    it('Forces the next queued save if the original forced save is still queued', async () => {
      clearSaveState()
      setBaseSaveWaitTime(1000)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      const secondRevision = updateModel(ModelChange)
      await Promise.all([
        saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false),
        saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, true),
        saveToServer(NO_OP, projectId, ProjectName, secondRevision, null, false),
      ])
      expect(mockSaveLog[projectId]).toEqual([ModelChange, secondRevision])
    })

    it('Does not affect future save throttling after being saved', async () => {
      clearSaveState()
      setBaseSaveWaitTime(1000)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      const secondRevision = updateModel(ModelChange)
      await Promise.all([
        saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false),
        saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, true),
      ])
      const nextSave = saveToServer(NO_OP, projectId, ProjectName, secondRevision, null, false)
      await delay(10)
      expect(mockSaveLog[projectId]).toEqual([ModelChange, firstRevision])
      await nextSave
    })
  })

  describe('Throttled saving', () => {
    it('Saves to the server when the current state is never-saved', async () => {
      clearSaveState()
      const projectId = randomProjectID()
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      expect(mockSaveLog[projectId]).toEqual([ModelChange])
    })

    it('Saves to the server when past the threshold', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      await saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
      await delay(20)
      expect(mockSaveLog[projectId]).toEqual([ModelChange, firstRevision])
    })

    it('Sets a timeout when not past the threshold', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      const save = saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
      expect(mockSaveLog[projectId]).toEqual([ModelChange])
      await save
      await delay(20)
      expect(mockSaveLog[projectId]).toEqual([ModelChange, firstRevision])
    })

    it('Does not spam the server if there was an error during saving', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      mockProjectsToError.add(projectId)
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      await delay(20)
      expect(mockSaveLog[projectId]).toBeUndefined()
      await delay(20)
      expect(mockSaveLog[projectId]).toBeUndefined()
      mockProjectsToError.delete(projectId)
      await delay(40)
      expect(mockSaveLog[projectId]).toEqual([ModelChange])
    })

    it('Replaces an errored save when a model change is made', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      mockProjectsToError.add(projectId)
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      await delay(20)
      expect(mockSaveLog[projectId]).toBeUndefined()
      await saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
      await delay(20)
      expect(mockSaveLog[projectId]).toBeUndefined()
      mockProjectsToError.delete(projectId)
      await delay(40)
      expect(mockSaveLog[projectId]).toEqual([firstRevision])
    })

    it('Queues saves to be throttled if a save is in progress', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      const secondRevision = updateModel(ModelChange)
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      const saves = [
        saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false),
        saveToServer(NO_OP, projectId, ProjectName, secondRevision, null, false),
      ]
      expect(mockSaveLog[projectId]).toEqual([ModelChange])
      await delay(40)
      expect(mockSaveLog[projectId]).toEqual([ModelChange, secondRevision])
      await Promise.all(saves)
    })

    it('forcing a queued save does not lose a throttled model change', async () => {
      clearSaveState()
      setBaseSaveWaitTime(1000)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
      await delay(10)
      await forceQueuedSave()
      await delay(10)
      expect(mockSaveLog[projectId]).toEqual([ModelChange, firstRevision])
    })
  })

  it('Clears a local saved project after uploading to the server', async () => {
    clearSaveState()
    setBaseSaveWaitTime(10)
    const projectId = randomProjectID()
    addLocalProject(projectId, ModelChange)
    await loadFromLocalStorage(projectId, NO_OP, false, new MockUtopiaTsWorkers(), NO_OP) // Load without triggering the upload
    expect(localProjects[localProjectKey(projectId)]).toBeDefined()
    await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, true) // Forcibly save to bypass throttling
    expect(mockSaveLog[projectId]).toEqual([ModelChange])
    expect(localProjects[localProjectKey(projectId)]).toBeUndefined()
  })
})

describe('Loading a local project', () => {
  it('Uploads to the server if the user is signed in', async () => {
    clearSaveState()
    setBaseSaveWaitTime(10)
    const projectId = randomProjectID()
    addLocalProject(projectId, ModelChange)
    expect(localProjects[localProjectKey(projectId)]).toBeDefined()
    await loadFromLocalStorage(projectId, NO_OP, true, new MockUtopiaTsWorkers(), NO_OP)
    await delay(20)
    expect(mockSaveLog[projectId]).toEqual([ModelChange])
    expect(localProjects[localProjectKey(projectId)]).toBeUndefined()
  })
})

describe('Forking a project', () => {
  const AssetFileName = 'asset.xyz'
  const startProject: PersistentModel = {
    ...ModelChange,
    projectContents: addFileToProjectContents(
      ModelChange.projectContents,
      AssetFileName,
      AssetFileWithoutBase64,
    ),
  }
  const startProjectIncludingBase64: PersistentModel = {
    ...ModelChange,
    projectContents: addFileToProjectContents(
      ModelChange.projectContents,
      AssetFileName,
      AssetFileWithBase64,
    ),
  }

  function setupTest() {
    clearSaveState()
    setBaseSaveWaitTime(10)
    const projectId = randomProjectID()
    let capturedData = {
      newProjectId: undefined as string | undefined,
      updatedFiles: {} as { [fileName: string]: AssetFile },
    }
    const dispatchFn: EditorDispatch = (actions: ReadonlyArray<EditorAction>) => {
      fastForEach(actions, (action) => {
        if (action.action === 'SET_PROJECT_ID') {
          if (capturedData.newProjectId != undefined) {
            fail(`Trying to repeatedly change the project ID`)
          }
          capturedData.newProjectId = action.id
        } else if (action.action === 'UPDATE_FILE' && isAssetFile(action.file)) {
          capturedData.updatedFiles[action.filePath] = action.file
        }
      })
    }
    return {
      projectId: projectId,
      capturedData: capturedData,
      dispatchFn: dispatchFn,
    }
  }

  it('Downloads the base 64 for assets and uploads them against the new project id if the user is signed in', async () => {
    const { projectId, capturedData, dispatchFn } = setupTest()
    await triggerForkProject(dispatchFn, startProject, projectId, '', loggedInUser({ userId: '1' }))
    expect(capturedData.newProjectId).toBeDefined()
    expect(capturedData.newProjectId).not.toEqual(projectId)
    expect(mockDownloadedAssetsLog[projectId]).toEqual([AssetFileName])
    expect(mockUploadedAssetsLog[capturedData.newProjectId!]).toEqual([AssetFileName])
    expect(mockSaveLog[capturedData.newProjectId!]).toEqual([
      {
        ...startProject,
        forkedFromProjectId: projectId,
      },
    ])
    expect(capturedData.updatedFiles[AssetFileName]).toEqual(AssetFileWithoutBase64)
  })

  it('Does not download the base 64 if the files already have them, and uploads them against the new project id if the user is signed in', async () => {
    const { projectId, capturedData, dispatchFn } = setupTest()
    await triggerForkProject(
      dispatchFn,
      startProjectIncludingBase64,
      projectId,
      '',
      loggedInUser({ userId: '1' }),
    )
    expect(capturedData.newProjectId).toBeDefined()
    expect(capturedData.newProjectId).not.toEqual(projectId)
    expect(mockDownloadedAssetsLog[projectId]).toEqual([])
    expect(mockUploadedAssetsLog[capturedData.newProjectId!]).toEqual([AssetFileName])
    expect(mockSaveLog[capturedData.newProjectId!]).toEqual([
      {
        ...startProject,
        forkedFromProjectId: projectId,
      },
    ])
    expect(capturedData.updatedFiles[AssetFileName]).toEqual(AssetFileWithoutBase64)
  })

  it('Downloads the base 64 for assets and stores them in the project if the user is not signed in', async () => {
    const { projectId, capturedData, dispatchFn } = setupTest()
    await triggerForkProject(dispatchFn, startProject, projectId, '', notLoggedIn)
    await delay(20)
    expect(capturedData.newProjectId).toBeDefined()
    expect(capturedData.newProjectId).not.toEqual(projectId)
    expect(mockDownloadedAssetsLog[projectId]).toEqual([AssetFileName])
    expect(mockUploadedAssetsLog[capturedData.newProjectId!]).toBeUndefined()
    expect(mockSaveLog[capturedData.newProjectId!]).toBeUndefined()
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]).toBeDefined()
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]!.model).toEqual({
      ...startProject,
      forkedFromProjectId: projectId,
      projectContents: addFileToProjectContents(
        ModelChange.projectContents,
        AssetFileName,
        assetFile(base64Contents),
      ),
    })
    expect(capturedData.updatedFiles[AssetFileName]).toEqual(AssetFileWithBase64)
  })

  it('Does not download or upload anything if the original project constains the base 64 and the user is not signed in', async () => {
    const { projectId, capturedData, dispatchFn } = setupTest()
    await triggerForkProject(dispatchFn, startProjectIncludingBase64, projectId, '', notLoggedIn)
    await delay(20)
    expect(capturedData.newProjectId).toBeDefined()
    expect(capturedData.newProjectId).not.toEqual(projectId)
    expect(mockDownloadedAssetsLog[projectId]).toEqual([])
    expect(mockUploadedAssetsLog[capturedData.newProjectId!]).toBeUndefined()
    expect(mockSaveLog[capturedData.newProjectId!]).toBeUndefined()
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]).toBeDefined()
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]!.model).toEqual({
      ...startProjectIncludingBase64,
      forkedFromProjectId: projectId,
    })
    expect(capturedData.updatedFiles[AssetFileName]).toEqual(AssetFileWithBase64)
  })
})
