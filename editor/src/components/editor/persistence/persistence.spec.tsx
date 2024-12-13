import type { PersistentModel } from '../store/editor-state'
import { StoryboardFilePath } from '../store/editor-state'
import { fastForEach, NO_OP } from '../../../core/shared/utils'
import { createPersistentModel, delay } from '../../../utils/utils.test-utils'
import { generateUID } from '../../../core/shared/uid-utils'
import type { AssetFile, TextFile } from '../../../core/shared/project-file-types'
import { assetFile } from '../../../core/shared/project-file-types'
import { isAssetFile, ProjectFile } from '../../../core/shared/project-file-types'
import type { AssetToSave, SaveProjectResponse, LoadProjectResponse } from '../server'
import { localProjectKey } from '../../../common/persistence'
import { MockUtopiaTsWorkers } from '../../../core/workers/workers'
import type { AssetFileWithFileName } from '../../assets'
import { gitBlobChecksumFromBase64, gitBlobChecksumFromText } from '../../assets'
import { addFileToProjectContents, getProjectFileByFilePath } from '../../assets'
import { forceNotNull } from '../../../core/shared/optional-utils'
import type {
  EditorAction,
  EditorDispatch,
  SetForkedFromProjectID,
  SetProjectName,
} from '../action-types'
import type { LocalProject, PersistenceContext } from './generic/persistence-types'
import { PersistenceMachine } from './persistence'
import { PersistenceBackend } from './persistence-backend'

let mockSaveLog: { [key: string]: Array<PersistentModel> } = {}
let mockDownloadedAssetsLog: { [projectId: string]: Array<string> } = {}
let mockUploadedAssetsLog: { [projectId: string]: Array<string> } = {}
let mockProjectsToError: Set<string> = new Set<string>()
let mockUnownedProjects: Set<string> = new Set<string>()

let allProjectIds: Array<string> = []
let serverProjects: { [key: string]: PersistentModel } = {}
let localProjects: { [key: string]: LocalProject<PersistentModel> } = {}

const base64Contents = 'data:asset/xyz;base64,SomeBase64'
const mockAssetFileWithBase64 = assetFile(base64Contents, gitBlobChecksumFromBase64(base64Contents))
const AssetFileWithoutBase64 = assetFile(undefined, gitBlobChecksumFromBase64(base64Contents))

const ProjectName = 'Project Name'
const BaseModel = createPersistentModel()
const FirstRevision = updateModel(BaseModel)
const SecondRevision = updateModel(FirstRevision)
const ThirdRevision = updateModel(SecondRevision)
const FourthRevision = updateModel(ThirdRevision)

let forcedNextProjectId: string | null = null

function mockRandomProjectID(): string {
  const newId =
    forcedNextProjectId == null ? generateUID(new Set(allProjectIds)) : forcedNextProjectId
  forcedNextProjectId = null
  allProjectIds.push(newId)
  return newId
}

function updateModel(model: PersistentModel): PersistentModel {
  const oldFile = forceNotNull(
    'Unexpectedly null.',
    getProjectFileByFilePath(model.projectContents, StoryboardFilePath),
  )
  const updatedFile = {
    ...oldFile,
    versionNumber: (oldFile as TextFile).versionNumber + 1,
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

jest.mock('../server', () => ({
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
      serverProjects[projectId] = persistentModel
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
      file: mockAssetFileWithBase64,
    }))
  },
  createNewProjectID: async (): Promise<string> => {
    return mockRandomProjectID()
  },
  createNewProject: async (
    persistentModel: PersistentModel,
    name: string,
    accessLevel: string | null,
  ): Promise<SaveProjectResponse> => {
    const projectId = mockRandomProjectID()
    if (persistentModel != null) {
      let currentLog = mockSaveLog[projectId] ?? []
      currentLog.push(persistentModel)
      mockSaveLog[projectId] = currentLog
      serverProjects[projectId] = persistentModel
    }
    return { id: mockRandomProjectID(), ownerId: 'Owner' }
  },
  assetToSave: (fileType: string, base64: string, fileName: string): AssetToSave => {
    return {
      fileType: fileType,
      base64: base64,
      fileName: fileName,
    }
  },
  loadProject: async (
    projectId: string,
    _lastSavedTS: string | null = null,
  ): Promise<LoadProjectResponse> => {
    const project = serverProjects[projectId]
    if (project == null) {
      return { type: 'ProjectNotFound' }
    } else {
      return {
        type: 'ProjectLoaded',
        id: projectId,
        ownerId: 'Owner',
        title: ProjectName,
        createdAt: '',
        modifiedAt: '',
        content: project,
      }
    }
  },
}))

jest.mock('../actions/actions', () => ({
  ...(jest.requireActual('../actions/actions') as any), // This pattern allows us to only mock a single function https://jestjs.io/docs/en/jest-object#jestrequireactualmodulename
  load: async (): Promise<void> => {
    return Promise.resolve()
  },
}))

jest.mock('../../../common/server', () => ({
  checkProjectOwnership: async (projectId: string) => ({
    isOwner: !mockUnownedProjects.has(projectId),
  }),
}))
jest.setTimeout(10000)

jest.mock('localforage', () => ({
  getItem: async (id: string): Promise<LocalProject<PersistentModel> | null> => {
    return Promise.resolve(localProjects[id])
  },
  setItem: async (id: string, project: LocalProject<PersistentModel>) => {
    localProjects[id] = project
  },
  removeItem: async (id: string) => {
    delete localProjects[id]
  },
  keys: async () => {
    return Object.keys(localProjects)
  },
}))

function setupTest(saveThrottle: number = 0) {
  let capturedData = {
    newProjectId: undefined as string | undefined,
    updatedFiles: {} as { [fileName: string]: AssetFile },
    dispatchedActions: [] as Array<EditorAction>,
    projectNotFound: false,
    projectNotAuthorized: false,
    createdOrLoadedProject: undefined as PersistentModel | undefined,
    latestContext: {
      projectOwnership: { ownerId: null, isOwner: false },
      loggedIn: false,
    } as PersistenceContext<PersistentModel>,
  }
  const testDispatch: EditorDispatch = (actions: ReadonlyArray<EditorAction>) => {
    capturedData.dispatchedActions.push(...actions)
    fastForEach(actions, (action) => {
      if (action.action === 'SET_PROJECT_ID') {
        capturedData.newProjectId = action.id
      } else if (action.action === 'UPDATE_FILE' && isAssetFile(action.file)) {
        capturedData.updatedFiles[action.filePath] = action.file
      }
    })
  }
  const onProjectNotFound = () => (capturedData.projectNotFound = true)
  const onCreatedOrLoadedProject = (
    _projectId: string,
    _projectName: string,
    createdOrLoadedProject: PersistentModel,
  ) => (capturedData.createdOrLoadedProject = createdOrLoadedProject)
  const onContextChange = (
    newContext: PersistenceContext<PersistentModel>,
    _oldContext: PersistenceContext<PersistentModel> | undefined,
  ) => (capturedData.latestContext = newContext)

  const testMachine = new PersistenceMachine(
    PersistenceBackend,
    testDispatch,
    onProjectNotFound,
    onCreatedOrLoadedProject,
    onContextChange,
    saveThrottle,
  )
  return {
    capturedData: capturedData,
    testDispatch: testDispatch,
    testMachine: testMachine,
  }
}

describe('Saving', () => {
  it('Saves locally when logged out', async () => {
    const { capturedData, testMachine } = setupTest()

    testMachine.createNew(ProjectName, BaseModel)
    await delay(20)
    testMachine.save(ProjectName, FirstRevision, 'force')
    await delay(20)
    testMachine.stop()

    expect(capturedData.newProjectId).toBeDefined()
    expect(mockSaveLog[capturedData.newProjectId!]).toBeUndefined()
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]).toBeDefined()
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]!.model).toEqual(FirstRevision)
  })

  it('Saves to server when logged in', async () => {
    const { capturedData, testMachine } = setupTest()

    testMachine.login()
    await delay(20)
    testMachine.createNew(ProjectName, BaseModel)
    await delay(20)
    testMachine.save(ProjectName, FirstRevision, 'force')
    await delay(20)
    testMachine.stop()

    expect(localProjects[localProjectKey(capturedData.newProjectId!)]).toBeUndefined()
    expect(capturedData.newProjectId).toBeDefined()
    expect(mockSaveLog[capturedData.newProjectId!]).toEqual([BaseModel, FirstRevision])
  })

  it('Throttles saves', async () => {
    const { capturedData, testMachine } = setupTest(10000)

    testMachine.login()
    await delay(20)
    testMachine.createNew(ProjectName, BaseModel)
    await delay(20)
    testMachine.save(ProjectName, FirstRevision, 'throttle') // Should be throttled and replaced by the next save
    await delay(20)
    testMachine.save(ProjectName, SecondRevision, 'throttle') // Should be throttled and replaced by the next save
    await delay(20)
    testMachine.save(ProjectName, ThirdRevision, 'throttle')
    await delay(20)
    testMachine.sendThrottledSave()
    await delay(20)
    testMachine.save(ProjectName, FourthRevision, 'throttle') // Should be throttled and won't save before the end of the test
    await delay(20)
    testMachine.stop()

    expect(localProjects[localProjectKey(capturedData.newProjectId!)]).toBeUndefined()
    expect(capturedData.newProjectId).toBeDefined()
    expect(mockSaveLog[capturedData.newProjectId!]).toEqual([BaseModel, ThirdRevision])
  })

  it('Does not throttle forced saves', async () => {
    const { capturedData, testMachine } = setupTest(10000)

    testMachine.login()
    await delay(20)
    testMachine.createNew(ProjectName, BaseModel)
    await delay(20)
    testMachine.save(ProjectName, FirstRevision, 'force')
    await delay(20)
    testMachine.save(ProjectName, SecondRevision, 'force')
    await delay(20)
    testMachine.save(ProjectName, ThirdRevision, 'force')
    await delay(20)
    testMachine.save(ProjectName, FourthRevision, 'force')
    await delay(20)
    testMachine.stop()

    expect(localProjects[localProjectKey(capturedData.newProjectId!)]).toBeUndefined()
    expect(capturedData.newProjectId).toBeDefined()
    expect(mockSaveLog[capturedData.newProjectId!]).toEqual([
      BaseModel,
      FirstRevision,
      SecondRevision,
      ThirdRevision,
      FourthRevision,
    ])
  })

  xit('Forks the project when not the owner', async () => {
    const { capturedData, testMachine } = setupTest(10000)

    testMachine.login()
    await delay(20)

    const startProjectId = mockRandomProjectID()
    serverProjects[startProjectId] = BaseModel
    mockUnownedProjects.add(startProjectId)

    testMachine.load(startProjectId)
    await delay(20)

    expect(capturedData.projectNotFound).toBeFalsy()
    expect(capturedData.createdOrLoadedProject).toEqual(BaseModel)

    testMachine.save(ProjectName, FirstRevision, 'force')
    await delay(20)
    testMachine.stop()

    const forkedProjectId = capturedData.newProjectId!
    expect(forkedProjectId).not.toEqual(startProjectId)
    expect(mockSaveLog[forkedProjectId]).toEqual([FirstRevision])
    expect(
      capturedData.dispatchedActions.some(
        (action) => action.action === 'SET_FORKED_FROM_PROJECT_ID' && action.id === startProjectId,
      ),
    ).toBeTruthy()
  })

  xit('Forks the project when not the owner, rolling back on a failed save', async () => {
    const { capturedData, testMachine } = setupTest(10000)

    testMachine.login()
    await delay(20)

    const startProjectId = mockRandomProjectID()
    serverProjects[startProjectId] = BaseModel
    mockUnownedProjects.add(startProjectId)

    testMachine.load(startProjectId)
    await delay(20)

    const dispatchedActionsBeforeForkCount = capturedData.dispatchedActions.length

    expect(capturedData.projectNotFound).toBeFalsy()
    expect(capturedData.createdOrLoadedProject).toEqual(BaseModel)

    // Check that the rollback values have been set
    expect(capturedData.latestContext.rollbackProjectId).toEqual(startProjectId)
    expect(capturedData.latestContext.rollbackProject).toEqual({
      name: ProjectName,
      content: BaseModel,
    })

    // Deliberately fail to fork the project
    const forkFailureProjectID = 'ForkFailureProject'
    forcedNextProjectId = forkFailureProjectID
    mockProjectsToError.add(forcedNextProjectId)

    testMachine.save(ProjectName, FirstRevision, 'force')
    await delay(20)

    // Check that the fork failed and only a toast was dispatched
    expect(capturedData.newProjectId).toBeUndefined()
    expect(mockSaveLog[startProjectId]).toBeUndefined()
    expect(mockSaveLog[forkFailureProjectID]).toBeUndefined()
    expect(capturedData.dispatchedActions.length - dispatchedActionsBeforeForkCount).toEqual(1)
    expect(capturedData.dispatchedActions.at(-1)!.action).toEqual('ADD_TOAST')

    // Check that the rollback values were applied and unchanged
    expect(capturedData.latestContext.projectId).toEqual(startProjectId)
    expect(capturedData.latestContext.project).toEqual({
      name: ProjectName,
      content: BaseModel,
    })
    expect(capturedData.latestContext.rollbackProjectId).toEqual(startProjectId)
    expect(capturedData.latestContext.rollbackProject).toEqual({
      name: ProjectName,
      content: BaseModel,
    })

    // Now successfully fork the project
    const forkSuccessProjectID = 'ForkSuccessProject'
    forcedNextProjectId = forkSuccessProjectID

    testMachine.save(ProjectName, SecondRevision, 'force')
    await delay(20)
    testMachine.stop()

    const forkedFromActions = capturedData.dispatchedActions.filter(
      (action) => action.action === 'SET_FORKED_FROM_PROJECT_ID',
    )
    const setNameActions = capturedData.dispatchedActions.filter(
      (action) => action.action === 'SET_PROJECT_NAME',
    )

    // Ensure that it was forked with the correct project model, project ID, and that the name was updated only once
    const forkedProjectName = `${ProjectName} (forked)`
    expect(capturedData.newProjectId).toEqual(forkSuccessProjectID)
    expect(mockSaveLog[forkSuccessProjectID]).toEqual([SecondRevision])
    expect(forkedFromActions.length).toBe(1)
    expect((forkedFromActions[0] as SetForkedFromProjectID).id).toEqual(startProjectId)
    expect(setNameActions.length).toBe(1)
    expect((setNameActions[0] as SetProjectName).name).toEqual(forkedProjectName)

    // Check that the rollback values were updated
    expect(capturedData.latestContext.rollbackProjectId).toEqual(forkSuccessProjectID)
    expect(capturedData.latestContext.rollbackProject).toEqual({
      name: forkedProjectName,
      content: SecondRevision,
    })
  })
})

describe('Login state', () => {
  // TODO Rheese and Balazs FIX THIS!
  xit('Logging in mid-session will switch to server saving and delete the local save', async () => {
    const { capturedData, testMachine } = setupTest()

    testMachine.createNew(ProjectName, BaseModel)
    await delay(20)

    // Check it was saved locally only
    expect(capturedData.newProjectId).toBeDefined()
    expect(mockSaveLog[capturedData.newProjectId!]).toBeUndefined()
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]).toBeDefined()
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]!.model).toEqual(BaseModel)

    testMachine.login()
    await delay(20)

    // Check that logging in uploaded it and deleted the local version
    expect(mockSaveLog[capturedData.newProjectId!]).toEqual([BaseModel])
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]).toBeUndefined()

    testMachine.save(ProjectName, FirstRevision, 'force')
    await delay(20)
    testMachine.stop()

    // Check that future saves go to the server
    expect(mockSaveLog[capturedData.newProjectId!]).toEqual([BaseModel, FirstRevision])
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]).toBeUndefined()
  })

  it('Logging out mid-session will switch to local saving', async () => {
    const { capturedData, testMachine } = setupTest()

    testMachine.login()
    await delay(20)
    testMachine.createNew(ProjectName, BaseModel)
    await delay(20)

    // Check it was saved to the server
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]).toBeUndefined()
    expect(capturedData.newProjectId).toBeDefined()
    expect(mockSaveLog[capturedData.newProjectId!]).toEqual([BaseModel])

    testMachine.logout()
    await delay(20)

    testMachine.save(ProjectName, FirstRevision, 'force')
    await delay(20)
    testMachine.stop()

    // Check it was saved locally
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]).toBeDefined()
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]!.model).toEqual(FirstRevision)
    expect(mockSaveLog[capturedData.newProjectId!]).toEqual([BaseModel]) // Should be no new saves
  })
})

describe('Loading a project', () => {
  function addServerProject(id: string, model: PersistentModel) {
    serverProjects[id] = model
  }

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

  it('Loads a server project', async () => {
    const { capturedData, testMachine } = setupTest()

    const projectId = mockRandomProjectID()
    addServerProject(projectId, BaseModel)

    testMachine.load(projectId)
    await delay(20)

    expect(capturedData.projectNotFound).toBeFalsy()
    expect(capturedData.createdOrLoadedProject).toEqual(BaseModel)
  })

  // TODO Rheese and Balazs FIX THIS!
  xit('Loads a local project', async () => {
    const { capturedData, testMachine } = setupTest()

    const projectId = mockRandomProjectID()
    addLocalProject(projectId, BaseModel)

    testMachine.load(projectId)
    await delay(20)

    expect(capturedData.projectNotFound).toBeFalsy()
    expect(capturedData.createdOrLoadedProject).toEqual(BaseModel)
  })

  // TODO Rheese and Balazs fix this
  xit('Favours a local project over a server project', async () => {
    const { capturedData, testMachine } = setupTest()
    const serverProject = BaseModel
    const localProject = updateModel(BaseModel)

    const projectId = mockRandomProjectID()
    addServerProject(projectId, serverProject)
    addLocalProject(projectId, localProject)

    testMachine.load(projectId)
    await delay(40)

    expect(capturedData.projectNotFound).toBeFalsy()
    expect(capturedData.createdOrLoadedProject).toEqual(localProject)
  })
})

describe('Forking a project', () => {
  const AssetFileName = 'asset.xyz'
  const startProject: PersistentModel = {
    ...BaseModel,
    projectContents: addFileToProjectContents(
      BaseModel.projectContents,
      AssetFileName,
      AssetFileWithoutBase64,
    ),
  }
  const startProjectIncludingBase64: PersistentModel = {
    ...BaseModel,
    projectContents: addFileToProjectContents(
      BaseModel.projectContents,
      AssetFileName,
      mockAssetFileWithBase64,
    ),
  }

  it('Downloads the base 64 for assets and uploads them against the new project id if the user is signed in', async () => {
    const { capturedData, testMachine } = setupTest()

    // Create the initial project
    testMachine.login()
    await delay(20)
    testMachine.createNew(ProjectName, startProject)
    await delay(20)

    expect(capturedData.newProjectId).toBeDefined()
    const startProjectId = capturedData.newProjectId!

    testMachine.fork(ProjectName, startProject)
    await delay(20)
    testMachine.stop()

    const forkedProjectId = capturedData.newProjectId!
    expect(forkedProjectId).not.toEqual(startProjectId)
    expect(mockDownloadedAssetsLog[startProjectId]).toEqual([AssetFileName])
    expect(mockUploadedAssetsLog[forkedProjectId]).toEqual([AssetFileName])
    expect(mockSaveLog[forkedProjectId]).toEqual([startProject])
    expect(capturedData.updatedFiles[AssetFileName]).toEqual(AssetFileWithoutBase64)
    expect(
      capturedData.dispatchedActions.some(
        (action) => action.action === 'SET_FORKED_FROM_PROJECT_ID' && action.id === startProjectId,
      ),
    ).toBeTruthy()
  })

  it('Downloads the base 64 for assets and stores them in the project if the user is not signed in', async () => {
    const { capturedData, testMachine } = setupTest()

    // Create the initial project
    testMachine.login()
    await delay(20)
    testMachine.createNew(ProjectName, startProject)
    await delay(20)

    expect(capturedData.newProjectId).toBeDefined()
    const startProjectId = capturedData.newProjectId!

    testMachine.logout()
    await delay(20)
    testMachine.fork(ProjectName, startProject)
    await delay(20)
    testMachine.stop()

    const forkedProjectId = capturedData.newProjectId!
    expect(forkedProjectId).not.toEqual(startProjectId)
    expect(mockDownloadedAssetsLog[startProjectId]).toEqual([AssetFileName])
    expect(mockUploadedAssetsLog[forkedProjectId]).toBeUndefined()
    expect(mockSaveLog[forkedProjectId]).toBeUndefined()
    expect(localProjects[localProjectKey(forkedProjectId)]).toBeDefined()
    expect(localProjects[localProjectKey(forkedProjectId)]!.model).toEqual({
      ...startProject,
      projectContents: addFileToProjectContents(
        startProject.projectContents,
        AssetFileName,
        assetFile(base64Contents, gitBlobChecksumFromBase64(base64Contents)),
      ),
    })
    expect(capturedData.updatedFiles[AssetFileName]).toEqual(mockAssetFileWithBase64)
    expect(
      capturedData.dispatchedActions.some(
        (action) => action.action === 'SET_FORKED_FROM_PROJECT_ID' && action.id === startProjectId,
      ),
    ).toBeTruthy()
  })

  it('Does not download or upload anything if the original project constains the base 64 and the user is not signed in', async () => {
    const { capturedData, testMachine } = setupTest()

    // Create the initial project
    await delay(20)
    testMachine.createNew(ProjectName, startProjectIncludingBase64)
    await delay(20)

    expect(capturedData.newProjectId).toBeDefined()
    const startProjectId = capturedData.newProjectId!

    await delay(20)
    testMachine.fork(ProjectName, startProjectIncludingBase64)
    await delay(20)
    testMachine.stop()

    const forkedProjectId = capturedData.newProjectId!
    expect(forkedProjectId).not.toEqual(startProjectId)
    expect(mockDownloadedAssetsLog[startProjectId]).toEqual([])
    expect(mockUploadedAssetsLog[forkedProjectId]).toBeUndefined()
    expect(mockSaveLog[forkedProjectId]).toBeUndefined()
    expect(localProjects[localProjectKey(forkedProjectId)]).toBeDefined()
    expect(localProjects[localProjectKey(capturedData.newProjectId!)]!.model).toEqual(
      startProjectIncludingBase64,
    )
    expect(capturedData.updatedFiles[AssetFileName]).toEqual(mockAssetFileWithBase64)
    expect(
      capturedData.dispatchedActions.some(
        (action) => action.action === 'SET_FORKED_FROM_PROJECT_ID' && action.id === startProjectId,
      ),
    ).toBeTruthy()
  })
})
