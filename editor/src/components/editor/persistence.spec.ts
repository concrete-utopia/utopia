import { PersistentModel, StoryboardFilePath } from './store/editor-state'
import {
  saveToServer,
  setBaseSaveWaitTime,
  clearSaveState,
  LocalProject,
  loadFromLocalStorage,
} from './persistence'
import { NO_OP } from '../../core/shared/utils'
import { createPersistentModel, delay } from '../../utils/utils.test-utils'
import { generateUID } from '../../core/shared/uid-utils'
import { TextFile } from '../../core/shared/project-file-types'
import { SaveProjectResponse } from './server'
import { localProjectKey } from '../../common/persistence'
import { MockUtopiaTsWorkers } from '../../core/workers/workers'
import { addFileToProjectContents, getContentsTreeFileFromString } from '../assets'
import { forceNotNull } from '../../core/shared/optional-utils'

let saveLog: { [key: string]: Array<PersistentModel> } = {}
let projectsToError: Set<string> = new Set<string>()

jest.mock('./server', () => ({
  updateSavedProject: async (
    projectId: string,
    persistentModel: PersistentModel | null,
    name: string | null,
  ): Promise<SaveProjectResponse> => {
    if (projectsToError.has(projectId)) {
      return Promise.reject(`Deliberately failing for ${projectId}`)
    }

    if (persistentModel != null) {
      let currentLog = saveLog[projectId] ?? []
      currentLog.push(persistentModel)
      saveLog[projectId] = currentLog
    }

    return Promise.resolve({ id: projectId, ownerId: 'Owner' })
  },
  saveImagesFromProject: async (projectId: string, persistentModel: PersistentModel) => {
    return Promise.resolve(persistentModel)
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
      expect(saveLog[projectId]).toEqual([ModelChange])
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
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, secondRevision])
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
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, firstRevision])
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
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, secondRevision])
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
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, firstRevision])
      await nextSave
    })
  })

  describe('Throttled saving', () => {
    it('Saves to the server when the current state is never-saved', async () => {
      clearSaveState()
      const projectId = randomProjectID()
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      expect(saveLog[projectId]).toEqual([ModelChange])
    })

    it('Saves to the server when past the threshold', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      await saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
      await delay(20)
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, firstRevision])
    })

    it('Sets a timeout when not past the threshold', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      const save = saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
      expect(saveLog[projectId].length).toEqual(1)
      expect(saveLog[projectId]).toEqual([ModelChange])
      await save
      await delay(20)
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, firstRevision])
    })

    it('Does not spam the server if there was an error during saving', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      projectsToError.add(projectId)
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      await delay(20)
      expect(saveLog[projectId]).toBeUndefined()
      await delay(20)
      expect(saveLog[projectId]).toBeUndefined()
      projectsToError.delete(projectId)
      await delay(40)
      expect(saveLog[projectId].length).toEqual(1)
      expect(saveLog[projectId]).toEqual([ModelChange])
    })

    it('Replaces an errored save when a model change is made', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      projectsToError.add(projectId)
      await saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      await delay(20)
      expect(saveLog[projectId]).toBeUndefined()
      await saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
      await delay(20)
      expect(saveLog[projectId]).toBeUndefined()
      projectsToError.delete(projectId)
      await delay(40)
      expect(saveLog[projectId].length).toEqual(1)
      expect(saveLog[projectId]).toEqual([firstRevision])
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
      expect(saveLog[projectId].length).toEqual(1)
      expect(saveLog[projectId]).toEqual([ModelChange])
      await delay(40)
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, secondRevision])
      await Promise.all(saves)
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
    expect(saveLog[projectId].length).toEqual(1)
    expect(saveLog[projectId]).toEqual([ModelChange])
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
    expect(saveLog[projectId].length).toEqual(1)
    expect(saveLog[projectId]).toEqual([ModelChange])
    expect(localProjects[localProjectKey(projectId)]).toBeUndefined()
  })
})
