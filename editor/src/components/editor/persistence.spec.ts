import { PersistentModel } from './store/editor-state'
import { saveToServer, setBaseSaveWaitTime, clearSaveState } from './persistence'
import { NO_OP } from '../../core/shared/utils'
import { createPersistentModel, delay } from '../../utils/test-utils'
import { generateUID } from '../../core/shared/uid-utils'
import { UIJSFile } from '../../core/shared/project-file-types'
import { SaveProjectResponse } from './server'

let saveLog: { [key: string]: Array<PersistentModel> } = {}
let projectsToError: Set<string> = new Set<string>()

jest.mock('./server', () => ({
  updateSavedProject: async (
    projectId: string,
    persistentModel: PersistentModel | null,
    name: string | null,
  ): Promise<SaveProjectResponse> => {
    await delay(10)
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
}))

jest.mock('../../common/server', () => ({
  checkProjectOwnership: async (projectId: string) => ({
    isOwner: true,
  }),
}))
jest.setTimeout(10000)

let allProjectIds: Array<string> = []
function randomProjectID(): string {
  const newId = generateUID(allProjectIds)
  allProjectIds.push(newId)
  return newId
}

const ProjectName = 'Project Name'
const ModelChange = createPersistentModel()

export function updateModel(model: PersistentModel): PersistentModel {
  return {
    ...model,
    projectContents: {
      ...model.projectContents,
      '/src/app.js': {
        ...model.projectContents['/src/app.js'],
        lastRevisedTime: Date.now(),
      } as UIJSFile,
    },
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
      saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
      saveToServer(NO_OP, projectId, ProjectName, secondRevision, null, true)
      await delay(40)
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, secondRevision])
    })

    it('Queues the save to be immediately triggered if a save is in progress', async () => {
      clearSaveState()
      setBaseSaveWaitTime(1000)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, true)
      await delay(40)
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, firstRevision])
    })

    it('Forces the next queued save if the original forced save is still queued', async () => {
      clearSaveState()
      setBaseSaveWaitTime(1000)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      const secondRevision = updateModel(ModelChange)
      saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, true)
      saveToServer(NO_OP, projectId, ProjectName, secondRevision, null, false)
      await delay(40)
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, secondRevision])
    })

    it('Does not affect future save throttling after being saved', async () => {
      clearSaveState()
      setBaseSaveWaitTime(1000)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      const secondRevision = updateModel(ModelChange)
      saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, true)
      await delay(40)
      saveToServer(NO_OP, projectId, ProjectName, secondRevision, null, false)
      await delay(40)
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, firstRevision])
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
      saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      await delay(40)
      saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
      await delay(40)
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, firstRevision])
    })

    it('Sets a timeout when not past the threshold', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      const firstRevision = updateModel(ModelChange)
      saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      await delay(10)
      saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
      await delay(10)
      expect(saveLog[projectId].length).toEqual(1)
      expect(saveLog[projectId]).toEqual([ModelChange])
      await delay(20)
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, firstRevision])
    })

    it('Does not spam the server if there was an error during saving', async () => {
      clearSaveState()
      setBaseSaveWaitTime(10)
      const projectId = randomProjectID()
      projectsToError.add(projectId)
      saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
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
      saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      await delay(20)
      expect(saveLog[projectId]).toBeUndefined()
      saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
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
      saveToServer(NO_OP, projectId, ProjectName, ModelChange, null, false)
      saveToServer(NO_OP, projectId, ProjectName, firstRevision, null, false)
      saveToServer(NO_OP, projectId, ProjectName, secondRevision, null, false)
      await delay(20)
      expect(saveLog[projectId].length).toEqual(1)
      expect(saveLog[projectId]).toEqual([ModelChange])
      await delay(40)
      expect(saveLog[projectId].length).toEqual(2)
      expect(saveLog[projectId]).toEqual([ModelChange, secondRevision])
    })
  })
})
