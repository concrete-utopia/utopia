jest.mock('./server', () => ({
  updateSavedProject: async (
    projectId: string,
    persistentModel: PersistentModel | null,
    name: string | null,
  ) => {
    await delay(200)
    saveLog.push({
      projectId: projectId,
      name: name,
    })
  },
}))
jest.mock('../../common/server', () => ({
  checkProjectOwnership: async (projectId: string) => ({
    isOwner: true,
  }),
}))
jest.setTimeout(10000)

import {
  PersistentModel,
  createEditorState,
  persistentModelFromEditorModel,
} from './store/editor-state'
import { saveToServer, createNewProject } from './persistence'
import { NO_OP } from '../../core/shared/utils'
import { DeleteAllLayoutSystemConfigButton } from '../inspector/sections/layout-section/layout-system-subsection/layout-system-controls'

let saveLog: Array<{ projectId: string; name: string | null }> = []

const delay = (time: number) => new Promise((resolve) => setTimeout(resolve, time))

describe('Throttled save functionality', () => {
  it('Timed save sequence', () => {
    // saveLog = []
    // const model = persistentModelFromEditorModel(createEditorState())
    // createNewProject(NO_OP, NO_OP)
    // // Immediately save project
    // await saveToServer(NO_OP, 'projectid', model, 'projectname1')
    // // Save should be successful
    // expect(saveLog).toHaveLength(1)
    // // Try to save quickly repeatedly
    // await saveToServer(NO_OP, 'projectid', model, 'projectname2')
    // await saveToServer(NO_OP, 'projectid', model, 'projectname3')
    // // The last 2 save requests should be blocked by throttling
    // expect(saveLog).toHaveLength(1)
    // //another save should automatically happen after a delay, because there were throttled saves
    // await delay(1000)
    // expect(saveLog).toHaveLength(2)
    // //this save should save the most recent content
    // expect(saveLog[1].name).toEqual('projectname3')
    // // Try to save after waiting enough time
    // await delay(1000)
    // await saveToServer(NO_OP, 'projectid', model, 'projectname3')
    // // The save should immediately happen
    // expect(saveLog).toHaveLength(3)
    // await delay(1000)
    // // Do not save when previous save is in progress
    // saveToServer(NO_OP, 'projectid', model, 'projectname3').then(() => {
    //   expect(saveLog).toHaveLength(4)
    //   done()
    // })
    // // this save is requested while the previous is in progress and blocked
    // saveToServer(NO_OP, 'projectid', model, 'projectname3')
  })
})
