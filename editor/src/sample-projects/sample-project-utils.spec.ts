import { defaultProject } from './sample-project-utils'
import * as fs from 'fs'

describe('Project model integrity check', () => {
  it('ensures that the server tests use the most recent project model structure', () => {
    const stringifiedProject = JSON.stringify(defaultProject())
    fs.writeFileSync('../server/test/Test/Utopia/Web/SampleProject.json', stringifiedProject)
  })
})
