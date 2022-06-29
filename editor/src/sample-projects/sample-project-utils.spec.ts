import { complexDefaultProject } from './sample-project-utils'
import * as fs from 'fs'

describe('Project model integrity check', () => {
  xit('ensures that the server tests use the most recent project model structure', () => {
    const stringifiedProject = JSON.stringify(complexDefaultProject())
    fs.writeFileSync('../server/test/Test/Utopia/Web/SampleProject.json', stringifiedProject)
  })
})
