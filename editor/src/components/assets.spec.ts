import * as FastCheck from 'fast-check'
import fastDeepEquals from 'fast-deep-equal'
import type { ProjectContents } from '../core/shared/project-file-types'
import {
  directory,
  isDirectory,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../core/shared/project-file-types'
import { codeFile } from '../core/shared/project-file-types'
import type { ProjectContentTreeRoot } from './assets'
import {
  contentsToTree,
  getProjectDependencies,
  projectContentDirectory,
  projectContentFile,
  treeToContents,
} from './assets'
import { projectContentsArbitrary } from './assets.test-utils'
import { simpleDefaultProject } from '../sample-projects/sample-project-utils'

function checkContentsToTree(contents: ProjectContents): boolean {
  const result = treeToContents(contentsToTree(contents))
  let withoutNewKeys = {
    ...result,
  }
  for (const key of Object.keys(result)) {
    if (!(key in contents)) {
      const newKeyContent = result[key]
      if (!isDirectory(newKeyContent)) {
        return false
      }
      delete withoutNewKeys[key]
    }
  }
  const withoutNewKeysIsTheSame = fastDeepEquals(withoutNewKeys, contents)
  return withoutNewKeysIsTheSame
}

describe('contentsToTree', () => {
  it('Cycling from ProjectContents and back again should produce the same value with added directories', () => {
    const arbitrary = projectContentsArbitrary()
    const prop = FastCheck.property(arbitrary, checkContentsToTree)
    FastCheck.assert(prop, { verbose: true })
  })
  it('Specific case to test for directory creation', () => {
    const contentTextFile = codeFile('{} + []', null)
    const contents: ProjectContents = {
      '/a/b/c': contentTextFile,
    }
    const expectedResult: ProjectContentTreeRoot = {
      a: projectContentDirectory('/a', directory(), {
        b: projectContentDirectory('/a/b', directory(), {
          c: projectContentFile('/a/b/c', contentTextFile),
        }),
      }),
    }
    expect(contentsToTree(contents)).toEqual(expectedResult)
  })
})

describe('getProjectDependencies', () => {
  it('should merge the dependencies from package.json and package-lock.json', () => {
    const project = simpleDefaultProject({
      additionalFiles: {
        '/package-lock.json': textFile(
          textFileContents(
            JSON.stringify(
              {
                dependencies: {
                  react: {
                    version: '1.0.0',
                  },
                },
              },
              null,
              2,
            ),
            unparsed,
            RevisionsState.CodeAhead,
          ),
          null,
          null,
          0,
        ),
      },
    })
    const dependencies = getProjectDependencies(project.projectContents)
    // from package.json
    expect(dependencies?.['react-dom']).toEqual('16.13.1')
    // from package-lock.json
    expect(dependencies?.react).toEqual('1.0.0')
  })
})
