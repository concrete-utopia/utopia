import * as FastCheck from 'fast-check'
import fastDeepEquals from 'fast-deep-equal'
import type { ProjectContents } from '../core/shared/project-file-types'
import { directory, isDirectory } from '../core/shared/project-file-types'
import { codeFile } from '../core/shared/project-file-types'
import type { ProjectContentTreeRoot } from './assets'
import {
  contentsToTree,
  projectContentDirectory,
  projectContentFile,
  treeToContents,
} from './assets'
import { projectContentsArbitrary } from './assets.test-utils'

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
