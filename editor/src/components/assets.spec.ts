import * as FastCheck from 'fast-check'
import * as R from 'ramda'
import { codeFile, directory, imageFile, isDirectory } from '../core/model/project-file-utils'
import {
  CodeFile,
  Directory,
  ImageFile,
  ProjectContents,
  ProjectFile,
} from '../core/shared/project-file-types'
import {
  contentsToTree,
  projectContentDirectory,
  projectContentFile,
  ProjectContentTreeRoot,
  treeToContents,
} from './assets'

function codeFileArbitrary(): FastCheck.Arbitrary<CodeFile> {
  return FastCheck.string().map((content) => codeFile(content, null))
}

function remoteImageFileArbitrary(): FastCheck.Arbitrary<ImageFile> {
  return FastCheck.constant(imageFile(undefined, undefined, 100, 100, 'hat'))
}

function localImageFileArbitrary(): FastCheck.Arbitrary<ImageFile> {
  return FastCheck.tuple(FastCheck.string(), FastCheck.string()).map(([imageType, base64]) =>
    imageFile(imageType, base64, 100, 100, 'hat'),
  )
}

function notDirectoryContentFileArbitrary(): FastCheck.Arbitrary<CodeFile | ImageFile> {
  return FastCheck.oneof<CodeFile | ImageFile>(
    codeFileArbitrary(),
    remoteImageFileArbitrary(),
    localImageFileArbitrary(),
  )
}

function directoryContentFileArbitrary(): FastCheck.Arbitrary<Directory> {
  return FastCheck.constant(directory())
}

function pathArbitrary(): FastCheck.Arbitrary<string> {
  return FastCheck.array(
    FastCheck.stringOf(
      FastCheck.char().filter((char) => char !== '/'),
      1,
      30,
    ),
    1,
    5,
  ).map((arr) => {
    return `/${arr.join('/')}`
  })
}

function projectContentsFilter(projectContents: ProjectContents): boolean {
  const allKeys = Object.keys(projectContents)
  for (const key of allKeys) {
    const pathElements = key.split('/')
    for (const size of R.range(1, pathElements.length)) {
      const subPath = pathElements.slice(0, size).join('/')
      const possibleContent = projectContents[subPath]
      if (possibleContent != null && !isDirectory(possibleContent)) {
        return false
      }
    }
  }
  return true
}

function projectContentsArbitrary(): FastCheck.Arbitrary<ProjectContents> {
  return FastCheck.dictionary<ProjectFile>(
    pathArbitrary(),
    FastCheck.oneof<ProjectFile>(
      notDirectoryContentFileArbitrary(),
      directoryContentFileArbitrary(),
    ),
  ).filter(projectContentsFilter)
}

function checkContentsToTree(contents: ProjectContents): boolean {
  const result = treeToContents(contentsToTree(contents))
  const newKeys = R.difference(Object.keys(result), Object.keys(contents))
  for (const newKey of newKeys) {
    const newKeyContent = result[newKey]
    if (!isDirectory(newKeyContent)) {
      return false
    }
  }
  const withoutNewKeys = R.omit(newKeys, result)
  const withoutNewKeysIsTheSame = R.equals(withoutNewKeys, contents)
  return withoutNewKeysIsTheSame
}

describe('contentsToTree', () => {
  it('Cycling from ProjectContents and back again should produce the same value with added directories', () => {
    const arbitrary = projectContentsArbitrary()
    const prop = FastCheck.property(arbitrary, checkContentsToTree)
    FastCheck.assert(prop, { verbose: true })
  })
  it('Specific case to test for directory creation', () => {
    const contentCodeFile = codeFile('{} + []', null)
    const contents: ProjectContents = {
      '/a/b/c': contentCodeFile,
    }
    const expectedResult: ProjectContentTreeRoot = {
      a: projectContentDirectory('/a', directory(), {
        b: projectContentDirectory('/a/b', directory(), {
          c: projectContentFile('/a/b/c', contentCodeFile),
        }),
      }),
    }
    expect(contentsToTree(contents)).toEqual(expectedResult)
  })
})
