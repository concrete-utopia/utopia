import * as FastCheck from 'fast-check'
import { directory, imageFile, isDirectory } from '../core/model/project-file-utils'
import {
  TextFile,
  Directory,
  ImageFile,
  ProjectContents,
  ProjectFile,
  codeFile,
} from '../core/shared/project-file-types'
import {
  contentsToTree,
  projectContentDirectory,
  projectContentFile,
  ProjectContentTreeRoot,
  treeToContents,
} from './assets'
import * as fastDeepEquals from 'fast-deep-equal'

function codeFileArbitrary(): FastCheck.Arbitrary<TextFile> {
  return FastCheck.string().map((content) => codeFile(content, null))
}

function remoteImageFileArbitrary(): FastCheck.Arbitrary<ImageFile> {
  return FastCheck.constant(imageFile(undefined, undefined, 100, 100, 123456))
}

function localImageFileArbitrary(): FastCheck.Arbitrary<ImageFile> {
  return FastCheck.tuple(FastCheck.string(), FastCheck.string()).map(([imageType, base64]) =>
    imageFile(imageType, base64, 100, 100, 123456),
  )
}

function notDirectoryContentFileArbitrary(): FastCheck.Arbitrary<TextFile | ImageFile> {
  return FastCheck.oneof<TextFile | ImageFile>(
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
    for (let size = 1; size < pathElements.length; size++) {
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
