import * as FastCheck from 'fast-check'
import type {
  TextFile,
  Directory,
  ImageFile,
  ProjectContents,
  ProjectFile,
  AssetFile,
} from '../core/shared/project-file-types'
import { directory, imageFile, isDirectory } from '../core/shared/project-file-types'
import { codeFile, assetFile } from '../core/shared/project-file-types'
import type {
  ProjectContentDirectory,
  ProjectContentFile,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from './assets'
import { projectContentDirectory, projectContentFile } from './assets'

export function codeFileArbitrary(): FastCheck.Arbitrary<TextFile> {
  return FastCheck.string().map((content) => codeFile(content, null))
}

export function remoteImageFileArbitrary(): FastCheck.Arbitrary<ImageFile> {
  return FastCheck.constant(imageFile(undefined, undefined, 100, 100, 123456, 'aaaaaaa'))
}

export function localImageFileArbitrary(): FastCheck.Arbitrary<ImageFile> {
  return FastCheck.tuple(FastCheck.string(), FastCheck.string()).map(([imageType, base64]) =>
    imageFile(imageType, base64, 100, 100, 123456, 'bbbbbbbb'),
  )
}

export function assetFileArbitrary(): FastCheck.Arbitrary<AssetFile> {
  return FastCheck.tuple(
    FastCheck.oneof<string | undefined>(FastCheck.constant(undefined), FastCheck.asciiString()),
    FastCheck.oneof<string | undefined>(FastCheck.constant(undefined), FastCheck.asciiString()),
  ).map(([base64, gitBlobSha]) => {
    return assetFile(base64, gitBlobSha)
  })
}

export function notDirectoryContentFileArbitrary(): FastCheck.Arbitrary<
  TextFile | ImageFile | AssetFile
> {
  return FastCheck.oneof<TextFile | ImageFile | AssetFile>(
    codeFileArbitrary(),
    remoteImageFileArbitrary(),
    localImageFileArbitrary(),
    assetFileArbitrary(),
  )
}

export function directoryContentFileArbitrary(): FastCheck.Arbitrary<Directory> {
  return FastCheck.constant(directory())
}

export function pathArbitrary(): FastCheck.Arbitrary<string> {
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

export function projectContentsArbitrary(): FastCheck.Arbitrary<ProjectContents> {
  return FastCheck.dictionary<ProjectFile>(
    pathArbitrary(),
    FastCheck.oneof<ProjectFile>(
      notDirectoryContentFileArbitrary(),
      directoryContentFileArbitrary(),
    ),
  ).filter(projectContentsFilter)
}

export function projectContentFileArbitrary(): FastCheck.Arbitrary<ProjectContentFile> {
  return FastCheck.tuple(pathArbitrary(), notDirectoryContentFileArbitrary()).map(
    ([fullPath, content]) => {
      return projectContentFile(fullPath, content)
    },
  )
}

export function projectContentDirectoryArbitrary(
  depth: number = 3,
): FastCheck.Arbitrary<ProjectContentDirectory> {
  return FastCheck.tuple(
    pathArbitrary(),
    directoryContentFileArbitrary(),
    projectContentTreeRootArbitrary(depth - 1),
  ).map(([fullPath, dir, children]) => {
    return projectContentDirectory(fullPath, dir, children)
  })
}

export function projectContentTreeRootArbitrary(
  depth: number = 3,
): FastCheck.Arbitrary<ProjectContentTreeRoot> {
  return FastCheck.dictionary<ProjectContentsTree>(
    pathArbitrary(),
    projectContentsTreeArbitrary(depth),
  )
}

export function projectContentsTreeArbitrary(
  depth: number = 3,
): FastCheck.Arbitrary<ProjectContentsTree> {
  if (depth > 1) {
    return FastCheck.oneof<ProjectContentsTree>(
      projectContentDirectoryArbitrary(depth),
      projectContentFileArbitrary(),
    )
  } else {
    return projectContentFileArbitrary()
  }
}
