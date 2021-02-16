import * as BrowserFS from 'browserfs'
import { FSModule } from 'browserfs/dist/node/core/FS'
import { forceNotNull } from '../shared/optional-utils'
import { ProjectFile } from '../shared/project-file-types'
import { createDirectory, writeFile } from './common/browserfs-utils'

const Scheme = 'utopia'
const RootDir = `/${Scheme}`

export function toFSPath(projectID: string, path: string): string {
  const fsPath = `${RootDir}/${projectID}${path}`
  return fsPath
}

export function writeProjectFile(projectID: string, path: string, file: ProjectFile): void {
  switch (file.type) {
    case 'DIRECTORY':
      {
        createDirectory(toFSPath(projectID, path))
      }
      break
    case 'TEXT_FILE':
      {
        const fileContents = Buffer.from(file.fileContents.code, 'utf-8')
        writeFile(toFSPath(projectID, path), fileContents)
      }
      break
    case 'ASSET_FILE':
      throw new Error(`Can't handle asset file at ${path}`)
    case 'IMAGE_FILE':
      throw new Error(`Can't handle image file at ${path}`)
  }
}
