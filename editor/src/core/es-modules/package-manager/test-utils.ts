import {
  isPackagerServerFileEntry,
  type PackagerServerResponse,
} from '../../shared/npm-dependency-types'
import type { NodeModules } from '../../shared/project-file-types'
import { esCodeFile, esRemoteDependencyPlaceholder } from '../../shared/project-file-types'

export function createNodeModules(contents: PackagerServerResponse['contents']): NodeModules {
  return contents.reduce((workingResult, entry) => {
    if (isPackagerServerFileEntry(entry)) {
      if (entry.fileEntry.fileContents === 'PLACEHOLDER_FILE') {
        return {
          ...workingResult,
          [entry.fileEntry.filename]: esRemoteDependencyPlaceholder('', false),
        }
      } else {
        return {
          ...workingResult,
          [entry.fileEntry.filename]: esCodeFile(
            entry.fileEntry.fileContents.content,
            'NODE_MODULES',
            entry.fileEntry.filename,
          ),
        }
      }
    } else {
      return workingResult
    }
  }, {})
}
