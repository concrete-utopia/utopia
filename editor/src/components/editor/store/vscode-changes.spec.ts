import type { ProjectChanges } from './vscode-changes'
import {
  combineProjectChanges,
  deletePathChange,
  ensureDirectoryExistsChange,
  writeProjectFileChange,
} from './vscode-changes'
import { codeFile } from '../../../core/shared/project-file-types'

describe('combineAccumulatedVSCodeChanges', () => {
  it('For multiple writes to a single file, strips all but the final result', () => {
    const ensureDirectoryExists1 = ensureDirectoryExistsChange('/src')
    const deletePath1 = deletePathChange('/src/thing.ts', false)
    const fileWrite1 = writeProjectFileChange('/src/stuff.ts', codeFile('1', null))
    const ensureDirectoryExists2 = ensureDirectoryExistsChange('/src/other')
    const deletePath2 = deletePathChange('/src/other/other-thing.ts', false)
    const fileWrite2 = writeProjectFileChange('/src/stuff.ts', codeFile('2', null))
    const fileWrite3 = writeProjectFileChange('/src/stuff.ts', codeFile('3', null))

    const first: ProjectChanges = {
      fileChanges: {
        collabProjectChanges: [ensureDirectoryExists1, deletePath1, fileWrite1],
        changesForVSCode: [ensureDirectoryExists1, deletePath1, fileWrite1],
      },
      updateDecorations: null,
      selectedChanged: null,
    }

    const second: ProjectChanges = {
      fileChanges: {
        collabProjectChanges: [ensureDirectoryExists2, deletePath2, fileWrite2, fileWrite3],
        changesForVSCode: [ensureDirectoryExists2, deletePath2, fileWrite2, fileWrite3],
      },
      updateDecorations: null,
      selectedChanged: null,
    }

    const result = combineProjectChanges(first, second)
    expect(result).toEqual({
      fileChanges: {
        collabProjectChanges: [
          ensureDirectoryExists1,
          deletePath1,
          ensureDirectoryExists2,
          deletePath2,
          fileWrite3,
        ],
        changesForVSCode: [
          ensureDirectoryExists1,
          deletePath1,
          ensureDirectoryExists2,
          deletePath2,
          fileWrite3,
        ],
      },
      updateDecorations: null,
      selectedChanged: null,
    })
  })
})
