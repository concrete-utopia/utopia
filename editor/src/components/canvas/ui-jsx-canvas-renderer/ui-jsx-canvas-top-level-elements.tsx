import { useContextSelector } from 'use-context-selector'
import { TopLevelElement } from '../../../core/shared/element-template'
import { Imports } from '../../../core/shared/project-file-types'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { getParseSuccessOrTransientForFilePath } from '../canvas-utils'
import { UtopiaProjectContext } from './ui-jsx-canvas-contexts'

export function useGetTopLevelElementsAndImports(
  filePath: string | null,
): { topLevelElements: TopLevelElement[]; imports: Imports } {
  return useContextSelector(UtopiaProjectContext, (c) => {
    if (filePath == null) {
      return { topLevelElements: [], imports: emptyImports() }
    } else {
      const success = getParseSuccessOrTransientForFilePath(
        filePath,
        c.projectContents,
        c.transientFilesState,
      )
      return {
        topLevelElements: success.topLevelElements,
        imports: success.imports,
      }
    }
  })
}
