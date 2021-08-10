import { useContextSelector } from 'use-context-selector'
import { TopLevelElement } from '../../../core/shared/element-template'
import { Imports } from '../../../core/shared/project-file-types'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { getParseSuccessOrTransientForFilePath } from '../canvas-utils'
import { UtopiaProjectContextData } from './ui-jsx-canvas-contexts'

export function getTopLevelElementsAndImports(
  filePath: string | null,
  utopiaProjectContext_FIXME_RERENDER: UtopiaProjectContextData,
): { topLevelElements: TopLevelElement[]; imports: Imports } {
  if (filePath == null) {
    return { topLevelElements: [], imports: emptyImports() }
  } else {
    const success = getParseSuccessOrTransientForFilePath(
      filePath,
      utopiaProjectContext_FIXME_RERENDER.projectContents,
      utopiaProjectContext_FIXME_RERENDER.transientFilesState,
    )
    return {
      topLevelElements: success.topLevelElements,
      imports: success.imports,
    }
  }
}
