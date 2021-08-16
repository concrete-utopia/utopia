import { usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { TopLevelElement } from '../../../core/shared/element-template'
import { Imports } from '../../../core/shared/project-file-types'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { getParseSuccessOrTransientForFilePath } from '../canvas-utils'
import { UtopiaProjectCtxAtom } from './ui-jsx-canvas-contexts'

const emptyResult = { topLevelElements: [], imports: emptyImports() }

export function useGetTopLevelElementsAndImports(
  filePath: string | null,
): { topLevelElements: TopLevelElement[]; imports: Imports } {
  const projectContext = usePubSubAtomReadOnly(UtopiaProjectCtxAtom) // TODO MAYBE create a usePubSubAtomSelector
  if (filePath == null) {
    return emptyResult
  } else {
    const success = getParseSuccessOrTransientForFilePath(
      filePath,
      projectContext.projectContents,
      projectContext.transientFilesState,
    )
    return {
      topLevelElements: success.topLevelElements,
      imports: success.imports,
    }
  }
}
