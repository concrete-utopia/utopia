import { usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import type { TopLevelElement } from '../../../core/shared/element-template'
import type { Imports } from '../../../core/shared/project-file-types'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { getParseSuccessForFilePath } from '../canvas-utils'
import { UtopiaProjectCtxAtom } from './ui-jsx-canvas-contexts'

const emptyResult = { topLevelElements: [], imports: emptyImports() }

export function useGetTopLevelElementsAndImports(
  filePath: string | null,
  shouldUpdateCallback: () => boolean,
): {
  topLevelElements: TopLevelElement[]
  imports: Imports
} {
  const projectContext = usePubSubAtomReadOnly(UtopiaProjectCtxAtom, shouldUpdateCallback) // TODO MAYBE create a usePubSubAtomSelector
  if (filePath == null) {
    return emptyResult
  } else {
    const success = getParseSuccessForFilePath(filePath, projectContext.projectContents)
    return {
      topLevelElements: success.topLevelElements,
      imports: success.imports,
    }
  }
}
