import { useContextSelector } from 'use-context-selector'
import { ArbitraryJSBlock, TopLevelElement } from '../../../core/shared/element-template'
import { Imports, isParseSuccess, isTextFile } from '../../../core/shared/project-file-types'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../../assets'
import { TransientFilesState, TransientFileState } from '../../editor/store/editor-state'
import { UtopiaProjectContext } from './ui-jsx-canvas-contexts'

interface GetParseSuccessOrTransientResult {
  topLevelElements: TopLevelElement[]
  imports: Imports
  jsxFactoryFunction: string | null
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null
}

const EmptyResult: GetParseSuccessOrTransientResult = {
  topLevelElements: [],
  imports: {},
  jsxFactoryFunction: null,
  combinedTopLevelArbitraryBlock: null,
}

export function getParseSuccessOrTransientForFilePath(
  filePath: string,
  projectContents: ProjectContentTreeRoot,
  transientFilesState: TransientFilesState | null,
): GetParseSuccessOrTransientResult {
  const projectFile = getContentsTreeFileFromString(projectContents, filePath)
  if (isTextFile(projectFile) && isParseSuccess(projectFile.fileContents.parsed)) {
    const parseSuccess = projectFile.fileContents.parsed
    const transientFileState: TransientFileState | null =
      transientFilesState == null ? null : transientFilesState[filePath] ?? null
    if (transientFileState == null) {
      return {
        topLevelElements: parseSuccess.topLevelElements,
        imports: parseSuccess.imports,
        jsxFactoryFunction: parseSuccess.jsxFactoryFunction,
        combinedTopLevelArbitraryBlock: parseSuccess.combinedTopLevelArbitraryBlock,
      }
    } else {
      return {
        topLevelElements: transientFileState.topLevelElementsIncludingScenes,
        imports: transientFileState.imports,
        jsxFactoryFunction: parseSuccess.jsxFactoryFunction,
        combinedTopLevelArbitraryBlock: parseSuccess.combinedTopLevelArbitraryBlock,
      }
    }
  } else {
    return EmptyResult
  }
}

export function useGetTopLevelElementsAndImports(
  filePath: string | null,
): { topLevelElements: TopLevelElement[]; imports: Imports } {
  return useContextSelector(UtopiaProjectContext, (c) => {
    if (filePath == null) {
      return { topLevelElements: EmptyResult.topLevelElements, imports: emptyImports() }
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
