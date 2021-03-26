import { useContextSelector } from 'use-context-selector'
import { ArbitraryJSBlock, TopLevelElement } from '../../../core/shared/element-template'
import { Imports, isParseSuccess, isTextFile } from '../../../core/shared/project-file-types'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../../assets'
import { TransientFileState } from '../../editor/store/editor-state'
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
  openStoryboardFileNameKILLME: string | null,
  transientFileState: TransientFileState | null,
): GetParseSuccessOrTransientResult {
  const projectFile = getContentsTreeFileFromString(projectContents, filePath)
  if (isTextFile(projectFile) && isParseSuccess(projectFile.fileContents.parsed)) {
    const parseSuccess = projectFile.fileContents.parsed
    if (openStoryboardFileNameKILLME === filePath && transientFileState != null) {
      return {
        topLevelElements: transientFileState.topLevelElementsIncludingScenes,
        imports: transientFileState.imports,
        jsxFactoryFunction: parseSuccess.jsxFactoryFunction,
        combinedTopLevelArbitraryBlock: parseSuccess.combinedTopLevelArbitraryBlock,
      }
    }
    return {
      topLevelElements: parseSuccess.topLevelElements,
      imports: parseSuccess.imports,
      jsxFactoryFunction: parseSuccess.jsxFactoryFunction,
      combinedTopLevelArbitraryBlock: parseSuccess.combinedTopLevelArbitraryBlock,
    }
  } else {
    return EmptyResult
  }
}

export function useGetTopLevelElements(filePath: string | null): TopLevelElement[] {
  return useContextSelector(UtopiaProjectContext, (c) => {
    if (filePath == null) {
      return EmptyResult.topLevelElements
    }
    return getParseSuccessOrTransientForFilePath(
      filePath,
      c.projectContents,
      c.openStoryboardFilePathKILLME,
      c.transientFileState,
    ).topLevelElements
  })
}
