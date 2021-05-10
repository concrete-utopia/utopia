import { ArbitraryJSBlock, TopLevelElement } from '../../../core/shared/element-template'
import { Imports, isParseSuccess, isTextFile } from '../../../core/shared/project-file-types'
import { useContextSelector } from '../../../utils/react-performance'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../../assets'
import { TransientFilesState, TransientFileState } from '../../editor/store/editor-state'
import { UtopiaProjectContext } from './ui-jsx-canvas-contexts'
import { useMemo } from 'react'
import * as deepEqual from 'fast-deep-equal'

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

export function useGetTopLevelElements(filePath: string | null): TopLevelElement[] {
  return useContextSelector(
    UtopiaProjectContext,
    useMemo(
      () => (c) => {
        if (filePath == null) {
          return EmptyResult.topLevelElements
        } else {
          return getParseSuccessOrTransientForFilePath(
            filePath,
            c.projectContents,
            c.transientFilesState,
          ).topLevelElements
        }
      },
      [filePath],
    ),
    deepEqual,
  )
}
