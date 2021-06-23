import * as React from 'react'
import type { MapLike } from 'typescript'
import {
  ArbitraryJSBlock,
  isUtopiaJSXComponent,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { fastForEach } from '../../../core/shared/utils'
import {
  getContentsTreeFileFromString,
  ProjectContentsTree,
  ProjectContentTreeRoot,
} from '../../assets'
import { importResultFromImports } from '../../editor/npm-dependency/npm-dependency'
import {
  CanvasBase64Blobs,
  TransientFilesState,
  TransientFileState,
  UIFileBase64Blobs,
} from '../../editor/store/editor-state'
import {
  ComponentRendererComponent,
  createComponentRendererComponent,
} from './ui-jsx-canvas-component-renderer'
import {
  MutableUtopiaContextProps,
  updateMutableUtopiaContextWithNewProps,
  UtopiaProjectContext,
} from './ui-jsx-canvas-contexts'
import { createLookupRender, utopiaCanvasJSXLookup } from './ui-jsx-canvas-element-renderer-utils'
import { runBlockUpdatingScope } from './ui-jsx-canvas-scope-utils'
import * as EP from '../../../core/shared/element-path'
import { DomWalkerInvalidatePathsContextData, UiJsxCanvasContextData } from '../ui-jsx-canvas'
import {
  ElementPath,
  HighlightBoundsForUids,
  isParseSuccess,
  isTextFile,
} from '../../../core/shared/project-file-types'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/workers/parser-printer/parser-printer-utils'
import { defaultIfNull, optionalFlatMap } from '../../../core/shared/optional-utils'
import { getParseSuccessOrTransientForFilePath } from '../canvas-utils'
import { useContextSelector } from 'use-context-selector'
import { shallowEqual } from '../../../core/shared/equality-utils'

const emptyFileBlobs: UIFileBase64Blobs = {}

export function createExecutionScope(
  filePath: string,
  customRequire: (importOrigin: string, toImport: string) => any,
  mutableContextRef: React.MutableRefObject<MutableUtopiaContextProps>,
  topLevelComponentRendererComponents: React.MutableRefObject<
    MapLike<MapLike<ComponentRendererComponent>>
  >,
  projectContents: ProjectContentTreeRoot,
  openStoryboardFileNameKILLME: string | null,
  transientFilesStateKILLME: TransientFilesState | null,
  fileBlobs: CanvasBase64Blobs,
  hiddenInstances: ElementPath[],
  metadataContext: UiJsxCanvasContextData,
  updateInvalidatedPaths: DomWalkerInvalidatePathsContextData,
  shouldIncludeCanvasRootInTheSpy: boolean,
): {
  scope: MapLike<any>
  topLevelJsxComponents: Map<string, UtopiaJSXComponent>
  requireResult: MapLike<any>
} {
  if (!(filePath in topLevelComponentRendererComponents.current)) {
    // we make sure that the ref has an entry for this filepath
    topLevelComponentRendererComponents.current[filePath] = {}
  }
  let topLevelComponentRendererComponentsForFile =
    topLevelComponentRendererComponents.current[filePath]

  const fileBlobsForFile = defaultIfNull(emptyFileBlobs, fileBlobs[filePath])

  const {
    topLevelElements,
    imports,
    jsxFactoryFunction,
    combinedTopLevelArbitraryBlock,
  } = getParseSuccessOrTransientForFilePath(filePath, projectContents, transientFilesStateKILLME)
  const requireResult: MapLike<any> = importResultFromImports(filePath, imports, customRequire)

  const userRequireFn = (toImport: string) => customRequire(filePath, toImport) // TODO this was a React usecallback

  let executionScope: MapLike<any> = { require: userRequireFn, ...requireResult }
  // TODO All of this is run on every interaction o_O

  let topLevelJsxComponents: Map<string, UtopiaJSXComponent> = new Map()

  // Make sure there is something in scope for all of the top level components
  fastForEach(topLevelElements, (topLevelElement) => {
    if (isUtopiaJSXComponent(topLevelElement)) {
      topLevelJsxComponents.set(topLevelElement.name, topLevelElement)
      if (!(topLevelElement.name in topLevelComponentRendererComponentsForFile)) {
        topLevelComponentRendererComponentsForFile[
          topLevelElement.name
        ] = createComponentRendererComponent({
          topLevelElementName: topLevelElement.name,
          mutableContextRef: mutableContextRef,
          filePath: filePath,
        })
      }
    }
  })

  executionScope = {
    ...executionScope,
    ...topLevelComponentRendererComponentsForFile,
  }

  // First make sure everything is in scope
  if (combinedTopLevelArbitraryBlock != null && openStoryboardFileNameKILLME != null) {
    const { highlightBounds, code } = getCodeAndHighlightBoundsForFile(filePath, projectContents)
    const lookupRenderer = createLookupRender(
      EP.emptyElementPath,
      executionScope,
      {},
      requireResult,
      hiddenInstances,
      fileBlobsForFile,
      [],
      undefined,
      metadataContext,
      updateInvalidatedPaths,
      jsxFactoryFunction,
      shouldIncludeCanvasRootInTheSpy,
      openStoryboardFileNameKILLME,
      imports,
      code,
      highlightBounds,
    )

    executionScope[JSX_CANVAS_LOOKUP_FUNCTION_NAME] = utopiaCanvasJSXLookup(
      combinedTopLevelArbitraryBlock.elementsWithin,
      executionScope,
      lookupRenderer,
    )

    runBlockUpdatingScope(requireResult, combinedTopLevelArbitraryBlock, executionScope)
  }

  // WARNING: mutating the mutableContextRef
  updateMutableUtopiaContextWithNewProps(mutableContextRef, {
    ...mutableContextRef.current,
    [filePath]: {
      mutableContext: {
        requireResult: requireResult,
        rootScope: executionScope,
        fileBlobs: fileBlobsForFile,
        jsxFactoryFunctionName: jsxFactoryFunction,
      },
    },
  })

  return {
    scope: executionScope,
    topLevelJsxComponents: topLevelJsxComponents,
    requireResult: requireResult,
  }
}

export function useGetCodeAndHighlightBounds(
  filePath: string | null,
): { code: string; highlightBounds: HighlightBoundsForUids | null } {
  return useContextSelector(
    UtopiaProjectContext,
    (c) => {
      if (filePath == null) {
        return { code: '', highlightBounds: null }
      } else {
        return getCodeAndHighlightBoundsForFile(filePath, c.projectContents)
      }
    },
    shallowEqual,
  )
}

export function getCodeAndHighlightBoundsForFile(
  filePath: string,
  projectContents: ProjectContentTreeRoot,
): {
  code: string
  highlightBounds: HighlightBoundsForUids | null
} {
  const file = getContentsTreeFileFromString(projectContents, filePath)
  if (isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
    return {
      code: file.fileContents.code,
      highlightBounds: file.fileContents.parsed.highlightBounds,
    }
  } else {
    return {
      code: '',
      highlightBounds: null,
    }
  }
}
