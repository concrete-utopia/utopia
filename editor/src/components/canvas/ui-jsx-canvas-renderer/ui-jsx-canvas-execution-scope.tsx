import React from 'react'
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
  MutableUtopiaCtxRefData,
  updateMutableUtopiaCtxRefWithNewProps,
  UtopiaProjectCtxAtom,
} from './ui-jsx-canvas-contexts'
import { createLookupRender, utopiaCanvasJSXLookup } from './ui-jsx-canvas-element-renderer-utils'
import { runBlockUpdatingScope } from './ui-jsx-canvas-scope-utils'
import * as EP from '../../../core/shared/element-path'
import { DomWalkerInvalidatePathsCtxData, UiJsxCanvasContextData } from '../ui-jsx-canvas'
import {
  ElementPath,
  HighlightBoundsForUids,
  isParseSuccess,
  isTextFile,
} from '../../../core/shared/project-file-types'
import { defaultIfNull, optionalFlatMap } from '../../../core/shared/optional-utils'
import { getParseSuccessForFilePath } from '../canvas-utils'
import { useContextSelector } from 'use-context-selector'
import { shallowEqual } from '../../../core/shared/equality-utils'
import { usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/shared/dom-utils'
import { emptySet } from '../../../core/shared/set-utils'

const emptyFileBlobs: UIFileBase64Blobs = {}

export function createExecutionScope(
  filePath: string,
  customRequire: (importOrigin: string, toImport: string) => any,
  mutableContextRef: React.MutableRefObject<MutableUtopiaCtxRefData>,
  topLevelComponentRendererComponents: React.MutableRefObject<
    MapLike<MapLike<ComponentRendererComponent>>
  >,
  projectContents: ProjectContentTreeRoot,
  openStoryboardFileNameKILLME: string | null,
  fileBlobs: CanvasBase64Blobs,
  hiddenInstances: Array<ElementPath>,
  displayNoneInstances: Array<ElementPath>,
  metadataContext: UiJsxCanvasContextData,
  updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData,
  shouldIncludeCanvasRootInTheSpy: boolean,
  editedText: ElementPath | null,
): {
  scope: MapLike<any>
  topLevelJsxComponents: Map<string | null, UtopiaJSXComponent>
  requireResult: MapLike<any>
} {
  if (!(filePath in topLevelComponentRendererComponents.current)) {
    // we make sure that the ref has an entry for this filepath
    topLevelComponentRendererComponents.current[filePath] = {}
  }
  let topLevelComponentRendererComponentsForFile =
    topLevelComponentRendererComponents.current[filePath]

  const fileBlobsForFile = defaultIfNull(emptyFileBlobs, fileBlobs[filePath])

  const { topLevelElements, imports, jsxFactoryFunction, combinedTopLevelArbitraryBlock } =
    getParseSuccessForFilePath(filePath, projectContents)
  const requireResult: MapLike<any> = importResultFromImports(filePath, imports, customRequire)

  const userRequireFn = (toImport: string) => customRequire(filePath, toImport) // TODO this was a React usecallback

  let module = {
    exports: {},
  }

  // Mirrors the same thing in evaluateJs.
  let process = {
    env: {
      NODE_ENV: 'production',
    },
  }

  let executionScope: MapLike<any> = {
    React: React,
    require: userRequireFn,
    module: module,
    exports: module.exports,
    process: process,
    ...requireResult,
  }
  // TODO All of this is run on every interaction o_O

  let topLevelJsxComponents: Map<string | null, UtopiaJSXComponent> = new Map()

  // Make sure there is something in scope for all of the top level components
  fastForEach(topLevelElements, (topLevelElement) => {
    if (isUtopiaJSXComponent(topLevelElement)) {
      topLevelJsxComponents.set(topLevelElement.name, topLevelElement)
      const elementName = topLevelElement.name ?? 'default'
      if (!(elementName in topLevelComponentRendererComponentsForFile)) {
        topLevelComponentRendererComponentsForFile[elementName] = createComponentRendererComponent({
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
      displayNoneInstances,
      fileBlobsForFile,
      new Set(),
      undefined,
      metadataContext,
      updateInvalidatedPaths,
      jsxFactoryFunction,
      shouldIncludeCanvasRootInTheSpy,
      openStoryboardFileNameKILLME,
      imports,
      code,
      highlightBounds,
      editedText,
    )

    executionScope[JSX_CANVAS_LOOKUP_FUNCTION_NAME] = utopiaCanvasJSXLookup(
      combinedTopLevelArbitraryBlock.elementsWithin,
      executionScope,
      lookupRenderer,
    )

    runBlockUpdatingScope(filePath, requireResult, combinedTopLevelArbitraryBlock, executionScope)
  }
  // WARNING: mutating the mutableContextRef
  updateMutableUtopiaCtxRefWithNewProps(mutableContextRef, {
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

const emptyHighlightBoundsResult = { code: '', highlightBounds: null }

export function useGetCodeAndHighlightBounds(
  filePath: string | null,
  shouldUpdateCallback: () => boolean,
): {
  code: string
  highlightBounds: HighlightBoundsForUids | null
} {
  const projectContext = usePubSubAtomReadOnly(UtopiaProjectCtxAtom, shouldUpdateCallback)
  if (filePath == null) {
    return emptyHighlightBoundsResult
  } else {
    return getCodeAndHighlightBoundsForFile(filePath, projectContext.projectContents)
  }
}

export function getCodeAndHighlightBoundsForFile(
  filePath: string,
  projectContents: ProjectContentTreeRoot,
): {
  code: string
  highlightBounds: HighlightBoundsForUids | null
} {
  const file = getContentsTreeFileFromString(projectContents, filePath)
  if (file != null && isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
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
