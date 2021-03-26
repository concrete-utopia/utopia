import * as React from 'react'
import type { MapLike } from 'typescript'
import {
  ArbitraryJSBlock,
  isUtopiaJSXComponent,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { fastForEach } from '../../../core/shared/utils'
import { ProjectContentTreeRoot } from '../../assets'
import { importResultFromImports } from '../../editor/npm-dependency/npm-dependency'
import { TransientFileState, UIFileBase64Blobs } from '../../editor/store/editor-state'
import {
  ComponentRendererComponent,
  createComponentRendererComponent,
} from './ui-jsx-canvas-component-renderer'
import { MutableUtopiaContextProps } from './ui-jsx-canvas-contexts'
import { createLookupRender, utopiaCanvasJSXLookup } from './ui-jsx-canvas-element-renderer-utils'
import { runBlockUpdatingScope } from './ui-jsx-canvas-scope-utils'
import { getTopLevelElements } from './ui-jsx-canvas-top-level-elements'
import * as TP from '../../../core/shared/template-path'
import { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import { TemplatePath } from '../../../core/shared/project-file-types'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/workers/parser-printer/parser-printer-utils'

export function createExecutionScope(
  filePath: string,
  customRequire: (importOrigin: string, toImport: string) => any,
  mutableContextRef: React.MutableRefObject<MutableUtopiaContextProps>,
  topLevelComponentRendererComponents: React.MutableRefObject<
    MapLike<MapLike<ComponentRendererComponent>>
  >,
  projectContents: ProjectContentTreeRoot,
  openStoryboardFileNameKILLME: string | null,
  transientFileState: TransientFileState | null,
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null,
  fileBlobs: UIFileBase64Blobs, // this is only for the storyboard file now,
  hiddenInstances: TemplatePath[],
  metadataContext: UiJsxCanvasContextData,
  jsxFactoryFunction: string | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
): {
  scope: MapLike<any>
  topLevelJsxComponents: Map<string, UtopiaJSXComponent>
  requireResult: MapLike<any>
} {
  if (topLevelComponentRendererComponents.current[filePath] == null) {
    // we make sure that the ref has an entry for this filepath
    topLevelComponentRendererComponents.current[filePath] = {}
  }
  let topLevelComponentRendererComponentsForFile =
    topLevelComponentRendererComponents.current[filePath]
  const { topLevelElements, imports } = getTopLevelElements(
    filePath,
    projectContents,
    openStoryboardFileNameKILLME,
    transientFileState,
  )
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

  // First make sure everything is in scope
  if (combinedTopLevelArbitraryBlock != null && openStoryboardFileNameKILLME != null) {
    const lookupRenderer = createLookupRender(
      TP.emptyInstancePath,
      executionScope,
      {},
      requireResult,
      hiddenInstances,
      fileBlobs,
      [],
      undefined,
      metadataContext,
      jsxFactoryFunction,
      shouldIncludeCanvasRootInTheSpy,
      openStoryboardFileNameKILLME,
    )

    executionScope[JSX_CANVAS_LOOKUP_FUNCTION_NAME] = utopiaCanvasJSXLookup(
      combinedTopLevelArbitraryBlock.elementsWithin,
      executionScope,
      lookupRenderer,
    )

    runBlockUpdatingScope(requireResult, combinedTopLevelArbitraryBlock, executionScope)
  }

  return {
    scope: {
      ...executionScope,
      ...topLevelComponentRendererComponentsForFile,
    },
    topLevelJsxComponents: topLevelJsxComponents,
    requireResult: requireResult,
  }
}
