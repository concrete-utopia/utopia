import * as React from 'react'
import type { MapLike } from 'typescript'
import {
  isUtopiaJSXComponent,
  TopLevelElement,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { Imports } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import { ProjectContentTreeRoot } from '../../assets'
import { importResultFromImports } from '../../editor/npm-dependency/npm-dependency'
import { TransientFileState } from '../../editor/store/editor-state'
import {
  ComponentRendererComponent,
  createComponentRendererComponent,
} from './ui-jsx-canvas-component-renderer'
import { MutableUtopiaContextProps } from './ui-jsx-canvas-contexts'
import { getTopLevelElements } from './ui-jsx-canvas-top-level-elements'

export function useExecutionScope(
  filePath: string,
  customRequire: (importOrigin: string, toImport: string) => any,
  mutableContextRef: React.MutableRefObject<MutableUtopiaContextProps>, // TODO this is only for the storyboard now
  projectContents: ProjectContentTreeRoot,
  openStoryboardFileNameKILLME: string | null,
  transientFileState: TransientFileState | null,
): {
  scope: MapLike<any>
  topLevelJsxComponents: Map<string, UtopiaJSXComponent>
  requireResult: MapLike<any>
} {
  const { topLevelElements, imports } = getTopLevelElements(
    filePath,
    projectContents,
    openStoryboardFileNameKILLME,
    transientFileState,
  )
  const requireResult: MapLike<any> = importResultFromImports(filePath, imports, customRequire)

  const userRequireFn = React.useCallback((toImport: string) => customRequire(filePath, toImport), [
    filePath,
    customRequire,
  ])
  let executionScope: MapLike<any> = { require: userRequireFn, ...requireResult }
  // TODO All of this is run on every interaction o_O

  let topLevelJsxComponents: Map<string, UtopiaJSXComponent> = new Map()
  let topLevelComponentRendererComponents = React.useRef<MapLike<ComponentRendererComponent>>({}) // these are only the storyboard components now

  // Make sure there is something in scope for all of the top level components
  fastForEach(topLevelElements, (topLevelElement) => {
    if (isUtopiaJSXComponent(topLevelElement)) {
      topLevelJsxComponents.set(topLevelElement.name, topLevelElement)
      if (!(topLevelElement.name in topLevelComponentRendererComponents.current)) {
        topLevelComponentRendererComponents.current[
          topLevelElement.name
        ] = createComponentRendererComponent({
          topLevelElementName: topLevelElement.name,
          mutableContextRef: mutableContextRef,
          filePath: filePath,
        })
      }
    }
  })

  return {
    scope: {
      ...executionScope,
      ...topLevelComponentRendererComponents.current,
    },
    topLevelJsxComponents: topLevelJsxComponents,
    requireResult: requireResult,
  }
}
