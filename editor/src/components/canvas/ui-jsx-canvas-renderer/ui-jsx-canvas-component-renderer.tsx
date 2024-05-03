import React from 'react'
import type { MapLike } from 'typescript'
import type {
  EarlyReturn,
  JSXElementChild,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import {
  isUtopiaJSXComponent,
  isSVGElement,
  isJSXElement,
  propertiesExposedByParam,
  propertiesExposedByParams,
} from '../../../core/shared/element-template'
import { optionalMap } from '../../../core/shared/optional-utils'
import type {
  DomWalkerInvalidatePathsCtxData,
  UiJsxCanvasContextData,
  VariableData,
} from '../ui-jsx-canvas'
import {
  DomWalkerInvalidatePathsCtxAtom,
  UiJsxCanvasCtxAtom,
  ElementsToRerenderGLOBAL,
} from '../ui-jsx-canvas'
import type { MutableUtopiaCtxRefData } from './ui-jsx-canvas-contexts'
import { RerenderUtopiaCtxAtom, SceneLevelUtopiaCtxAtom } from './ui-jsx-canvas-contexts'
import { applyPropsParamToPassedProps } from './ui-jsx-canvas-props-utils'
import { runBlockUpdatingScope } from './ui-jsx-canvas-scope-utils'
import * as EP from '../../../core/shared/element-path'
import {
  createLookupRender,
  renderCoreElement,
  utopiaCanvasJSXLookup,
} from './ui-jsx-canvas-element-renderer-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { UTOPIA_INSTANCE_PATH, UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import { getPathsFromString, getUtopiaID } from '../../../core/shared/uid-utils'
import { useGetTopLevelElementsAndImports } from './ui-jsx-canvas-top-level-elements'
import { useGetCodeAndHighlightBounds } from './ui-jsx-canvas-execution-scope'
import { usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import {
  JSX_CANVAS_LOOKUP_FUNCTION_NAME,
  applyBlockReturnFunctions,
} from '../../../core/shared/dom-utils'
import { objectMap } from '../../../core/shared/object-utils'
import type { ComponentRendererComponent } from './component-renderer-component'
import { mapArrayToDictionary } from '../../../core/shared/array-utils'
import { assertNever } from '../../../core/shared/utils'
import { addFakeSpyEntry } from './ui-jsx-canvas-spy-wrapper'

function tryToGetInstancePath(
  maybePath: ElementPath | null,
  pathsString: string | null,
): ElementPath | null {
  const paths = getPathsFromString(pathsString)
  if (EP.isElementPath(maybePath)) {
    return maybePath
  } else if (paths.length > 0) {
    return paths[0]
  } else {
    return null
  }
}

export function createComponentRendererComponent(params: {
  topLevelElementName: string | null
  filePath: string
  mutableContextRef: React.MutableRefObject<MutableUtopiaCtxRefData>
}): ComponentRendererComponent {
  const Component = (realPassedPropsIncludingUtopiaSpecialStuff: any) => {
    const {
      [UTOPIA_INSTANCE_PATH]: instancePathAny, // TODO types?
      [UTOPIA_PATH_KEY]: pathsString, // TODO types?
      ...realPassedProps
    } = realPassedPropsIncludingUtopiaSpecialStuff

    const mutableContext = params.mutableContextRef.current[params.filePath].mutableContext

    const instancePath: ElementPath | null = tryToGetInstancePath(instancePathAny, pathsString)

    function shouldUpdate() {
      return (
        ElementsToRerenderGLOBAL.current === 'rerender-all-elements' ||
        ElementsToRerenderGLOBAL.current.some((er) => {
          return (
            (instancePath != null &&
              (EP.pathsEqual(er, instancePath) || EP.isParentComponentOf(instancePath, er))) ||
            isElementInChildrenPropTree(EP.toString(er), realPassedProps)
          )
        })
      )
    }

    const rerenderUtopiaContext = usePubSubAtomReadOnly(RerenderUtopiaCtxAtom, shouldUpdate)

    const { topLevelElements, imports } = useGetTopLevelElementsAndImports(
      params.filePath,
      shouldUpdate,
    )
    const { code, highlightBounds } = useGetCodeAndHighlightBounds(params.filePath, shouldUpdate)

    const utopiaJsxComponent: UtopiaJSXComponent | null =
      topLevelElements.find((elem): elem is UtopiaJSXComponent => {
        return isUtopiaJSXComponent(elem) && elem.name === params.topLevelElementName
      }) ?? null

    const shouldIncludeCanvasRootInTheSpy = rerenderUtopiaContext.shouldIncludeCanvasRootInTheSpy

    const hiddenInstances = rerenderUtopiaContext.hiddenInstances
    const displayNoneInstances = rerenderUtopiaContext.displayNoneInstances
    const sceneContext = usePubSubAtomReadOnly(SceneLevelUtopiaCtxAtom, shouldUpdate)

    let metadataContext: UiJsxCanvasContextData = usePubSubAtomReadOnly(
      UiJsxCanvasCtxAtom,
      shouldUpdate,
    )
    const updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData = usePubSubAtomReadOnly(
      DomWalkerInvalidatePathsCtxAtom,
      shouldUpdate,
    )

    if (utopiaJsxComponent == null) {
      // If this element cannot be found, we want to purposefully cause a 'ReferenceError' to notify the user.
      throw new ReferenceError(`${params.topLevelElementName} is not defined`)
    }

    const rootElementPath = optionalMap(
      (path) => EP.appendNewElementPath(path, getUtopiaID(utopiaJsxComponent.rootElement)),
      instancePath,
    )

    let codeError: Error | null = null

    const appliedProps = optionalMap(
      (param) =>
        applyPropsParamToPassedProps(
          mutableContext.rootScope,
          rootElementPath,
          realPassedProps,
          param,
          {
            requireResult: mutableContext.requireResult,
            rootScope: mutableContext.rootScope,
            parentComponentInputProps: realPassedProps,
            filePath: params.filePath,
            hiddenInstances: hiddenInstances,
            displayNoneInstances: displayNoneInstances,
            fileBlobs: mutableContext.fileBlobs,
            validPaths: sceneContext.validPaths,
            reactChildren: undefined,
            metadataContext: metadataContext,
            updateInvalidatedPaths: updateInvalidatedPaths,
            jsxFactoryFunctionName: mutableContext.jsxFactoryFunctionName,
            shouldIncludeCanvasRootInTheSpy: shouldIncludeCanvasRootInTheSpy,
            imports: imports,
            code: code,
            highlightBounds: highlightBounds,
            editedText: rerenderUtopiaContext.editedText,
            variablesInScope: {},
          },
          undefined,
          codeError,
        ),
      utopiaJsxComponent.param,
    ) ?? { props: realPassedProps }

    let scope: MapLike<any> = {
      ...mutableContext.rootScope,
      ...appliedProps,
    }

    let spiedVariablesInScope: VariableData = {}
    if (utopiaJsxComponent.param != null) {
      spiedVariablesInScope = mapArrayToDictionary(
        propertiesExposedByParam(utopiaJsxComponent.param),
        (paramName) => {
          return paramName
        },
        (paramName) => {
          return {
            spiedValue: scope[paramName],
            insertionCeiling: instancePath,
          }
        },
      )
    }

    // Protect against infinite recursion by taking the view that anything
    // beyond a particular depth is going infinite or is likely
    // to be out of control otherwise.
    if (instancePath != null && EP.depth(instancePath) > 100) {
      throw new Error(`Element hierarchy is too deep, potentially has become infinite.`)
    }

    // either this updateInvalidatedPaths or the one in SpyWrapper is probably redundant
    if (shouldUpdate()) {
      updateInvalidatedPaths((invalidPaths) => {
        // Do not add `svg` elements that are the root element of a component.
        // As they will not be cleared by the DOM walker as they are not instances
        // of HTMLElement.
        const isSVGJSXElement =
          isJSXElement(utopiaJsxComponent.rootElement) &&
          isSVGElement(utopiaJsxComponent.rootElement.name)
        if (rootElementPath != null && !isSVGJSXElement) {
          return invalidPaths.add(EP.toString(rootElementPath))
        } else {
          return invalidPaths
        }
      })
    }

    const renderContextBase = {
      rootScope: scope,
      parentComponentInputProps: realPassedProps,
      requireResult: mutableContext.requireResult,
      hiddenInstances: hiddenInstances,
      displayNoneInstances: displayNoneInstances,
      fileBlobs: mutableContext.fileBlobs,
      validPaths: sceneContext.validPaths,
      reactChildren: undefined,
      metadataContext: metadataContext,
      updateInvalidatedPaths: updateInvalidatedPaths,
      jsxFactoryFunctionName: mutableContext.jsxFactoryFunctionName,
      shouldIncludeCanvasRootInTheSpy: shouldIncludeCanvasRootInTheSpy,
      filePath: params.filePath,
      imports: imports,
      code: code,
      highlightBounds: highlightBounds,
      editedText: rerenderUtopiaContext.editedText,
    }

    const buildResult = React.useRef<React.ReactElement | null>(null)

    let earlyReturn: EarlyReturn | null = null
    if (utopiaJsxComponent.arbitraryJSBlock != null) {
      const propertiesFromParams = propertiesExposedByParams(
        utopiaJsxComponent.arbitraryJSBlock.params,
      )
      const lookupRenderer = createLookupRender(
        rootElementPath,
        {
          ...renderContextBase,
          variablesInScope: {},
        },
        null,
        propertiesFromParams,
        null,
      )

      scope[JSX_CANVAS_LOOKUP_FUNCTION_NAME] = utopiaCanvasJSXLookup(
        utopiaJsxComponent.arbitraryJSBlock.elementsWithin,
        scope,
        lookupRenderer,
      )
      applyBlockReturnFunctions(scope)

      const arbitraryBlockResult = runBlockUpdatingScope(
        params.filePath,
        mutableContext.requireResult,
        utopiaJsxComponent.arbitraryJSBlock,
        scope,
      )

      switch (arbitraryBlockResult.type) {
        case 'ARBITRARY_BLOCK_RAN_TO_END':
          spiedVariablesInScope = {
            ...spiedVariablesInScope,
            ...objectMap(
              (spiedValue) => ({
                spiedValue: spiedValue,
                insertionCeiling: instancePath,
              }),
              arbitraryBlockResult.scope,
            ),
          }
          break
        case 'EARLY_RETURN_VOID':
          earlyReturn = arbitraryBlockResult
          buildResult.current = undefined as any
          break
        case 'EARLY_RETURN_RESULT':
          earlyReturn = arbitraryBlockResult
          buildResult.current = arbitraryBlockResult.result as any
          break
        default:
          assertNever(arbitraryBlockResult)
      }
    }

    function buildComponentRenderResult(element: JSXElementChild): React.ReactElement {
      const ownElementPath = optionalMap(
        (path) => EP.appendNewElementPath(path, getUtopiaID(element)),
        instancePath,
      )

      const renderedCoreElement = renderCoreElement(
        element,
        ownElementPath,
        scope,
        {
          ...renderContextBase,
          variablesInScope: spiedVariablesInScope,
        },
        realPassedProps['data-uid'],
        codeError,
        null,
      )

      if (typeof renderedCoreElement === 'string' || typeof renderedCoreElement === 'number') {
        return <>{renderedCoreElement}</>
      } else {
        return renderedCoreElement
      }
    }

    if (earlyReturn != null) {
      if (instancePath != null) {
        addFakeSpyEntry(
          sceneContext.validPaths,
          metadataContext,
          instancePath,
          utopiaJsxComponent.rootElement,
          params.filePath,
          imports,
          'not-a-conditional',
          earlyReturn,
          null,
        )
      }
    } else if (shouldUpdate()) {
      buildResult.current = buildComponentRenderResult(utopiaJsxComponent.rootElement)
    }
    return buildResult.current
  }
  Component.displayName = `ComponentRenderer(${params.topLevelElementName})`
  Component.topLevelElementName = params.topLevelElementName
  Component.utopiaType = 'UTOPIA_COMPONENT_RENDERER_COMPONENT' as const
  Component.filePath = params.filePath
  Component.originalName = params.topLevelElementName
  return Component
}

// Checks if the element with the given elementPath is rendered in the props.children subtree
// LIMITATION: this function only checks props.children, so if the given element is rendered, but from a
// different prop, isElementInChildrenPropTree will return false
// If we will support renderProps, this should be updated to check other props which receive react elements
function isElementInChildrenPropTree(elementPath: string, props: any): boolean {
  const childrenArr = React.Children.toArray(props.children).filter(React.isValidElement)

  if (childrenArr.length === 0) {
    return false
  }
  const elementIsChild = childrenArr.some((c) => (c.props as any)[UTOPIA_PATH_KEY] === elementPath)
  if (elementIsChild) {
    return true
  } else {
    return childrenArr.some((c) => isElementInChildrenPropTree(elementPath, c.props))
  }
}
