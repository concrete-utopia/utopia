import * as React from 'react'
import { MapLike } from 'typescript'
import { getUtopiaID } from '../../../core/model/element-template-utils'
import { isSceneElementIgnoringImports } from '../../../core/model/scene-utils'
import {
  UTOPIA_PATHS_KEY,
  UTOPIA_SCENE_ID_KEY,
  UTOPIA_INSTANCE_PATH,
  UTOPIA_UIDS_KEY,
  UTOPIA_UID_ORIGINAL_PARENTS_KEY,
} from '../../../core/model/utopia-constants'
import { flatMapEither, forEachRight } from '../../../core/shared/either'
import {
  JSXElementChild,
  isJSXElement,
  JSXElement,
  jsxAttributeValue,
  ElementsWithin,
  isIntrinsicElement,
  isIntrinsicHTMLElement,
  JSXArbitraryBlock,
  getJSXAttribute,
} from '../../../core/shared/element-template'
import { jsxAttributesToProps, setJSXValueAtPath } from '../../../core/shared/jsx-attributes'
import {
  ElementPath,
  HighlightBoundsForUids,
  Imports,
} from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/workers/parser-printer/parser-printer-utils'
import { Utils } from '../../../uuiui-deps'
import { UIFileBase64Blobs } from '../../editor/store/editor-state'
import { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import { SceneComponent } from './scene-component'
import * as PP from '../../../core/shared/property-path'
import * as EP from '../../../core/shared/element-path'
import { Storyboard } from 'utopia-api'
import { resolveParamsAndRunJsCode } from '../../../core/shared/javascript-cache'
import { objectMap } from '../../../core/shared/object-utils'
import { cssValueOnlyContainsComments } from '../../../printer-parsers/css/css-parser-utils'
import { filterDataProps } from '../../../utils/canvas-react-utils'
import { buildSpyWrappedElement } from './ui-jsx-canvas-spy-wrapper'
import { appendToUidString, createIndexedUid } from '../../../core/shared/uid-utils'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { isComponentRendererComponent } from './ui-jsx-canvas-component-renderer'
import { optionalMap } from '../../../core/shared/optional-utils'
import { canvasMissingJSXElementError } from './canvas-render-errors'

export function createLookupRender(
  elementPath: ElementPath,
  rootScope: MapLike<any>,
  parentComponentInputProps: MapLike<any>,
  requireResult: MapLike<any>,
  hiddenInstances: Array<ElementPath>,
  fileBlobs: UIFileBase64Blobs,
  validPaths: Array<ElementPath>,
  reactChildren: React.ReactNode | undefined,
  metadataContext: UiJsxCanvasContextData,
  jsxFactoryFunctionName: string | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  filePath: string,
  imports: Imports,
  code: string,
  highlightBounds: HighlightBoundsForUids | null,
): (element: JSXElement, scope: MapLike<any>) => React.ReactChild {
  let index = 0

  return (element: JSXElement, scope: MapLike<any>): React.ReactChild => {
    index++
    const innerUID = getUtopiaID(element)
    const generatedUID = createIndexedUid(innerUID, index)
    const withGeneratedUID = setJSXValueAtPath(
      element.props,
      PP.create(['data-uid']),
      jsxAttributeValue(generatedUID, emptyComments),
    )

    // TODO BALAZS should this be here? or should the arbitrary block never have a template path with that last generated element?
    const elementPathWithoutTheLastElementBecauseThatsAWeirdGeneratedUID = EP.parentPath(
      elementPath,
    )

    const innerPath = EP.appendToPath(
      elementPathWithoutTheLastElementBecauseThatsAWeirdGeneratedUID,
      generatedUID,
    )

    let augmentedInnerElement = element
    forEachRight(withGeneratedUID, (attrs) => {
      augmentedInnerElement = {
        ...augmentedInnerElement,
        props: attrs,
      }
    })
    return renderCoreElement(
      augmentedInnerElement,
      innerPath,
      rootScope,
      scope,
      parentComponentInputProps,
      requireResult,
      hiddenInstances,
      fileBlobs,
      validPaths,
      generatedUID,
      reactChildren,
      metadataContext,
      jsxFactoryFunctionName,
      null,
      shouldIncludeCanvasRootInTheSpy,
      filePath,
      imports,
      code,
      highlightBounds,
    )
  }
}

function monkeyUidProp(uid: string | undefined, propsToUpdate: MapLike<any>): MapLike<any> {
  let monkeyedProps: MapLike<any> = {
    ...propsToUpdate,
  }

  const uidsFromProps = monkeyedProps[UTOPIA_UIDS_KEY]
  const uidsToPass = appendToUidString(uidsFromProps, uid)
  monkeyedProps[UTOPIA_UIDS_KEY] = uidsToPass

  return monkeyedProps
}

export function renderCoreElement(
  element: JSXElementChild,
  elementPath: ElementPath,
  rootScope: MapLike<any>,
  inScope: MapLike<any>,
  parentComponentInputProps: MapLike<any>,
  requireResult: MapLike<any>,
  hiddenInstances: Array<ElementPath>,
  fileBlobs: UIFileBase64Blobs,
  validPaths: Array<ElementPath>,
  uid: string | undefined,
  reactChildren: React.ReactNode | undefined,
  metadataContext: UiJsxCanvasContextData,
  jsxFactoryFunctionName: string | null,
  codeError: Error | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  filePath: string,
  imports: Imports,
  code: string,
  highlightBounds: HighlightBoundsForUids | null,
): React.ReactChild {
  if (codeError != null) {
    throw codeError
  }
  switch (element.type) {
    case 'JSX_ELEMENT': {
      const assembledProps = jsxAttributesToProps(inScope, element.props, requireResult)

      const passthroughProps = monkeyUidProp(uid, assembledProps)

      const key = optionalMap(EP.toString, elementPath) ?? passthroughProps[UTOPIA_UIDS_KEY]

      return renderJSXElement(
        key,
        element,
        elementPath,
        parentComponentInputProps,
        requireResult,
        rootScope,
        inScope,
        hiddenInstances,
        fileBlobs,
        validPaths,
        passthroughProps,
        metadataContext,
        jsxFactoryFunctionName,
        null,
        shouldIncludeCanvasRootInTheSpy,
        filePath,
        imports,
        code,
        highlightBounds,
      )
    }
    case 'JSX_ARBITRARY_BLOCK': {
      const innerRender = createLookupRender(
        elementPath,
        rootScope,
        parentComponentInputProps,
        requireResult,
        hiddenInstances,
        fileBlobs,
        validPaths,
        reactChildren,
        metadataContext,
        jsxFactoryFunctionName,
        shouldIncludeCanvasRootInTheSpy,
        filePath,
        imports,
        code,
        highlightBounds,
      )

      const blockScope = {
        ...inScope,
        [JSX_CANVAS_LOOKUP_FUNCTION_NAME]: utopiaCanvasJSXLookup(
          element.elementsWithin,
          inScope,
          innerRender,
        ),
      }
      return runJSXArbitraryBlock(requireResult, element, blockScope)
    }
    case 'JSX_FRAGMENT': {
      let renderedChildren: Array<React.ReactChild> = []
      fastForEach(element.children, (child) => {
        const childPath = EP.appendToPath(EP.parentPath(elementPath), getUtopiaID(child))
        const renderResult = renderCoreElement(
          child,
          childPath,
          rootScope,
          inScope,
          parentComponentInputProps,
          requireResult,
          hiddenInstances,
          fileBlobs,
          validPaths,
          uid,
          reactChildren,
          metadataContext,
          jsxFactoryFunctionName,
          codeError,
          shouldIncludeCanvasRootInTheSpy,
          filePath,
          imports,
          code,
          highlightBounds,
        )
        renderedChildren.push(renderResult)
      })
      return <>{renderedChildren}</>
    }
    case 'JSX_TEXT_BLOCK': {
      return element.text
    }
    default:
      const _exhaustiveCheck: never = element
      throw new Error(`Unhandled type ${JSON.stringify(element)}`)
  }
}

function renderJSXElement(
  key: string,
  jsx: JSXElement,
  elementPath: ElementPath,
  parentComponentInputProps: MapLike<any>,
  requireResult: MapLike<any>,
  rootScope: MapLike<any>,
  inScope: MapLike<any>,
  hiddenInstances: Array<ElementPath>,
  fileBlobs: UIFileBase64Blobs,
  validPaths: Array<ElementPath>,
  passthroughProps: MapLike<any>,
  metadataContext: UiJsxCanvasContextData,
  jsxFactoryFunctionName: string | null,
  codeError: Error | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  filePath: string,
  imports: Imports,
  code: string,
  highlightBounds: HighlightBoundsForUids | null,
): React.ReactElement {
  if (elementPath == null) {
    throw new Error(`Utopia Error: the element renderer did not receive a ElementPath, key: ${key}`)
  }
  let elementProps = { key: key, ...passthroughProps }
  if (isHidden(hiddenInstances, elementPath)) {
    elementProps = hideElement(elementProps)
  }
  elementProps = streamlineInFileBlobs(elementProps, fileBlobs)

  const createChildrenElement = (child: JSXElementChild): React.ReactChild => {
    const childPath = EP.appendToPath(elementPath, getUtopiaID(child))
    return renderCoreElement(
      child,
      childPath,
      rootScope,
      inScope,
      parentComponentInputProps,
      requireResult,
      hiddenInstances,
      fileBlobs,
      validPaths,
      undefined,
      undefined,
      metadataContext,
      jsxFactoryFunctionName,
      codeError,
      shouldIncludeCanvasRootInTheSpy,
      filePath,
      imports,
      code,
      highlightBounds,
    )
  }

  const childrenElements = jsx.children.map(createChildrenElement)
  const elementInScope = getElementFromScope(jsx, inScope)
  const elementFromImport = getElementFromScope(jsx, requireResult)
  const elementFromScopeOrImport = Utils.defaultIfNull(elementFromImport, elementInScope)
  const elementIsScene = isSceneElementIgnoringImports(jsx)
  const elementOrScene = elementIsScene ? SceneComponent : elementFromScopeOrImport
  const elementIsIntrinsic =
    !elementIsScene && elementFromScopeOrImport == null && isIntrinsicElement(jsx.name)
  const elementIsBaseHTML = elementIsIntrinsic && isIntrinsicHTMLElement(jsx.name)
  const FinalElement = elementIsIntrinsic ? jsx.name.baseVariable : elementOrScene

  const elementPropsWithScenePath = isComponentRendererComponent(FinalElement)
    ? { ...elementProps, [UTOPIA_INSTANCE_PATH]: elementPath }
    : elementProps

  const elementPropsWithSceneID = elementIsScene
    ? { ...elementPropsWithScenePath, [UTOPIA_SCENE_ID_KEY]: EP.toString(elementPath) }
    : elementPropsWithScenePath

  const finalProps =
    elementIsIntrinsic && !elementIsBaseHTML
      ? filterDataProps(elementPropsWithSceneID)
      : elementPropsWithSceneID

  const finalPropsIcludingElementPath = {
    ...finalProps,
    [UTOPIA_PATHS_KEY]: optionalMap(EP.toString, elementPath),
  }

  const staticElementPathForGeneratedElement = optionalMap(EP.dynamicPathToStaticPath, elementPath)

  const staticValidPaths = validPaths.map(EP.dynamicPathToStaticPath)

  if (FinalElement == null) {
    throw canvasMissingJSXElementError(jsxFactoryFunctionName, code, jsx, filePath, highlightBounds)
  }

  if (
    elementPath != null &&
    EP.containsPath(staticElementPathForGeneratedElement, staticValidPaths)
  ) {
    let childrenElementPaths: ElementPath[] = []

    Utils.fastForEach(jsx.children, (child) => {
      if (isJSXElement(child)) {
        const childPath = optionalMap((p) => EP.appendToPath(p, getUtopiaID(child)), elementPath)
        if (childPath != null && EP.containsPath(childPath, validPaths)) {
          childrenElementPaths.push(childPath)
        }
      }
    })

    return buildSpyWrappedElement(
      jsx,
      finalPropsIcludingElementPath,
      elementPath,
      metadataContext,
      childrenElementPaths,
      childrenElements,
      FinalElement,
      inScope,
      jsxFactoryFunctionName,
      shouldIncludeCanvasRootInTheSpy,
      imports,
    )
  } else {
    const childrenOrNull = childrenElements.length !== 0 ? childrenElements : null
    return renderComponentUsingJsxFactoryFunction(
      inScope,
      jsxFactoryFunctionName,
      FinalElement,
      finalPropsIcludingElementPath,
      childrenOrNull,
    )
  }
}

function isHidden(hiddenInstances: ElementPath[], elementPath: ElementPath | null): boolean {
  return elementPath != null && hiddenInstances.some((path) => EP.pathsEqual(path, elementPath))
}

function hideElement(props: any): any {
  const styleProps = Utils.propOr({}, 'style', props as any)
  return {
    ...props,
    style: {
      ...styleProps,
      visibility: 'hidden',
    },
  } as any
}

export function utopiaCanvasJSXLookup(
  elementsWithin: ElementsWithin,
  executionScope: MapLike<any>,
  render: (element: JSXElement, inScope: MapLike<any>) => React.ReactChild,
): (uid: string, inScope: MapLike<any>) => React.ReactChild | null {
  return (uid, inScope) => {
    const element = elementsWithin[uid]
    if (element == null) {
      return null
    } else {
      const combinedScope = { ...executionScope, ...inScope }
      return render(element, combinedScope)
    }
  }
}

function runJSXArbitraryBlock(
  requireResult: MapLike<any>,
  block: JSXArbitraryBlock,
  currentScope: MapLike<any>,
): any {
  return resolveParamsAndRunJsCode(block, requireResult, currentScope)
}

function getElementFromScope(jsxElementToLookup: JSXElement, scope: MapLike<any> | null): any {
  if (scope == null) {
    return undefined
  } else {
    if (jsxElementToLookup.name.baseVariable in scope) {
      const fromVar = scope[jsxElementToLookup.name.baseVariable]
      const result = Utils.pathOr(
        undefined,
        PP.getElements(jsxElementToLookup.name.propertyPath),
        fromVar,
      )
      return result
    } else {
      return undefined
    }
  }
}

export function renderComponentUsingJsxFactoryFunction(
  inScope: MapLike<any>,
  factoryFunctionName: string | null,
  type: any,
  props: any,
  ...children: Array<any>
): any {
  const fixedProps = fixStyleObjectRemoveCommentOnlyValues(props)
  let factoryFunction = React.createElement
  if (factoryFunctionName != null) {
    if (factoryFunctionName in inScope) {
      if (
        inScope[factoryFunctionName] != null &&
        typeof inScope[factoryFunctionName] === 'function'
      ) {
        factoryFunction = inScope[factoryFunctionName]
      } else {
        // TODO add StackFrame!
        throw new Error(`Factory function ${factoryFunctionName} is undefined or not a function.`)
      }
    } else {
      // TODO add StackFrame!
      throw new Error(`Unable to find factory function ${factoryFunctionName} in scope.`)
    }
  }
  // This is disgusting, but we want to make sure that if there is only one child it isn't wrapped in an array,
  // since that code that uses `React.Children.only`
  const childrenToRender = children.map((innerChildren) =>
    innerChildren != null && Array.isArray(innerChildren) && innerChildren.length === 1
      ? innerChildren[0]
      : innerChildren,
  )
  return factoryFunction.call(null, type, fixedProps, ...childrenToRender)
}

function fixStyleObjectRemoveCommentOnlyValues(props: Readonly<unknown>): any {
  if (typeof props === 'object' && 'style' in props) {
    const propsAsAny = props as any
    const style = propsAsAny.style
    const fixedStyle: any = objectMap((styleProp) => {
      if (typeof styleProp === 'string' && cssValueOnlyContainsComments(styleProp)) {
        /**
         * see https://github.com/facebook/react/issues/19477
         * our problem: we store the disabled style values as commented out,
         * and we allow a style prop to only contain commented out values.
         *
         * This is fine if you render something from scratch, so this will never be an issue for our users.
         *
         * But in the case of the canvas, when you change the value from _something_ to _only comments_,
         * we expect the DOM API to clear out the previous value. The real behavior however is to ignore the comments-only new value,
         * and keep the old value alive.
         *
         * Solution: we explicitly mange the style prop such that if a property only contains comments, we replace it with a `null`,
         * which the DOM API will treat as "remove existing value" as expected.
         *
         * Example: { backgroundColor: '\/*red*\/ \/*green*\/' } should disable the backgroundColor, so we will
         * replace it with { backgroundColor: null } in the Canvas.
         */
        return null
      } else {
        return styleProp
      }
    }, style)
    return {
      ...propsAsAny,
      style: fixedStyle,
    }
  } else {
    // no style props, just return the props object without mangling
    return props
  }
}

function streamlineInFileBlobs(props: any, fileBlobs: UIFileBase64Blobs): any {
  if (typeof props === 'object' && !Array.isArray(props) && props !== null) {
    const elementID = props['data-uid']
    if (elementID in fileBlobs) {
      return {
        ...props,
        src: `data:;base64,${fileBlobs[elementID].base64}`,
      }
    } else {
      return props
    }
  } else {
    return props
  }
}
