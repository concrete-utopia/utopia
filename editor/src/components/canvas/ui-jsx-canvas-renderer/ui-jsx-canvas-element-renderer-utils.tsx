import * as React from 'react'
import { MapLike } from 'typescript'
import { getUtopiaID } from '../../../core/model/element-template-utils'
import { isSceneElement, PathForResizeContent } from '../../../core/model/scene-utils'
import {
  UTOPIA_ORIGINAL_ID_KEY,
  UTOPIA_UID_KEY,
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
} from '../../../core/shared/element-template'
import { jsxAttributesToProps, setJSXValueAtPath } from '../../../core/shared/jsx-attributes'
import { InstancePath, TemplatePath } from '../../../core/shared/project-file-types'
import { fastForEach } from '../../../core/shared/utils'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/workers/parser-printer/parser-printer-utils'
import { Utils } from '../../../uuiui-deps'
import { UIFileBase64Blobs } from '../../editor/store/editor-state'
import { UiJsxCanvasContextData } from '../ui-jsx-canvas'
import { SceneRootRenderer } from './scene-root'
import * as PP from '../../../core/shared/property-path'
import * as TP from '../../../core/shared/template-path'
import { Storyboard } from 'utopia-api'
import { resolveParamsAndRunJsCode } from '../../../core/shared/javascript-cache'
import { objectMap } from '../../../core/shared/object-utils'
import { cssValueOnlyContainsComments } from '../../../printer-parsers/css/css-parser-utils'
import { filterDataProps } from '../../../utils/canvas-react-utils'
import { buildSpyWrappedElement } from './ui-jsx-canvas-spy-wrapper'
import { createIndexedUid } from '../../../core/shared/uid-utils'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'

export function renderCoreElement(
  element: JSXElementChild,
  templatePath: InstancePath,
  rootScope: MapLike<any>,
  inScope: MapLike<any>,
  parentComponentInputProps: MapLike<any>,
  requireResult: MapLike<any>,
  hiddenInstances: Array<TemplatePath>,
  fileBlobs: UIFileBase64Blobs,
  validPaths: Array<InstancePath>,
  uid: string | undefined,
  reactChildren: React.ReactNode | undefined,
  metadataContext: UiJsxCanvasContextData,
  jsxFactoryFunctionName: string | null,
  codeError: Error | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  temporarySceneTemplatePath: TemplatePath | null,
): React.ReactElement {
  if (codeError != null) {
    throw codeError
  }
  if (isJSXElement(element) && isSceneElement(element)) {
    return <SceneRootRenderer sceneElement={element} />
  }
  switch (element.type) {
    case 'JSX_ELEMENT': {
      const assembledProps = jsxAttributesToProps(inScope, element.props, requireResult)

      let passthroughProps: MapLike<any> = {
        ...assembledProps,
      }

      const uidForProps = Utils.defaultIfNull(assembledProps[UTOPIA_UID_KEY], uid)
      if (uidForProps != null) {
        passthroughProps[UTOPIA_UID_KEY] = uidForProps
      }

      const originalIDForProps = Utils.defaultIfNull(
        assembledProps[UTOPIA_ORIGINAL_ID_KEY],
        parentComponentInputProps[UTOPIA_ORIGINAL_ID_KEY],
      )
      if (originalIDForProps != null) {
        passthroughProps[UTOPIA_ORIGINAL_ID_KEY] = originalIDForProps
      }

      return renderJSXElement(
        TP.toString(templatePath),
        element,
        templatePath,
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
        temporarySceneTemplatePath,
      )
    }
    case 'JSX_ARBITRARY_BLOCK': {
      let innerIndex: number = 0
      function innerRender(
        innerElement: JSXElement,
        innerInScope: MapLike<any>,
      ): React.ReactElement {
        innerIndex++
        const innerPath = TP.appendToPath(templatePath, `index-${innerIndex}`)

        const innerUID = getUtopiaID(innerElement)
        const withOriginalID = setJSXValueAtPath(
          innerElement.props,
          PP.create([UTOPIA_ORIGINAL_ID_KEY]),
          jsxAttributeValue(innerUID, emptyComments),
        )
        const generatedUID = createIndexedUid(innerUID, innerIndex)
        const withGeneratedUID = flatMapEither(
          (attrs) =>
            setJSXValueAtPath(
              attrs,
              PP.create(['data-uid']),
              jsxAttributeValue(generatedUID, emptyComments),
            ),
          withOriginalID,
        )

        let augmentedInnerElement = innerElement
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
          innerInScope,
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
          temporarySceneTemplatePath,
        )
      }
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
      let renderedChildren: Array<React.ReactElement> = []
      fastForEach(element.children, (child) => {
        const renderResult = renderCoreElement(
          child,
          templatePath,
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
          temporarySceneTemplatePath,
        )
        renderedChildren.push(renderResult)
      })
      return <>{renderedChildren}</>
    }
    case 'JSX_TEXT_BLOCK': {
      // JSXTextBlock is the final remaining case.
      return renderComponentUsingJsxFactoryFunction(
        inScope,
        jsxFactoryFunctionName,
        React.Fragment,
        { key: TP.toString(templatePath) },
        element.text,
      )
    }
    default:
      const _exhaustiveCheck: never = element
      throw new Error(`Unhandled type ${JSON.stringify(element)}`)
  }
}

function renderJSXElement(
  key: string,
  jsx: JSXElement,
  templatePath: InstancePath,
  parentComponentInputProps: MapLike<any>,
  requireResult: MapLike<any>,
  rootScope: MapLike<any>,
  inScope: MapLike<any>,
  hiddenInstances: Array<TemplatePath>,
  fileBlobs: UIFileBase64Blobs,
  validPaths: Array<InstancePath>,
  passthroughProps: MapLike<any>,
  metadataContext: UiJsxCanvasContextData,
  jsxFactoryFunctionName: string | null,
  codeError: Error | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  temporarySceneTemplatePath: TemplatePath | null,
): React.ReactElement {
  let elementProps = { key: key, ...passthroughProps }
  if (isHidden(hiddenInstances, templatePath)) {
    elementProps = hideElement(elementProps)
  }
  elementProps = streamlineInFileBlobs(elementProps, fileBlobs)

  const createChildrenElement = (
    child: JSXElementChild,
  ): React.ReactElement | Array<React.ReactElement> => {
    const childPath = TP.appendToPath(templatePath, getUtopiaID(child))
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
      temporarySceneTemplatePath,
    )
  }

  const childrenElements = jsx.children.map(createChildrenElement)
  const ElementInScope = getElementFromScope(jsx, inScope)
  const ElementFromImport = getElementFromScope(jsx, requireResult)
  const ElementFromScopeOrImport = Utils.defaultIfNull(ElementFromImport, ElementInScope)
  const elementIsIntrinsic = ElementFromScopeOrImport == null && isIntrinsicElement(jsx.name)
  const elementIsBaseHTML = elementIsIntrinsic && isIntrinsicHTMLElement(jsx.name)
  const FinalElement = elementIsIntrinsic ? jsx.name.baseVariable : ElementFromScopeOrImport
  const finalProps =
    elementIsIntrinsic && !elementIsBaseHTML ? filterDataProps(elementProps) : elementProps

  if (FinalElement != null && TP.containsPath(templatePath, validPaths)) {
    let childrenTemplatePaths: InstancePath[] = []

    Utils.fastForEach(jsx.children, (child) => {
      if (isJSXElement(child)) {
        const childPath = TP.appendToPath(templatePath, getUtopiaID(child))
        if (TP.containsPath(childPath, validPaths)) {
          childrenTemplatePaths.push(childPath)
        }
      }
    })

    return buildSpyWrappedElement(
      jsx,
      finalProps,
      templatePath,
      metadataContext,
      childrenTemplatePaths,
      childrenElements,
      FinalElement,
      inScope,
      jsxFactoryFunctionName,
      shouldIncludeCanvasRootInTheSpy,
      temporarySceneTemplatePath,
    )
  } else {
    const childrenOrNull = childrenElements.length !== 0 ? childrenElements : null
    return renderComponentUsingJsxFactoryFunction(
      inScope,
      jsxFactoryFunctionName,
      FinalElement,
      finalProps,
      childrenOrNull,
    )
  }
}

function isHidden(hiddenInstances: TemplatePath[], templatePath: TemplatePath): boolean {
  return hiddenInstances.some((path) => TP.pathsEqual(path, templatePath))
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

function utopiaCanvasJSXLookup(
  elementsWithin: ElementsWithin,
  executionScope: MapLike<any>,
  render: (element: JSXElement, inScope: MapLike<any>) => React.ReactElement,
): (uid: string, inScope: MapLike<any>) => React.ReactElement | null {
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
    // TODO SCENES remove this when the Scene metadata work is finished
    // this is now needed, otherwise the Storyboard needs to be imported to the ui js file, but the linter will show warnings
    if (jsxElementToLookup.name.baseVariable === 'Storyboard') {
      return Storyboard
    } else if (jsxElementToLookup.name.baseVariable in scope) {
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
      factoryFunction = inScope[factoryFunctionName]
    } else {
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
