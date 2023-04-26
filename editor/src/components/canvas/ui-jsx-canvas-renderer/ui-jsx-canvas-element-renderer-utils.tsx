import React from 'react'
import { MapLike } from 'typescript'
import {
  UTOPIA_PATH_KEY,
  UTOPIA_SCENE_ID_KEY,
  UTOPIA_INSTANCE_PATH,
  UTOPIA_UID_KEY,
  UTOPIA_UID_ORIGINAL_PARENTS_KEY,
} from '../../../core/model/utopia-constants'
import { flatMapEither, forEachRight } from '../../../core/shared/either'
import {
  JSXElementChild,
  isJSXElement,
  JSXElement,
  jsExpressionValue,
  ElementsWithin,
  isIntrinsicElement,
  isIntrinsicHTMLElement,
  JSXArbitraryBlock,
  emptyComments,
  jsxTextBlock,
  isJSXFragment,
  JSXElementLike,
  isJSXArbitraryBlock,
} from '../../../core/shared/element-template'
import {
  getAccumulatedElementsWithin,
  jsxAttributesToProps,
  jsxAttributeToValue,
  setJSXValueAtPath,
} from '../../../core/shared/jsx-attributes'
import {
  ElementPath,
  HighlightBoundsForUids,
  Imports,
} from '../../../core/shared/project-file-types'
import { assertNever, fastForEach, NO_OP } from '../../../core/shared/utils'
import { Utils } from '../../../uuiui-deps'
import { UIFileBase64Blobs } from '../../editor/store/editor-state'
import { DomWalkerInvalidatePathsCtxData, UiJsxCanvasContextData } from '../ui-jsx-canvas'
import { SceneComponent } from './scene-component'
import * as PP from '../../../core/shared/property-path'
import * as EP from '../../../core/shared/element-path'
import { resolveParamsAndRunJsCode } from '../../../core/shared/javascript-cache'
import { objectMap } from '../../../core/shared/object-utils'
import { cssValueOnlyContainsComments } from '../../../printer-parsers/css/css-parser-utils'
import { filterDataProps } from '../../../utils/canvas-react-utils'
import {
  addFakeSpyEntry,
  buildSpyWrappedElement,
  clearOpposingConditionalSpyValues,
} from './ui-jsx-canvas-spy-wrapper'
import { createIndexedUid, getUtopiaID } from '../../../core/shared/uid-utils'
import { isComponentRendererComponent } from './ui-jsx-canvas-component-renderer'
import { optionalMap } from '../../../core/shared/optional-utils'
import { canvasMissingJSXElementError } from './canvas-render-errors'
import { importedFromWhere } from '../../editor/import-utils'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/shared/dom-utils'
import { TextEditorWrapper, unescapeHTML } from '../../text-editor/text-editor'
import {
  findUtopiaCommentFlag,
  isUtopiaCommentFlagConditional,
} from '../../../core/shared/comment-flags'

export function createLookupRender(
  elementPath: ElementPath | null,
  rootScope: MapLike<any>,
  parentComponentInputProps: MapLike<any>,
  requireResult: MapLike<any>,
  hiddenInstances: Array<ElementPath>,
  displayNoneInstances: Array<ElementPath>,
  fileBlobs: UIFileBase64Blobs,
  validPaths: Set<ElementPath>,
  reactChildren: React.ReactNode | undefined,
  metadataContext: UiJsxCanvasContextData,
  updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData,
  jsxFactoryFunctionName: string | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  filePath: string,
  imports: Imports,
  code: string,
  highlightBounds: HighlightBoundsForUids | null,
  editedText: ElementPath | null,
): (element: JSXElement, scope: MapLike<any>) => React.ReactChild {
  let index = 0

  return (element: JSXElement, scope: MapLike<any>): React.ReactChild => {
    index++
    const innerUID = getUtopiaID(element)
    const generatedUID = createIndexedUid(innerUID, index)
    const withGeneratedUID = setJSXValueAtPath(
      element.props,
      PP.create('data-uid'),
      jsExpressionValue(generatedUID, emptyComments),
    )

    // TODO BALAZS should this be here? or should the arbitrary block never have a template path with that last generated element?
    const elementPathWithoutTheLastElementBecauseThatsAWeirdGeneratedUID = optionalMap(
      EP.parentPath,
      elementPath,
    )

    const innerPath = optionalMap(
      (path) => EP.appendToPath(path, generatedUID),
      elementPathWithoutTheLastElementBecauseThatsAWeirdGeneratedUID,
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
      displayNoneInstances,
      fileBlobs,
      validPaths,
      generatedUID,
      reactChildren,
      metadataContext,
      updateInvalidatedPaths,
      jsxFactoryFunctionName,
      null,
      shouldIncludeCanvasRootInTheSpy,
      filePath,
      imports,
      code,
      highlightBounds,
      editedText,
    )
  }
}

function monkeyUidProp(uid: string | undefined, propsToUpdate: MapLike<any>): MapLike<any> {
  let monkeyedProps: MapLike<any> = {
    ...propsToUpdate,
  }

  const uidFromProps = monkeyedProps[UTOPIA_UID_KEY]
  const uidToPass = uidFromProps ?? uid
  monkeyedProps[UTOPIA_UID_KEY] = uidToPass

  return monkeyedProps
}

function NoOpLookupRender(element: JSXElement, scope: MapLike<any>): React.ReactChild {
  throw new Error(
    `Utopia Error: createLookupRender was not used properly for element: ${element.name.baseVariable}`,
  )
}

export function renderCoreElement(
  element: JSXElementChild,
  elementPath: ElementPath | null,
  rootScope: MapLike<any>,
  inScope: MapLike<any>,
  parentComponentInputProps: MapLike<any>,
  requireResult: MapLike<any>,
  hiddenInstances: Array<ElementPath>,
  displayNoneInstances: Array<ElementPath>,
  fileBlobs: UIFileBase64Blobs,
  validPaths: Set<ElementPath>,
  uid: string | undefined,
  reactChildren: React.ReactNode | undefined,
  metadataContext: UiJsxCanvasContextData,
  updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData,
  jsxFactoryFunctionName: string | null,
  codeError: Error | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  filePath: string,
  imports: Imports,
  code: string,
  highlightBounds: HighlightBoundsForUids | null,
  editedText: ElementPath | null,
): React.ReactChild {
  if (codeError != null) {
    throw codeError
  }

  switch (element.type) {
    case 'JSX_ELEMENT': {
      const elementsWithinProps = getAccumulatedElementsWithin(element.props)

      const anyElementsWithin = Object.keys(elementsWithinProps).length > 0

      const innerRender = anyElementsWithin
        ? createLookupRender(
            elementPath,
            rootScope,
            parentComponentInputProps,
            requireResult,
            hiddenInstances,
            displayNoneInstances,
            fileBlobs,
            validPaths,
            reactChildren,
            metadataContext,
            updateInvalidatedPaths,
            jsxFactoryFunctionName,
            shouldIncludeCanvasRootInTheSpy,
            filePath,
            imports,
            code,
            highlightBounds,
            editedText,
          )
        : NoOpLookupRender

      const blockScope = anyElementsWithin
        ? {
            ...inScope,
            [JSX_CANVAS_LOOKUP_FUNCTION_NAME]: utopiaCanvasJSXLookup(
              elementsWithinProps,
              inScope,
              innerRender,
            ),
          }
        : inScope

      const assembledProps = jsxAttributesToProps(
        filePath,
        blockScope,
        element.props,
        requireResult,
      )

      const passthroughProps = monkeyUidProp(uid, assembledProps)

      const key = optionalMap(EP.toString, elementPath) ?? passthroughProps[UTOPIA_UID_KEY]

      return renderJSXElement(
        key,
        element,
        elementPath,
        parentComponentInputProps,
        requireResult,
        rootScope,
        inScope,
        hiddenInstances,
        displayNoneInstances,
        fileBlobs,
        validPaths,
        passthroughProps,
        metadataContext,
        updateInvalidatedPaths,
        jsxFactoryFunctionName,
        null,
        shouldIncludeCanvasRootInTheSpy,
        filePath,
        imports,
        code,
        highlightBounds,
        editedText,
      )
    }
    case 'ATTRIBUTE_OTHER_JAVASCRIPT': {
      const innerRender = createLookupRender(
        elementPath,
        rootScope,
        parentComponentInputProps,
        requireResult,
        hiddenInstances,
        displayNoneInstances,
        fileBlobs,
        validPaths,
        reactChildren,
        metadataContext,
        updateInvalidatedPaths,
        jsxFactoryFunctionName,
        shouldIncludeCanvasRootInTheSpy,
        filePath,
        imports,
        code,
        highlightBounds,
        editedText,
      )

      const blockScope = {
        ...inScope,
        [JSX_CANVAS_LOOKUP_FUNCTION_NAME]: utopiaCanvasJSXLookup(
          element.elementsWithin,
          inScope,
          innerRender,
        ),
      }
      return runJSXArbitraryBlock(filePath, requireResult, element, blockScope)
    }
    case 'JSX_FRAGMENT': {
      const key = optionalMap(EP.toString, elementPath) ?? element.uid

      return renderJSXElement(
        key,
        element,
        elementPath,
        parentComponentInputProps,
        requireResult,
        rootScope,
        inScope,
        hiddenInstances,
        displayNoneInstances,
        fileBlobs,
        validPaths,
        [],
        metadataContext,
        updateInvalidatedPaths,
        jsxFactoryFunctionName,
        null,
        shouldIncludeCanvasRootInTheSpy,
        filePath,
        imports,
        code,
        highlightBounds,
        editedText,
      )
    }
    case 'JSX_TEXT_BLOCK': {
      const parentPath = Utils.optionalMap(EP.parentPath, elementPath)
      // when the text is just edited its parent renders it in a text editor, so no need to render anything here
      if (parentPath != null && EP.pathsEqual(parentPath, editedText)) {
        return <></>
      }

      const lines = element.text.split('<br />').map((line) => unescapeHTML(line))
      return (
        <>
          {lines.map((l, index) => (
            <React.Fragment key={index}>
              {l}
              {index < lines.length - 1 ? <br /> : null}
            </React.Fragment>
          ))}
        </>
      )
    }
    case 'JSX_CONDITIONAL_EXPRESSION': {
      const commentFlag = findUtopiaCommentFlag(element.comments, 'conditional')
      const override = isUtopiaCommentFlagConditional(commentFlag) ? commentFlag.value : null
      const defaultConditionValueAsAny = jsxAttributeToValue(
        filePath,
        inScope,
        requireResult,
        element.condition,
      )
      // Coerce `defaultConditionValueAsAny` to a value that is definitely a boolean, not something that is truthy.
      // eslint-disable-next-line @typescript-eslint/strict-boolean-expressions
      const defaultConditionValue: boolean = !!defaultConditionValueAsAny
      const activeConditionValue = override ?? defaultConditionValue
      const actualElement = activeConditionValue ? element.whenTrue : element.whenFalse

      if (elementPath != null) {
        clearOpposingConditionalSpyValues(
          metadataContext,
          element,
          activeConditionValue,
          elementPath,
        )

        addFakeSpyEntry(metadataContext, elementPath, element, filePath, imports, {
          active: activeConditionValue,
          default: defaultConditionValue,
        })
      }

      if (isJSXArbitraryBlock(actualElement)) {
        return jsxAttributeToValue(filePath, inScope, requireResult, actualElement)
      } else {
        const childPath = optionalMap(
          (path) => EP.appendToPath(path, getUtopiaID(actualElement)),
          elementPath,
        )

        return renderCoreElement(
          actualElement,
          childPath,
          rootScope,
          inScope,
          parentComponentInputProps,
          requireResult,
          hiddenInstances,
          displayNoneInstances,
          fileBlobs,
          validPaths,
          uid,
          reactChildren,
          metadataContext,
          updateInvalidatedPaths,
          jsxFactoryFunctionName,
          codeError,
          shouldIncludeCanvasRootInTheSpy,
          filePath,
          imports,
          code,
          highlightBounds,
          editedText,
        )
      }
    }
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
      return jsxAttributeToValue(filePath, inScope, requireResult, element)
    default:
      const _exhaustiveCheck: never = element
      throw new Error(`Unhandled type ${JSON.stringify(element)}`)
  }
}

function trimAndJoinTextFromJSXElements(elements: Array<JSXElementChild>): string | null {
  let combinedText = ''
  for (let i = 0; i < elements.length; i++) {
    const c = elements[i]
    switch (c.type) {
      case 'JSX_TEXT_BLOCK':
        combinedText += trimWhitespaces(c.text, elements[i - 1] ?? null, elements[i + 1] ?? null)
        break
      case 'JSX_ELEMENT':
        if (c.name.baseVariable === 'br') {
          combinedText += '\n'
        }
        break
      case 'ATTRIBUTE_OTHER_JAVASCRIPT':
        if (c.transpiledJavascript === `return ${c.javascript}`) {
          combinedText += `{${c.originalJavascript}}`
        }
        break
      case 'JSX_FRAGMENT':
      case 'JSX_CONDITIONAL_EXPRESSION':
        break
      case 'ATTRIBUTE_VALUE':
        if (typeof c.value === 'string') {
          combinedText += c.value
        }
        break
      case 'ATTRIBUTE_NESTED_ARRAY':
      case 'ATTRIBUTE_NESTED_OBJECT':
      case 'ATTRIBUTE_FUNCTION_CALL':
        break
      default:
        assertNever(c)
    }
  }
  return combinedText
}

function trimWhitespaces(
  text: string,
  elementBefore: JSXElementChild | null,
  elementAfter: JSXElementChild | null,
): string {
  if (text.length === 0) {
    return ''
  }

  const trimmedText = text
    // split around all whitespaces, we don't want to keep newlines or repeated spaces
    .split(/\s/)
    // empty strings will appear between repeated whitespaces, we can ignore them
    .filter((s) => s.length > 0)
    // join back everything with a single space
    .join(' ')

  // when the text has a leading whitespace and there is an arbitrary block before that, we need to keep the whitespace
  const keepSpaceBefore = text[0] === ' ' && elementBefore?.type === 'ATTRIBUTE_OTHER_JAVASCRIPT'
  // when the text has an trailing whitespace and there is an arbitrary block after that, we need to keep the whitespace
  const keepSpaceAfter =
    text[text.length - 1] === ' ' && elementAfter?.type === 'ATTRIBUTE_OTHER_JAVASCRIPT'

  if (keepSpaceBefore && keepSpaceAfter) {
    return ' ' + trimmedText + ' '
  }

  if (keepSpaceAfter) {
    return trimmedText + ' '
  }
  if (keepSpaceBefore) {
    return ' ' + trimmedText
  }

  return trimmedText
}

function renderJSXElement(
  key: string,
  jsx: JSXElementLike,
  elementPath: ElementPath | null,
  parentComponentInputProps: MapLike<any>,
  requireResult: MapLike<any>,
  rootScope: MapLike<any>,
  inScope: MapLike<any>,
  hiddenInstances: Array<ElementPath>,
  displayNoneInstances: Array<ElementPath>,
  fileBlobs: UIFileBase64Blobs,
  validPaths: Set<ElementPath>,
  passthroughProps: MapLike<any>,
  metadataContext: UiJsxCanvasContextData,
  updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData,
  jsxFactoryFunctionName: string | null,
  codeError: Error | null,
  shouldIncludeCanvasRootInTheSpy: boolean,
  filePath: string,
  imports: Imports,
  code: string,
  highlightBounds: HighlightBoundsForUids | null,
  editedText: ElementPath | null,
): React.ReactElement {
  const createChildrenElement = (child: JSXElementChild): React.ReactChild => {
    const childPath = optionalMap((path) => EP.appendToPath(path, getUtopiaID(child)), elementPath)
    return renderCoreElement(
      child,
      childPath,
      rootScope,
      inScope,
      parentComponentInputProps,
      requireResult,
      hiddenInstances,
      displayNoneInstances,
      fileBlobs,
      validPaths,
      undefined,
      undefined,
      metadataContext,
      updateInvalidatedPaths,
      jsxFactoryFunctionName,
      codeError,
      shouldIncludeCanvasRootInTheSpy,
      filePath,
      imports,
      code,
      highlightBounds,
      editedText,
    )
  }

  const elementIsIntrinsic = isJSXElement(jsx) && isIntrinsicElement(jsx.name)
  const elementIsFragment = isJSXFragment(jsx)
  const elementIsBaseHTML = elementIsIntrinsic && isIntrinsicHTMLElement(jsx.name)
  const elementInScope = elementIsIntrinsic ? null : getElementFromScope(jsx, inScope)
  const elementFromImport = elementIsIntrinsic ? null : getElementFromScope(jsx, requireResult)
  const elementFromScopeOrImport = Utils.defaultIfNull(elementFromImport, elementInScope)
  const elementIsTextEdited = elementPath != null && EP.pathsEqual(elementPath, editedText)
  const elementIsTextEditedAndNoTextBlockChild =
    elementIsTextEdited && jsx.children.every((c) => c.type !== 'JSX_TEXT_BLOCK')

  const childrenWithNewTextBlock = elementIsTextEditedAndNoTextBlockChild
    ? [...jsx.children, jsxTextBlock('')]
    : jsx.children
  const childrenElements = childrenWithNewTextBlock.map(createChildrenElement)

  // Not necessary to check the top level elements, as we'll use a comparison of the
  // elements from scope and import to confirm it's not a top level element.
  const importedFrom = elementIsFragment
    ? null
    : importedFromWhere(filePath, jsx.name.baseVariable, [], imports)
  const elementIsScene =
    !elementIsIntrinsic &&
    importedFrom != null &&
    importedFrom.type === 'IMPORTED_ORIGIN' && // Imported and not from the same file.
    importedFrom.filePath === 'utopia-api' && // Originating from `utopia-api`
    importedFrom.exportedName === 'Scene' && // `Scene` component.
    elementFromImport === elementInScope // Ensures this is not a user defined component with the same name.
  const elementOrScene = elementIsScene ? SceneComponent : elementFromScopeOrImport

  const FinalElement = elementIsIntrinsic ? jsx.name.baseVariable : elementOrScene
  const FinalElementOrFragment = elementIsFragment ? React.Fragment : FinalElement

  let elementProps = { key: key, ...passthroughProps }
  if (!elementIsFragment) {
    if (isHidden(hiddenInstances, elementPath)) {
      elementProps = hideElement(elementProps)
    }
    if (elementIsDisplayNone(displayNoneInstances, elementPath)) {
      elementProps = displayNoneElement(elementProps)
    }
    elementProps = streamlineInFileBlobs(elementProps, fileBlobs)
  }

  const elementPropsWithScenePath = isComponentRendererComponent(FinalElement)
    ? { ...elementProps, [UTOPIA_INSTANCE_PATH]: elementPath }
    : elementProps

  const elementPropsWithSceneID =
    elementIsScene && elementPath != null
      ? { ...elementPropsWithScenePath, [UTOPIA_SCENE_ID_KEY]: EP.toString(elementPath) }
      : elementPropsWithScenePath

  const propsIncludingElementPath = {
    ...elementPropsWithSceneID,
    [UTOPIA_PATH_KEY]: optionalMap(EP.toString, elementPath),
  }

  const looksLikeReactIntrinsicButNotHTML = elementIsIntrinsic && !elementIsBaseHTML

  const finalProps =
    looksLikeReactIntrinsicButNotHTML || elementIsFragment
      ? filterDataProps(propsIncludingElementPath)
      : propsIncludingElementPath

  if (!elementIsFragment && FinalElement == null) {
    throw canvasMissingJSXElementError(jsxFactoryFunctionName, code, jsx, filePath, highlightBounds)
  }

  if (elementPath != null && validPaths.has(EP.makeLastPartOfPathStatic(elementPath))) {
    if (elementIsTextEdited) {
      const text = trimAndJoinTextFromJSXElements(childrenWithNewTextBlock)
      const textContent = unescapeHTML(text ?? '')
      const textEditorProps = {
        elementPath: elementPath,
        filePath: filePath,
        text: textContent,
        component: FinalElement,
        passthroughProps: finalProps,
      }

      return buildSpyWrappedElement(
        jsx,
        textEditorProps,
        elementPath,
        metadataContext,
        updateInvalidatedPaths,
        childrenElements,
        TextEditorWrapper,
        inScope,
        jsxFactoryFunctionName,
        shouldIncludeCanvasRootInTheSpy,
        imports,
        filePath,
      )
    }
    return buildSpyWrappedElement(
      jsx,
      finalProps,
      elementPath,
      metadataContext,
      updateInvalidatedPaths,
      childrenElements,
      FinalElementOrFragment,
      inScope,
      jsxFactoryFunctionName,
      shouldIncludeCanvasRootInTheSpy,
      imports,
      filePath,
    )
  } else {
    return renderComponentUsingJsxFactoryFunction(
      inScope,
      jsxFactoryFunctionName,
      FinalElementOrFragment,
      finalProps,
      ...childrenElements,
    )
  }
}

function isHidden(hiddenInstances: ElementPath[], elementPath: ElementPath | null): boolean {
  return (
    elementPath != null &&
    hiddenInstances.some((path) => EP.isDescendantOfOrEqualTo(elementPath, path))
  )
}
function elementIsDisplayNone(
  displayNoneInstances: ElementPath[],
  elementPath: ElementPath | null,
): boolean {
  return (
    elementPath != null &&
    displayNoneInstances.some((path) => EP.isDescendantOfOrEqualTo(elementPath, path))
  )
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

function displayNoneElement(props: any): any {
  const styleProps = Utils.propOr({}, 'style', props as any)
  return {
    ...props,
    style: {
      ...styleProps,
      display: 'none',
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
  filePath: string,
  requireResult: MapLike<any>,
  block: JSXArbitraryBlock,
  currentScope: MapLike<any>,
): any {
  switch (block.type) {
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
      return jsxAttributeToValue(filePath, block, requireResult, block)
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return resolveParamsAndRunJsCode(filePath, block, requireResult, currentScope)
    default:
      assertNever(block)
  }
}

function getElementFromScope(jsxElementToLookup: JSXElementLike, scope: MapLike<any> | null): any {
  if (scope == null || isJSXFragment(jsxElementToLookup)) {
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
  return factoryFunction.call(null, type, fixedProps, ...children)
}

function fixStyleObjectRemoveCommentOnlyValues(props: Readonly<unknown>): any {
  if (typeof props === 'object' && 'style' in props && (props as any)['style'] != null) {
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
