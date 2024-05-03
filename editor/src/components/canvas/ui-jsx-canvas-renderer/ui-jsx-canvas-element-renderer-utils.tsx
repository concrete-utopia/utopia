import React from 'react'
import type { MapLike } from 'typescript'
import {
  UTOPIA_PATH_KEY,
  UTOPIA_SCENE_ID_KEY,
  UTOPIA_INSTANCE_PATH,
  UTOPIA_UID_KEY,
} from '../../../core/model/utopia-constants'
import { forEachRight } from '../../../core/shared/either'
import type {
  JSXElementChild,
  JSXElement,
  ElementsWithin,
  JSExpression,
  JSXElementLike,
  JSIdentifier,
  JSPropertyAccess,
  JSElementAccess,
} from '../../../core/shared/element-template'
import {
  isJSXElement,
  jsExpressionValue,
  isIntrinsicElement,
  isIntrinsicHTMLElement,
  emptyComments,
  jsxTextBlock,
  isJSXFragment,
  isJSExpression,
  isJSXMapExpression,
  propertiesExposedByParam,
  propertiesExposedByParams,
  hasElementsWithin,
} from '../../../core/shared/element-template'
import {
  getAccumulatedElementsWithin,
  jsxAttributesToProps,
  jsxAttributeToValue,
} from '../../../core/shared/jsx-attributes'
import { setJSXValueAtPath } from '../../../core/shared/jsx-attribute-utils'
import type {
  ElementPath,
  HighlightBoundsForUids,
  Imports,
} from '../../../core/shared/project-file-types'
import { assertNever } from '../../../core/shared/utils'
import { Utils } from '../../../uuiui-deps'
import type { UIFileBase64Blobs } from '../../editor/store/editor-state'
import type {
  DomWalkerInvalidatePathsCtxData,
  UiJsxCanvasContextData,
  VariableData,
} from '../ui-jsx-canvas'
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
import { getUtopiaID } from '../../../core/shared/uid-utils'
import { isComponentRendererComponent } from './component-renderer-component'
import { optionalMap } from '../../../core/shared/optional-utils'
import { canvasMissingJSXElementError } from './canvas-render-errors'
import { importedFromWhere } from '../../editor/import-utils'
import { JSX_CANVAS_LOOKUP_FUNCTION_NAME } from '../../../core/shared/dom-utils'
import type { TextEditorProps } from '../../text-editor/text-editor'
import { TextEditorWrapper, unescapeHTML } from '../../text-editor/text-editor'
import {
  findUtopiaCommentFlag,
  isUtopiaCommentFlagConditional,
  isUtopiaCommentFlagMapCount,
} from '../../../core/shared/comment-flags'
import { RemixSceneComponent } from './remix-scene-component'
import { STEGANOGRAPHY_ENABLED, isFeatureEnabled } from '../../../utils/feature-switches'
import { jsxElementChildToText } from './jsx-element-child-to-text'

export interface RenderContext {
  rootScope: MapLike<any>
  parentComponentInputProps: MapLike<any>
  requireResult: MapLike<any>
  hiddenInstances: Array<ElementPath>
  displayNoneInstances: Array<ElementPath>
  fileBlobs: UIFileBase64Blobs
  validPaths: Set<string>
  reactChildren: React.ReactNode | undefined
  metadataContext: UiJsxCanvasContextData
  updateInvalidatedPaths: DomWalkerInvalidatePathsCtxData
  jsxFactoryFunctionName: string | null
  shouldIncludeCanvasRootInTheSpy: boolean
  filePath: string
  imports: Imports
  code: string
  highlightBounds: HighlightBoundsForUids | null
  editedText: ElementPath | null
  variablesInScope: VariableData
}

export function createLookupRender(
  elementPath: ElementPath | null,
  context: RenderContext,
  renderLimit: number | null,
  valuesInScopeFromParameters: Array<string>,
  assignedToProp: string | null,
): (element: JSXElement, scope: MapLike<any>) => React.ReactChild | null {
  let index = 0

  return (element: JSXElement, scope: MapLike<any>): React.ReactChild | null => {
    index++
    if (renderLimit != null && index > renderLimit) {
      return null
    }
    const innerUID = getUtopiaID(element)
    const generatedUID = EP.createIndexedUid(innerUID, index)
    const withGeneratedUID = setJSXValueAtPath(
      element.props,
      PP.create('data-uid'),
      jsExpressionValue(generatedUID, emptyComments),
    )

    const innerPath = optionalMap((path) => EP.appendToPath(path, generatedUID), elementPath)

    let augmentedInnerElement = element
    forEachRight(withGeneratedUID, (attrs) => {
      augmentedInnerElement = {
        ...augmentedInnerElement,
        props: attrs,
      }
    })

    let innerVariablesInScope: VariableData = {
      ...context.variablesInScope,
    }
    for (const valueInScope of valuesInScopeFromParameters) {
      innerVariablesInScope[valueInScope] = {
        spiedValue: scope[valueInScope],
        insertionCeiling: innerPath,
      }
    }

    return renderCoreElement(
      augmentedInnerElement,
      innerPath,
      scope,
      { ...context, variablesInScope: innerVariablesInScope },
      generatedUID,
      null,
      assignedToProp,
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
  inScope: MapLike<any>,
  renderContext: RenderContext,
  uid: string | undefined,
  codeError: Error | null,
  assignedToProp: string | null,
): React.ReactChild {
  const {
    requireResult,
    validPaths,
    metadataContext,
    updateInvalidatedPaths,
    jsxFactoryFunctionName,
    shouldIncludeCanvasRootInTheSpy,
    filePath,
    imports,
    editedText,
    variablesInScope,
  } = renderContext
  if (codeError != null) {
    throw codeError
  }

  switch (element.type) {
    case 'JSX_ELEMENT': {
      const elementsWithinProps = getAccumulatedElementsWithin(element.props)

      const anyElementsWithin = Object.keys(elementsWithinProps).length > 0

      const innerRender = anyElementsWithin
        ? createLookupRender(elementPath, renderContext, null, [], assignedToProp)
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
        blockScope,
        element.props,
        elementPath,
        renderContext,
        uid,
        codeError,
      )

      const passthroughProps = monkeyUidProp(uid, assembledProps)

      const key = optionalMap(EP.toString, elementPath) ?? passthroughProps[UTOPIA_UID_KEY]

      return renderJSXElement(
        key,
        element,
        elementPath,
        inScope,
        passthroughProps,
        renderContext,
        null, // this null passed as the codeError param is matching the old version of the codebase, but codeError should probably not be passed around anyways as we try to throw it as high as possible
        assignedToProp,
      )
    }
    case 'JSX_MAP_EXPRESSION': {
      const commentFlag = findUtopiaCommentFlag(element.comments, 'map-count')
      const mapCountOverride = isUtopiaCommentFlagMapCount(commentFlag) ? commentFlag.value : null

      const elementIsTextEdited = elementPath != null && EP.pathsEqual(elementPath, editedText)

      if (elementPath != null) {
        addFakeSpyEntry(
          validPaths,
          metadataContext,
          elementPath,
          element,
          filePath,
          imports,
          'not-a-conditional',
          null,
          variablesInScope,
          null,
        )
      }

      const valuesInScopeFromParameters = element.valuesInScopeFromParameters
      const elementsWithin: ElementsWithin = {
        ...(hasElementsWithin(element.valueToMap) ? element.valueToMap.elementsWithin : {}),
        ...(hasElementsWithin(element.mapFunction) ? element.mapFunction.elementsWithin : {}),
      }
      if (elementIsTextEdited) {
        const runJSExpressionLazy = () => {
          const innerRender = createLookupRender(
            elementPath,
            renderContext,
            mapCountOverride,
            valuesInScopeFromParameters,
            null,
          )

          const blockScope = {
            ...inScope,
            [JSX_CANVAS_LOOKUP_FUNCTION_NAME]: utopiaCanvasJSXLookup(
              elementsWithin,
              inScope,
              innerRender,
            ),
          }
          return runJSExpression(
            element,
            elementPath,
            blockScope,
            renderContext,
            uid,
            codeError,
            null,
          )
        }

        const originalTextContent = STEGANOGRAPHY_ENABLED ? runJSExpressionLazy() : null

        const textContent = trimJoinUnescapeTextFromJSXElements([element])
        const textEditorProps: TextEditorProps = {
          elementPath: elementPath,
          filePath: filePath,
          text: textContent,
          originalText: originalTextContent,
          component: React.Fragment,
          passthroughProps: {},
          textProp: 'itself',
        }

        return buildSpyWrappedElement(
          element,
          textEditorProps,
          elementPath,
          metadataContext,
          updateInvalidatedPaths,
          [],
          TextEditorWrapper,
          inScope,
          jsxFactoryFunctionName,
          shouldIncludeCanvasRootInTheSpy,
          imports,
          filePath,
          variablesInScope,
          'real-element',
          null,
        )
      }
      const innerRender = createLookupRender(
        elementPath,
        renderContext,
        mapCountOverride,
        valuesInScopeFromParameters,
        null,
      )

      const blockScope = {
        ...inScope,
        [JSX_CANVAS_LOOKUP_FUNCTION_NAME]: utopiaCanvasJSXLookup(
          elementsWithin,
          inScope,
          innerRender,
        ),
      }
      return runJSExpression(element, elementPath, blockScope, renderContext, uid, codeError, null)
    }
    case 'ATTRIBUTE_OTHER_JAVASCRIPT': {
      const commentFlag = findUtopiaCommentFlag(element.comments, 'map-count')
      const mapCountOverride =
        isJSXMapExpression(element) && isUtopiaCommentFlagMapCount(commentFlag)
          ? commentFlag.value
          : null

      const elementIsTextEdited = elementPath != null && EP.pathsEqual(elementPath, editedText)

      if (elementPath != null) {
        addFakeSpyEntry(
          validPaths,
          metadataContext,
          elementPath,
          element,
          filePath,
          imports,
          'not-a-conditional',
          null,
          variablesInScope,
          assignedToProp,
        )
      }

      const valuesInScopeFromParameters = propertiesExposedByParams(element.params)

      if (elementIsTextEdited) {
        const runJSExpressionLazy = () => {
          const innerRender = createLookupRender(
            elementPath,
            renderContext,
            mapCountOverride,
            valuesInScopeFromParameters,
            assignedToProp,
          )

          const blockScope = {
            ...inScope,
            [JSX_CANVAS_LOOKUP_FUNCTION_NAME]: utopiaCanvasJSXLookup(
              element.elementsWithin,
              inScope,
              innerRender,
            ),
          }
          return runJSExpression(
            element,
            elementPath,
            blockScope,
            renderContext,
            uid,
            codeError,
            assignedToProp,
          )
        }

        const originalTextContent = STEGANOGRAPHY_ENABLED ? runJSExpressionLazy() : null

        const textContent = trimJoinUnescapeTextFromJSXElements([element])
        const textEditorProps: TextEditorProps = {
          elementPath: elementPath,
          filePath: filePath,
          text: textContent,
          originalText: originalTextContent,
          component: React.Fragment,
          passthroughProps: {},
          textProp: 'itself',
        }

        return buildSpyWrappedElement(
          element,
          textEditorProps,
          elementPath,
          metadataContext,
          updateInvalidatedPaths,
          [],
          TextEditorWrapper,
          inScope,
          jsxFactoryFunctionName,
          shouldIncludeCanvasRootInTheSpy,
          imports,
          filePath,
          variablesInScope,
          'real-element',
          assignedToProp,
        )
      }
      const innerRender = createLookupRender(
        elementPath,
        renderContext,
        mapCountOverride,
        valuesInScopeFromParameters,
        assignedToProp,
      )

      const blockScope = {
        ...inScope,
        [JSX_CANVAS_LOOKUP_FUNCTION_NAME]: utopiaCanvasJSXLookup(
          element.elementsWithin,
          inScope,
          innerRender,
        ),
      }
      return runJSExpression(
        element,
        elementPath,
        blockScope,
        renderContext,
        uid,
        codeError,
        assignedToProp,
      )
    }
    case 'JSX_FRAGMENT': {
      const key = optionalMap(EP.toString, elementPath) ?? element.uid

      return renderJSXElement(
        key,
        element,
        elementPath,
        inScope,
        [],
        renderContext,
        codeError,
        assignedToProp,
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
        inScope,
        element.condition,
        elementPath,
        renderContext,
        uid,
        codeError,
        assignedToProp,
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

        addFakeSpyEntry(
          validPaths,
          metadataContext,
          elementPath,
          element,
          filePath,
          imports,
          {
            active: activeConditionValue,
            default: defaultConditionValue,
          },
          null,
          variablesInScope,
          assignedToProp,
        )
      }

      const childPath = optionalMap(
        (path) => EP.appendToPath(path, getUtopiaID(actualElement)),
        elementPath,
      )

      const elementIsTextEdited = elementPath != null && EP.pathsEqual(elementPath, editedText)

      if (elementIsTextEdited) {
        // when the inactive branch is an expression we can edit the full conditional as text
        const inactiveBranch = activeConditionValue ? element.whenFalse : element.whenTrue
        const fullTextEdit = isJSExpression(inactiveBranch)
        const textContent = fullTextEdit
          ? trimJoinUnescapeTextFromJSXElements([element])
          : trimJoinUnescapeTextFromJSXElements([actualElement])

        const textProp = (() => {
          if (fullTextEdit) {
            return 'fullConditional'
          }
          if (activeConditionValue) {
            return 'whenTrue'
          }
          return 'whenFalse'
        })()

        const textEditorProps: TextEditorProps = {
          elementPath: elementPath,
          filePath: filePath,
          text: textContent,
          originalText: null,
          component: React.Fragment,
          passthroughProps: {},
          textProp: textProp,
        }

        return buildSpyWrappedElement(
          actualElement,
          textEditorProps,
          childPath!,
          metadataContext,
          updateInvalidatedPaths,
          [],
          TextEditorWrapper,
          inScope,
          jsxFactoryFunctionName,
          shouldIncludeCanvasRootInTheSpy,
          imports,
          filePath,
          variablesInScope,
          'real-element',
          assignedToProp,
        )
      }

      return renderCoreElement(
        actualElement,
        childPath,
        inScope,
        renderContext,
        uid,
        codeError,
        assignedToProp,
      )
    }
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'JS_PROPERTY_ACCESS':
    case 'JS_ELEMENT_ACCESS':
    case 'JS_IDENTIFIER':
      if (elementPath != null) {
        addFakeSpyEntry(
          validPaths,
          metadataContext,
          elementPath,
          element,
          filePath,
          imports,
          'not-a-conditional',
          null,
          variablesInScope,
          assignedToProp,
        )
      }

      const elementIsTextEdited = elementPath != null && EP.pathsEqual(elementPath, editedText)
      if (elementIsTextEdited) {
        const textContent = trimJoinUnescapeTextFromJSXElements([element])
        const textEditorProps: TextEditorProps = {
          elementPath: elementPath,
          filePath: filePath,
          text: textContent,
          originalText: null,
          component: React.Fragment,
          passthroughProps: {},
          textProp: 'itself',
        }

        return buildSpyWrappedElement(
          element,
          textEditorProps,
          elementPath,
          metadataContext,
          updateInvalidatedPaths,
          [],
          TextEditorWrapper,
          inScope,
          jsxFactoryFunctionName,
          shouldIncludeCanvasRootInTheSpy,
          imports,
          filePath,
          variablesInScope,
          'real-element',
          assignedToProp,
        )
      }

      return jsxAttributeToValue(
        inScope,
        element,
        elementPath,
        renderContext,
        uid,
        codeError,
        assignedToProp,
      )
    default:
      const _exhaustiveCheck: never = element
      throw new Error(`Unhandled type ${JSON.stringify(element)}`)
  }
}

function trimJoinUnescapeTextFromJSXElements(elements: Array<JSXElementChild>): string {
  let combinedText = ''
  for (let i = 0; i < elements.length; i++) {
    combinedText += jsxElementChildToText(
      elements[i],
      elements[i - 1] ?? null,
      elements[i + 1] ?? null,
      'jsx',
      'outermost',
    )
  }

  return unescapeHTML(combinedText)
}

function renderJSXElement(
  key: string,
  jsx: JSXElementLike,
  elementPath: ElementPath | null,
  inScope: MapLike<any>,
  passthroughProps: MapLike<any>,
  renderContext: RenderContext,
  codeError: Error | null,
  assignedToProp: string | null,
): React.ReactElement {
  const {
    requireResult,
    hiddenInstances,
    displayNoneInstances,
    fileBlobs,
    validPaths,
    metadataContext,
    updateInvalidatedPaths,
    jsxFactoryFunctionName,
    shouldIncludeCanvasRootInTheSpy,
    filePath,
    imports,
    code,
    highlightBounds,
    editedText,
    variablesInScope,
  } = renderContext
  const createChildrenElement = (child: JSXElementChild): React.ReactChild => {
    const childPath = optionalMap((path) => EP.appendToPath(path, getUtopiaID(child)), elementPath)
    return renderCoreElement(child, childPath, inScope, renderContext, undefined, codeError, null)
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

  const isElementImportedFromModule = (moduleName: string, name: string) =>
    !elementIsIntrinsic &&
    importedFrom != null &&
    importedFrom.type === 'IMPORTED_ORIGIN' && // Imported and not from the same file.
    importedFrom.filePath === moduleName && // Originating from {moduleName}
    importedFrom.exportedName === name && // {name} component.
    elementFromImport === elementInScope // Ensures this is not a user defined component with the same name.

  const elementIsScene = isElementImportedFromModule('utopia-api', 'Scene')
  const elementIsRemixScene = isElementImportedFromModule('utopia-api', 'RemixScene')

  const element = (() => {
    if (elementIsScene) {
      return SceneComponent
    }
    if (elementIsRemixScene) {
      return RemixSceneComponent
    }
    return elementFromScopeOrImport
  })()

  const FinalElement = elementIsIntrinsic ? jsx.name.baseVariable : element
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

  if (
    elementPath != null &&
    validPaths.has(EP.toString(EP.makeLastPartOfPathStatic(elementPath)))
  ) {
    if (elementIsTextEdited) {
      const runJSExpressionLazy = () => {
        const innerRender = createLookupRender(elementPath, renderContext, null, [], assignedToProp)

        const blockScope: Record<any, any> = {
          ...inScope,
          [JSX_CANVAS_LOOKUP_FUNCTION_NAME]: utopiaCanvasJSXLookup({}, inScope, innerRender),
        }

        const expressionToEvaluate =
          childrenWithNewTextBlock.length > 0 && isJSExpression(childrenWithNewTextBlock[0])
            ? childrenWithNewTextBlock[0]
            : jsExpressionValue(null, emptyComments) // placeholder

        const result = runJSExpression(
          expressionToEvaluate,
          elementPath,
          blockScope,
          renderContext,
          jsx.uid,
          codeError,
          assignedToProp,
        )
        return result
      }

      const originalTextContent = STEGANOGRAPHY_ENABLED ? runJSExpressionLazy() : null

      const textContent = trimJoinUnescapeTextFromJSXElements(childrenWithNewTextBlock)
      const textEditorProps: TextEditorProps = {
        elementPath: elementPath,
        filePath: filePath,
        text: textContent,
        originalText: originalTextContent,
        component: FinalElement,
        passthroughProps: finalProps,
        textProp: 'child',
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
        variablesInScope,
        'text-editor',
        assignedToProp,
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
      variablesInScope,
      'real-element',
      assignedToProp,
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
  render: (element: JSXElement, inScope: MapLike<any>) => React.ReactChild | null,
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

function runJSExpression(
  block: JSExpression,
  elementPath: ElementPath | null,
  currentScope: MapLike<any>,
  renderContext: RenderContext,
  uid: string | undefined,
  codeError: Error | null, // this can be probably deleted, it is passed down through multiple layers but we just throw the error in the end
  assignedToProp: string | null,
): any {
  switch (block.type) {
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'JS_PROPERTY_ACCESS':
    case 'JS_ELEMENT_ACCESS':
    case 'JS_IDENTIFIER':
    case 'JSX_ELEMENT':
      return jsxAttributeToValue(
        currentScope,
        block,
        elementPath,
        renderContext,
        uid,
        codeError,
        assignedToProp,
      )

    case 'JSX_MAP_EXPRESSION':
      const valueToMap = runJSExpression(
        block.valueToMap,
        elementPath,
        currentScope,
        renderContext,
        uid,
        codeError,
        null,
      )
      const mapFunction = runJSExpression(
        block.mapFunction,
        elementPath,
        currentScope,
        renderContext,
        uid,
        codeError,
        null,
      )
      const result = valueToMap.map(mapFunction)
      return result
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return resolveParamsAndRunJsCode(
        renderContext.filePath,
        block,
        renderContext.requireResult,
        currentScope,
      )
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
