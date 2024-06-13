import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  JSExpressionOtherJavaScript,
  JSXElementChild,
} from '../../../../core/shared/element-template'
import { getJSXElementNameLastPart } from '../../../../core/shared/element-template'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { NO_OP, assertNever } from '../../../../core/shared/utils'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { useDataPickerButton } from './component-section'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { selectComponents } from '../../../editor/actions/meta-actions'
import { dataPathSuccess, traceDataFromElement } from '../../../../core/data-tracing/data-tracing'
import type { RenderedAt } from '../../../editor/store/editor-state'
import { replaceElementInScope } from '../../../editor/actions/action-creators'
import {
  getCartoucheDataTypeForExpression,
  useVariablesInScopeForSelectedElement,
} from './variables-in-scope-utils'
import { DataPickerPreferredAllAtom, jsxElementChildToValuePath } from './data-picker-utils'
import { useAtom } from 'jotai'
import type { CartoucheDataType, CartoucheHighlight, CartoucheUIProps } from './cartouche-ui'
import { CartoucheUI } from './cartouche-ui'
import * as PP from '../../../../core/shared/property-path'

interface DataReferenceCartoucheControlProps {
  elementPath: ElementPath
  childOrAttribute: JSXElementChild
  selected: boolean
  renderedAt: RenderedAt
  surroundingScope: ElementPath
  highlight?: CartoucheHighlight | null
}

export const DataReferenceCartoucheControl = React.memo(
  (props: DataReferenceCartoucheControlProps) => {
    const { elementPath, childOrAttribute, selected } = props

    const dispatch = useDispatch()

    const contentsToDisplay = useEditorState(
      Substores.metadata,
      (store) =>
        getTextContentOfElement(
          childOrAttribute,
          MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, elementPath),
        ),
      'DataReferenceCartoucheControl contentsToDisplay',
    )

    const dataTraceResult = useEditorState(
      Substores.projectContentsAndMetadata,
      (store) => {
        return traceDataFromElement(
          props.childOrAttribute,
          props.surroundingScope,
          store.editor.jsxMetadata,
          store.editor.projectContents,
          dataPathSuccess([]),
        )
      },
      'IdentifierExpressionCartoucheControl trace',
    )

    const updateDataWithDataPicker = React.useCallback(
      (expression: JSExpressionOtherJavaScript) => {
        switch (props.renderedAt.type) {
          case 'element-property-path':
            return dispatch([
              replaceElementInScope(props.renderedAt.elementPropertyPath.elementPath, {
                type: 'replace-property-value',
                propertyPath: props.renderedAt.elementPropertyPath.propertyPath,
                replaceWith: expression,
              }),
            ])
          case 'child-node':
            return dispatch([
              replaceElementInScope(props.renderedAt.parentPath, {
                type: 'replace-child-with-uid',
                uid: props.renderedAt.nodeUid,
                replaceWith: expression,
              }),
            ])
          default:
            assertNever(props.renderedAt)
        }
      },
      [dispatch, props.renderedAt],
    )

    const propertyPath =
      props.renderedAt.type === 'element-property-path'
        ? props.renderedAt.elementPropertyPath.propertyPath
        : props.renderedAt.type === 'child-node'
        ? PP.create('children')
        : assertNever(props.renderedAt)

    const [preferredAllState] = useAtom(DataPickerPreferredAllAtom)

    const variableNamesInScope = useVariablesInScopeForSelectedElement(
      elementPath,
      propertyPath,
      preferredAllState,
    )

    const pathToCurrenlySelectedValue = React.useMemo(
      () => jsxElementChildToValuePath(childOrAttribute),
      [childOrAttribute],
    )

    const currentlySelectedValueDataType = useEditorState(
      Substores.projectContentsAndMetadataAndVariablesInScope,
      (store) => {
        return getCartoucheDataTypeForExpression(
          elementPath,
          childOrAttribute,
          store.editor.variablesInScope,
        )
      },
      'DataReferenceCartoucheControl currentlySelectedValueDataType',
    )

    const dataPickerButtonData = useDataPickerButton(
      variableNamesInScope,
      updateDataWithDataPicker,
      pathToCurrenlySelectedValue,
      elementPath,
    )

    const isDataComingFromHookResult = dataTraceResult.type === 'hook-result'

    const onClick = React.useCallback(() => {
      dispatch(selectComponents([elementPath], false))
    }, [dispatch, elementPath])

    return (
      <>
        <DataCartoucheInner
          ref={dataPickerButtonData.setReferenceElement}
          onClick={onClick}
          onDoubleClick={dataPickerButtonData.openPopup}
          contentsToDisplay={contentsToDisplay}
          selected={selected}
          safeToDelete={false}
          highlight={props.highlight}
          onDelete={NO_OP}
          testId={`data-reference-cartouche-${EP.toString(elementPath)}`}
          contentIsComingFromServer={isDataComingFromHookResult}
          datatype={currentlySelectedValueDataType}
        />
        {/* this div prevents the popup form putting padding into the condensed rows */}
        <div style={{ width: 0 }}>
          {dataPickerButtonData.popupIsOpen ? dataPickerButtonData.DataPickerComponent : null}
        </div>
      </>
    )
  },
)

interface DataCartoucheInnerProps {
  onClick: (e: React.MouseEvent) => void
  onDoubleClick: (e: React.MouseEvent) => void
  selected: boolean
  contentsToDisplay: {
    type: 'literal' | 'reference'
    label: string | null
    shortLabel: string | null
  }
  safeToDelete: boolean
  onDelete: () => void
  testId: string
  contentIsComingFromServer: boolean
  datatype: CartoucheDataType
  highlight?: CartoucheHighlight | null
  badge?: React.ReactNode
}

export const DataCartoucheInner = React.forwardRef(
  (props: DataCartoucheInnerProps, ref: React.Ref<HTMLDivElement>) => {
    const {
      onClick,
      onDoubleClick,
      safeToDelete,
      onDelete: onDeleteCallback,
      selected,
      highlight,
      testId,
      contentsToDisplay,
      contentIsComingFromServer,
      datatype,
    } = props

    const onDeleteInner = React.useCallback(
      (e: React.MouseEvent) => {
        e.stopPropagation()
        onDeleteCallback()
      },
      [onDeleteCallback],
    )

    const onDelete = safeToDelete ? onDeleteInner : undefined

    const source: CartoucheUIProps['source'] =
      contentsToDisplay.type === 'literal'
        ? 'literal'
        : contentIsComingFromServer
        ? 'external'
        : 'internal'

    return (
      <CartoucheUI
        onDelete={onDelete}
        onClick={onClick}
        onDoubleClick={onDoubleClick}
        datatype={datatype}
        selected={selected}
        highlight={highlight}
        testId={testId}
        tooltip={contentsToDisplay.label ?? contentsToDisplay.shortLabel ?? 'DATA'}
        role='selection'
        source={source}
        ref={ref}
        badge={props.badge}
      >
        {contentsToDisplay.shortLabel ?? contentsToDisplay.label ?? 'DATA'}
      </CartoucheUI>
    )
  },
)

export function getTextContentOfElement(
  element: JSXElementChild,
  metadata: ElementInstanceMetadata | null,
): { type: 'literal' | 'reference'; label: string | null; shortLabel: string | null } {
  switch (element.type) {
    case 'ATTRIBUTE_VALUE':
      return { type: 'literal', label: `${JSON.stringify(element.value)}`, shortLabel: null }
    case 'JSX_TEXT_BLOCK':
      return { type: 'literal', label: element.text.trim(), shortLabel: null }
    case 'JS_IDENTIFIER':
      return { type: 'reference', label: element.name.trim(), shortLabel: null }
    case 'JS_ELEMENT_ACCESS':
      return {
        type: 'reference',
        label: `${getTextContentOfElement(element.onValue, null).label}[${
          getTextContentOfElement(element.element, null).label
        }]`,
        shortLabel: `${TruncationPrefix}${getTextContentOfElement(element.element, null).label}`,
      }
    case 'JS_PROPERTY_ACCESS':
      return {
        type: 'reference',
        label: `${getTextContentOfElement(element.onValue, null).label}.${element.property}`,
        shortLabel: `${TruncationPrefix}${element.property}`,
      }
    case 'ATTRIBUTE_FUNCTION_CALL':
      return { type: 'reference', label: `${element.functionName}(...`, shortLabel: null }
    case 'JSX_ELEMENT':
      return {
        type: 'literal',
        label: metadata?.textContent ?? `${getJSXElementNameLastPart(element.name)}`,
        shortLabel: null,
      }
    case 'ATTRIBUTE_NESTED_ARRAY':
      return { type: 'literal', label: '[...]', shortLabel: null }
    case 'ATTRIBUTE_NESTED_OBJECT':
      return { type: 'literal', label: '{...}', shortLabel: null }
    case 'JSX_MAP_EXPRESSION':
      return { type: 'literal', label: 'List', shortLabel: null }
    case 'JSX_CONDITIONAL_EXPRESSION':
      return { type: 'literal', label: 'Conditional', shortLabel: null }
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return { type: 'literal', label: element.originalJavascript, shortLabel: null }
    case 'JSX_FRAGMENT':
      return { type: 'literal', label: 'Fragment', shortLabel: null }
    default:
      assertNever(element)
  }
}

const TruncationPrefix = `…`
