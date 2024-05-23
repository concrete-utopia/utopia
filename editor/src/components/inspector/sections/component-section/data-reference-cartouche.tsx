import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  JSExpressionOtherJavaScript,
  JSXElementChild,
} from '../../../../core/shared/element-template'
import { getJSXElementNameLastPart } from '../../../../core/shared/element-template'
import type { ElementPath, ElementPropertyPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import * as EPP from '../../../template-property-path'
import { NO_OP, assertNever } from '../../../../core/shared/utils'
import { FlexRow, Icn, Icons, Tooltip, UtopiaStyles, colorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { useDataPickerButton } from './component-section'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { selectComponents } from '../../../editor/actions/meta-actions'
import { when } from '../../../../utils/react-conditionals'
import { dataPathSuccess, traceDataFromElement } from '../../../../core/data-tracing/data-tracing'
import type { RenderedAt } from '../../../editor/store/editor-state'
import { replaceElementInScope } from '../../../editor/actions/action-creators'
import { useVariablesInScopeForSelectedElement } from './variables-in-scope-utils'
import { DataPickerPreferredAllAtom } from './data-picker-popup'
import { useAtom } from 'jotai'

interface DataReferenceCartoucheControlProps {
  elementPath: ElementPath
  childOrAttribute: JSXElementChild
  selected: boolean
  renderedAt: RenderedAt
  surroundingScope: ElementPath
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
        ? null
        : assertNever(props.renderedAt)

    const [preferredAllState] = useAtom(DataPickerPreferredAllAtom)

    const variableNamesInScope = useVariablesInScopeForSelectedElement(
      elementPath,
      propertyPath,
      preferredAllState,
    )

    const dataPickerButtonData = useDataPickerButton(variableNamesInScope, updateDataWithDataPicker)

    const isDataComingFromHookResult = dataTraceResult.type === 'hook-result'

    const onClick = React.useCallback(() => {
      dispatch(selectComponents([elementPath], false))
    }, [dispatch, elementPath])

    return (
      <>
        {dataPickerButtonData.popupIsOpen ? dataPickerButtonData.DataPickerComponent : null}
        <DataCartoucheInner
          ref={dataPickerButtonData.setReferenceElement}
          onClick={onClick}
          onDoubleClick={dataPickerButtonData.openPopup}
          contentsToDisplay={contentsToDisplay}
          selected={selected}
          safeToDelete={false}
          inverted={false}
          onDelete={NO_OP}
          testId={`data-reference-cartouche-${EP.toString(elementPath)}`}
          contentIsComingFromServer={isDataComingFromHookResult}
        />
      </>
    )
  },
)

interface DataCartoucheInnerProps {
  onClick: (e: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  onDoubleClick: (e: React.MouseEvent<HTMLDivElement, MouseEvent>) => void
  selected: boolean
  inverted: boolean
  contentsToDisplay: { type: 'literal' | 'reference'; label: string | null }
  safeToDelete: boolean
  onDelete: () => void
  testId: string
  contentIsComingFromServer: boolean
}

export const DataCartoucheInner = React.forwardRef(
  (props: DataCartoucheInnerProps, ref: React.Ref<HTMLDivElement>) => {
    const {
      onClick,
      onDoubleClick,
      safeToDelete,
      onDelete,
      selected,
      inverted,
      contentsToDisplay,
      contentIsComingFromServer,
    } = props

    const onDeleteInner = React.useCallback(
      (e: React.MouseEvent) => {
        e.stopPropagation()
        onDelete()
      },
      [onDelete],
    )

    const cartoucheIconColorToUse = contentIsComingFromServer ? 'green' : 'dynamic'

    const cartoucheIconColor = inverted
      ? 'on-highlight-main'
      : contentsToDisplay.type === 'reference'
      ? cartoucheIconColorToUse
      : 'secondary'

    const borderColor = inverted ? colorTheme.white.value : colorTheme.primary.value

    const primaryForegoundColorToUse = contentIsComingFromServer
      ? colorTheme.green.value
      : colorTheme.dynamicBlue.value

    const primaryBackgroundColorToUse = contentIsComingFromServer
      ? colorTheme.green10.value
      : colorTheme.selectionBlue10.value

    const foregroundColor = inverted
      ? colorTheme.white.value
      : contentsToDisplay.type === 'reference'
      ? primaryForegoundColorToUse
      : colorTheme.neutralForeground.value

    const backgroundColor =
      contentsToDisplay.type === 'reference'
        ? primaryBackgroundColorToUse
        : colorTheme.fg0Opacity10.value

    const label = contentsToDisplay.label ?? 'DATA'

    return (
      <div
        onClick={onClick}
        onDoubleClick={onDoubleClick}
        style={{
          minWidth: 0, // this ensures that the div can never expand the allocated grid space
        }}
        ref={ref}
      >
        <FlexRow
          style={{
            cursor: 'pointer',
            fontSize: 10,
            fontWeight: 400,
            color: foregroundColor,
            backgroundColor: backgroundColor,
            border: selected ? '1px solid ' + borderColor : '1px solid transparent',
            padding: '0px 6px 0 4px',
            borderRadius: 4,
            height: 20,
            display: 'flex',
            flex: 1,
            gap: 4,
          }}
        >
          {contentsToDisplay.type === 'reference' ? (
            <Icons.NavigatorData color={cartoucheIconColor} />
          ) : null}
          <Tooltip title={label}>
            <div
              style={{
                flex: 1,
                paddingTop: 1,
                /* Standard CSS ellipsis */
                whiteSpace: 'nowrap',
                overflow: 'hidden',
                textOverflow: 'ellipsis',

                /* Beginning of string */
                direction: contentsToDisplay.type === 'reference' ? 'rtl' : 'ltr', // TODO we need a better way to ellipsize the beginnign because rtl eats ' " marks
                textAlign: 'left',
                ...UtopiaStyles.fontStyles.monospaced,
              }}
            >
              {label}
              &lrm;
              {/* the &lrm; non-printing character is added to fix the punctuation marks disappearing because of direction: rtl */}
            </div>
          </Tooltip>
          {when(
            safeToDelete,
            <Icn
              category='semantic'
              type='cross'
              color={cartoucheIconColor}
              width={12}
              height={12}
              data-testid={`delete-${props.testId}`}
              onClick={onDeleteInner}
            />,
          )}
        </FlexRow>
      </div>
    )
  },
)

export function getTextContentOfElement(
  element: JSXElementChild,
  metadata: ElementInstanceMetadata | null,
): { type: 'literal' | 'reference'; label: string | null } {
  switch (element.type) {
    case 'ATTRIBUTE_VALUE':
      return { type: 'literal', label: `${JSON.stringify(element.value)}` }
    case 'JSX_TEXT_BLOCK':
      return { type: 'literal', label: `'${element.text}'` }
    case 'JS_IDENTIFIER':
      return { type: 'reference', label: `${element.name}` }
    case 'JS_ELEMENT_ACCESS':
      return {
        type: 'reference',
        label: `${getTextContentOfElement(element.onValue, null).label}[${
          getTextContentOfElement(element.element, null).label
        }]`,
      }
    case 'JS_PROPERTY_ACCESS':
      return {
        type: 'reference',
        label: `${getTextContentOfElement(element.onValue, null).label}.${element.property}`,
      }
    case 'ATTRIBUTE_FUNCTION_CALL':
      return { type: 'reference', label: `${element.functionName}(...` }
    case 'JSX_ELEMENT':
      return {
        type: 'literal',
        label: metadata?.textContent ?? `${getJSXElementNameLastPart(element.name)}`,
      }
    case 'ATTRIBUTE_NESTED_ARRAY':
      return { type: 'literal', label: '[...]' }
    case 'ATTRIBUTE_NESTED_OBJECT':
      return { type: 'literal', label: '{...}' }
    case 'JSX_MAP_EXPRESSION':
      return { type: 'literal', label: 'List' }
    case 'JSX_CONDITIONAL_EXPRESSION':
      return { type: 'literal', label: 'Conditional' }
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      return { type: 'literal', label: element.originalJavascript }
    case 'JSX_FRAGMENT':
      return { type: 'literal', label: 'Fragment' }
    default:
      assertNever(element)
  }
}
