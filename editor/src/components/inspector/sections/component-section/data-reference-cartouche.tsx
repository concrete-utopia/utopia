import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  JSXElementChild,
} from '../../../../core/shared/element-template'
import { getJSXElementNameLastPart } from '../../../../core/shared/element-template'
import type { ElementPath, PropertyPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import { NO_OP, assertNever } from '../../../../core/shared/utils'
import { FlexRow, Icn, Icons, Tooltip, UtopiaStyles, colorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { useDataPickerButton } from './component-section'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { selectComponents } from '../../../editor/actions/meta-actions'
import { when } from '../../../../utils/react-conditionals'
import { traceDataFromProp } from '../../../../core/data-tracing/data-tracing'
import * as EPP from '../../../template-property-path'

interface DataReferenceCartoucheControlProps {
  elementPath: ElementPath
  renderedByElementPath: ElementPath
  childOrAttribute: JSXElementChild
  selected: boolean
  propertyPath: PropertyPath
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

    // TODO get the actual data _value_ to display in the cartouche, and only show the data reference in the tooltip

    const dataPickerButtonData = useDataPickerButton(
      [EP.parentPath(elementPath)],
      props.propertyPath,
      false, // TODO
      {
        control: 'none',
      },
    )

    const isDataComingFromHookResult = useEditorState(
      Substores.projectContentsAndMetadata,
      (store) => {
        return traceDataFromProp(
          EPP.create(props.renderedByElementPath, props.propertyPath),
          store.editor.jsxMetadata,
          store.editor.projectContents,
          [],
        )
      },
      'IdentifierExpressionCartoucheControl trace',
    )

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
          contentIsComingFromServer={isDataComingFromHookResult.type === 'hook-result'}
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

    const cartoucheIconColorToUse = contentIsComingFromServer ? 'component' : 'primary'

    const cartoucheIconColor = inverted
      ? 'on-highlight-main'
      : contentsToDisplay.type === 'reference'
      ? cartoucheIconColorToUse
      : 'secondary'

    const borderColor = inverted
      ? colorTheme.neutralInvertedForeground.value
      : colorTheme.primary.value

    const primaryForegoundColorToUse = contentIsComingFromServer
      ? colorTheme.component.value
      : colorTheme.primary10.value

    const foregroundColor = inverted
      ? colorTheme.neutralInvertedForeground.value
      : contentsToDisplay.type === 'reference'
      ? primaryForegoundColorToUse
      : colorTheme.neutralForeground.value

    const primaryBackgroundColorToUse = contentIsComingFromServer
      ? colorTheme.componentPurple05solid.value
      : colorTheme.primary10.value

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
            color: foregroundColor,
            backgroundColor: backgroundColor,
            border: selected ? '1px solid ' + borderColor : '1px solid transparent',
            padding: '0px 4px',
            borderRadius: 4,
            height: 20,
            display: 'flex',
            flex: 1,
            gap: 4,
          }}
        >
          {contentsToDisplay.type === 'reference' ? (
            <Icons.NavigatorData color={cartoucheIconColor} />
          ) : (
            <Icons.NavigatorText color={cartoucheIconColor} />
          )}
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
              type='cross-medium'
              color={cartoucheIconColor}
              width={16}
              height={16}
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
