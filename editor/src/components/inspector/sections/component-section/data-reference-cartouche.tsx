import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  JSXElementChild,
} from '../../../../core/shared/element-template'
import { getJSXElementNameLastPart } from '../../../../core/shared/element-template'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import { assertNever } from '../../../../core/shared/utils'
import { FlexRow, Icn, Icons, Tooltip, UtopiaStyles, colorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { IdentifierExpressionCartoucheControl } from './cartouche-control'
import { useDataPickerButton } from './component-section'

interface DataReferenceCartoucheControlProps {
  onClick: (event: React.MouseEvent<HTMLDivElement>) => void
  elementPath: ElementPath
  childOrAttribute: JSXElementChild
  selected: boolean
}

export const DataReferenceCartoucheControl = React.memo(
  (props: DataReferenceCartoucheControlProps) => {
    const { elementPath, childOrAttribute, selected, onClick } = props

    const contentsToDisplay = useEditorState(
      Substores.metadata,
      (store) =>
        getTextContentOfElement(
          childOrAttribute,
          MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, elementPath),
        ),
      'DataReferenceCartoucheControl contentsToDisplay',
    )

    const dataPickerButtonData = useDataPickerButton(
      [EP.parentPath(elementPath)],
      PP.fromString('children'), // TODO
      false, // TODO
      {
        control: 'none',
      },
    )

    const onDeleteCartouche = React.useCallback(() => {}, [])

    const cartoucheIconColor = contentsToDisplay.type === 'reference' ? 'primary' : 'secondary'
    const editButtonIconColor = contentsToDisplay.type === 'reference' ? 'primary' : 'secondary'
    const foregroundColor =
      contentsToDisplay.type === 'reference'
        ? colorTheme.primary.value
        : colorTheme.neutralForeground.value
    const backgroundColor =
      contentsToDisplay.type === 'reference' ? colorTheme.primary10.value : colorTheme.bg4.value

    const label = contentsToDisplay.label ?? 'DATA'

    return (
      <>
        {dataPickerButtonData.popupIsOpen ? dataPickerButtonData.DataPickerComponent : null}
        <div
          onClick={onClick}
          style={{
            minWidth: 0, // this ensures that the div can never expand the allocated grid space
          }}
          ref={dataPickerButtonData.setReferenceElement}
        >
          <FlexRow
            style={{
              cursor: 'pointer',
              fontSize: 10,
              color: foregroundColor,
              backgroundColor: backgroundColor,
              border: selected ? '1px solid ' + colorTheme.primary.value : '1px solid transparent',
              padding: '0px 4px',
              borderRadius: 4,
              height: 22,
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
            <Icn
              category='semantic'
              type='pipette'
              color={editButtonIconColor}
              width={18}
              height={18}
              onClick={dataPickerButtonData.openPopup}
              style={{ zoom: 0.6 }}
            />
          </FlexRow>
        </div>
      </>
    )
  },
)

function getTextContentOfElement(
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
        label: metadata?.textContent ?? `<${getJSXElementNameLastPart(element.name)}>`,
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
