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
import { Icons } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { IdentifierExpressionCartoucheControl } from './cartouche-control'
import { useDataPickerButton } from './component-section'

interface DataReferenceCartoucheControlProps {
  elementPath: ElementPath
  childOrAttribute: JSXElementChild
}

export const DataReferenceCartoucheControl = React.memo(
  (props: DataReferenceCartoucheControlProps) => {
    const { elementPath, childOrAttribute } = props

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

    return (
      <>
        {dataPickerButtonData.popupIsOpen ? dataPickerButtonData.DataPickerComponent : null}
        <div
          style={{
            minWidth: 0, // this ensures that the div can never expand the allocated grid space
          }}
          ref={dataPickerButtonData.setReferenceElement}
        >
          <IdentifierExpressionCartoucheControl
            contents={contentsToDisplay.label ?? 'DATA'}
            icon={contentsToDisplay.type === 'reference' ? '🌸' : <Icons.StringInputControl />}
            matchType={contentsToDisplay.type === 'reference' ? 'full' : 'none'}
            onOpenDataPicker={dataPickerButtonData.openPopup}
            onDeleteCartouche={onDeleteCartouche}
            safeToDelete={false}
            testId={'data-reference-cartouche'}
          />
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
