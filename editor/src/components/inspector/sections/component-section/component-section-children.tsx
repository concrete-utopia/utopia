import React from 'react'
import type {
  ControlDescription,
  RegularControlDescription,
} from '../../../custom-code/internal-property-controls'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import { iconForControlType } from '../../../../uuiui'
import type { ControlForPropProps } from './property-control-controls'
import { StringInputPropertyControl } from './property-control-controls'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type {
  ElementInstanceMetadataMap,
  JSXElementChild,
} from '../../../../core/shared/element-template'
import { isJSXElement, isJSXTextBlock } from '../../../../core/shared/element-template'
import { IdentifierExpressionCartoucheControl } from './cartouche-control'
import { renderedAtChildNode } from '../../../editor/store/editor-state'
import { isRight } from '../../../../core/shared/either'
import { DataReferenceCartoucheControl } from './data-reference-cartouche'
import { assertNever } from '../../../../core/shared/utils'
import { childrenAreProbablyNumericExpression } from '../../../editor/element-children'
import type { CartoucheDataType } from './cartouche-ui'

export function useChildrenPropOverride(
  props: ControlForPropProps<RegularControlDescription> & {
    onDeleteCartouche: () => void
    safeToDelete: boolean
    dataTypeForExpression: CartoucheDataType
  },
) {
  const childrenContent = useEditorState(
    Substores.metadata,
    (store) => {
      return getMaybeChildrenContent(
        store.editor.jsxMetadata,
        props.elementPath,
        props.propName,
        props.controlDescription,
      )
    },
    'useGetMaybeChildrenOverride maybeChildrenDataRefElement',
  )

  if (childrenContent == null) {
    return null
  }

  switch (childrenContent.type) {
    case 'cartouche':
      return (
        <DataReferenceCartoucheControl
          elementPath={props.elementPath}
          renderedAt={renderedAtChildNode(props.elementPath, childrenContent.value.uid)}
          surroundingScope={props.elementPath}
          childOrAttribute={childrenContent.value}
          selected={false}
        />
      )
    case 'expression':
      return (
        <IdentifierExpressionCartoucheControl
          contents={'Expression'}
          icon={React.createElement(iconForControlType('none'))}
          matchType='partial'
          onOpenDataPicker={props.onOpenDataPicker}
          onDeleteCartouche={props.onDeleteCartouche}
          testId={`cartouche-${PP.toString(props.propPath)}`}
          propertyPath={props.propPath}
          safeToDelete={props.safeToDelete}
          elementPath={props.elementPath}
          datatype={props.dataTypeForExpression}
        />
      )
    case 'text':
      return (
        <StringInputPropertyControl
          {...props}
          controlDescription={
            props.controlDescription.control === 'string-input'
              ? props.controlDescription
              : {
                  control: 'string-input',
                  label: 'hey',
                }
          }
        />
      )
    default:
      assertNever(childrenContent)
  }
}

function getMaybeChildrenContent(
  jsxMetadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
  propName: string,
  controlDescription: ControlDescription,
):
  | { type: 'cartouche'; value: JSXElementChild }
  | { type: 'text' }
  | { type: 'expression' }
  | null {
  if (propName !== 'children') {
    return null
  }

  const element = MetadataUtils.findElementByElementPath(jsxMetadata, elementPath)
  if (element != null && isRight(element.element) && isJSXElement(element.element.value)) {
    const { children } = element.element.value
    if (children.every((child) => isJSXTextBlock(child))) {
      return { type: 'text' }
    } else if (
      controlDescription.control === 'number-input' &&
      childrenAreProbablyNumericExpression(children)
    ) {
      return { type: 'expression' }
    } else if (
      children.length === 1 &&
      MetadataUtils.isElementDataReference(children[0]) &&
      children[0].type !== 'ATTRIBUTE_OTHER_JAVASCRIPT'
    ) {
      return { type: 'cartouche', value: children[0] }
    }
  }

  return null
}
