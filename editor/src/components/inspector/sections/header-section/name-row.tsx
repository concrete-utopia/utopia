import * as React from 'react'
import { getControlStyles } from '../../common/control-status'
import { SelectOption } from '../../controls/select-control'
import { JSXElementName, jsxElementName } from '../../../../core/shared/element-template'
import { GridRow } from '../../widgets/grid-row'
import { PopupList } from '../../../../uuiui'
import { betterReactMemo } from '../../../../uuiui-deps'

export interface NameRowProps {
  onElementTypeChange: (value: JSXElementName) => void
}

export interface NameRowInnerProps extends NameRowProps {
  label: string
  type: null | string
}

export const NameRow = betterReactMemo('NameRow', (props: NameRowInnerProps) => {
  const onSelect = React.useCallback(
    (selectOption: SelectOption) => {
      const value = selectOption.value
      if (typeof value === 'string') {
        const elementName = jsxElementName(value, [])
        props.onElementTypeChange(elementName)
      } else {
        props.onElementTypeChange(value)
      }
    },
    [props],
  )

  return (
    <GridRow padded={true} type='<---1fr--->|------172px-------|'>
      <span
        style={{
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
        }}
      >
        Render as
      </span>
      {props.type == null ? null : (
        <PopupList
          disabled={!controlStyles.interactive}
          value={{ value: props.type, label: props.type }}
          onSubmitValue={onSelect}
          options={typeOptions}
          containerMode='default'
        />
      )}
    </GridRow>
  )
})

const constrolStatus = 'simple'
const controlStyles = getControlStyles(constrolStatus)

export const typeOptions: ReadonlyArray<SelectOption> = [
  {
    value: 'View',
    label: 'View',
    icon: {
      category: 'element',
      type: 'view',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'Rectangle',
    label: 'Rectangle',
    icon: {
      category: 'element',
      type: 'rectangle',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'Ellipse',
    label: 'Ellipse',
    icon: {
      category: 'element',
      type: 'ellipse',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'div',
    label: 'div',
    icon: {
      category: 'element',
      type: 'div',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'span',
    label: 'span',
    icon: {
      category: 'element',
      type: 'div',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: jsxElementName('animated', ['div']),
    label: 'animated.div',
    icon: {
      category: 'element',
      type: 'animated',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'img',
    label: 'Image',
    icon: {
      category: 'element',
      type: 'image',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
  {
    value: 'Text',
    label: 'Text',
    icon: {
      category: 'element',
      type: 'text',
      width: 18,
      height: 18,
      color: 'black',
    },
  },
]
