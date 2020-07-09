import { ObjectInterpolation } from '@emotion/core'
import styled from '@emotion/styled'
import { getChainSegmentEdge } from '../../utils/utils'
import { ControlStyles, betterReactMemo } from '../../uuiui-deps'
import { IcnProps } from '../icn'
import { UtopiaTheme } from '../styles/theme'
import * as React from 'react'

export type ChainedType = 'not-chained' | 'first' | 'last' | 'middle'

export type BoxCorners =
  | 'right'
  | 'left'
  | 'top'
  | 'bottom'
  | 'topLeft'
  | 'topRight'
  | 'bottomRight'
  | 'bottomLeft'
  | 'none'
  | 'all'

function getChainedBoxShadow(
  controlStyles: ControlStyles,
  chained: ChainedType,
  focused: boolean,
): ObjectInterpolation<any> {
  const controlStatusEdges = getChainSegmentEdge(controlStyles)
  const focusedBoxShadow = `0 0 0 1px ${UtopiaTheme.color.inspectorFocusedColor.value} inset`

  const standardBoxShadow = {
    boxShadow: focused ? focusedBoxShadow : `0 0 0 1px ${controlStyles.borderColor} inset`,
  }
  if (controlStyles.interactive) {
    switch (chained) {
      case 'not-chained': {
        return standardBoxShadow
      }
      case 'first': {
        return {
          boxShadow: focused
            ? focusedBoxShadow
            : `${controlStatusEdges.top}, ${controlStatusEdges.bottom}, ${controlStatusEdges.left}`,
        }
      }
      case 'middle': {
        return {
          boxShadow: focused
            ? focusedBoxShadow
            : `${controlStatusEdges.top}, ${controlStatusEdges.bottom}`,
        }
      }
      case 'last': {
        return {
          boxShadow: focused
            ? focusedBoxShadow
            : `${controlStatusEdges.top}, ${controlStatusEdges.bottom}, ${controlStatusEdges.right}`,
        }
      }
      default: {
        const _exhaustiveCheck: never = chained
        return standardBoxShadow
      }
    }
  } else {
    return standardBoxShadow
  }
}

export function getBorderRadiusStyles(chained: ChainedType, rc: BoxCorners) {
  return {
    borderRadius: chained || rc != null ? 0 : UtopiaTheme.inputBorderRadius,
    borderTopRightRadius:
      rc === 'all' || rc === 'right' || rc === 'topRight' || rc === 'top'
        ? UtopiaTheme.inputBorderRadius
        : undefined,
    borderBottomRightRadius:
      rc === 'all' || rc === 'right' || rc === 'bottomRight' || rc === 'bottom'
        ? UtopiaTheme.inputBorderRadius
        : undefined,
    borderTopLeftRadius:
      rc === 'all' || rc === 'left' || rc === 'topLeft' || rc === 'top'
        ? UtopiaTheme.inputBorderRadius
        : undefined,
    borderBottomLeftRadius:
      rc === 'all' || rc === 'left' || rc === 'bottomLeft' || rc === 'bottom'
        ? UtopiaTheme.inputBorderRadius
        : undefined,
  }
}

export interface BaseInputProps {
  focusOnMount?: boolean
  inputProps?: React.InputHTMLAttributes<HTMLInputElement>
}

interface InspectorInputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  chained?: ChainedType
  controlStyles: ControlStyles
  focused: boolean
  labelInner?: string | IcnProps
  roundCorners?: BoxCorners
  mixed?: boolean
  value?: string | string[] | number
}

const StyledInput = styled.input<InspectorInputProps>(
  ({ chained = 'not-chained', controlStyles, focused, labelInner, roundCorners = 'all' }) => ({
    outline: 'none',
    paddingTop: 2,
    paddingBottom: 2,
    paddingLeft: 6,
    paddingRight: labelInner != null ? 15 : 6,
    backgroundColor: controlStyles.backgroundColor,
    fontStyle: controlStyles.fontStyle,
    fontWeight: controlStyles.fontWeight,
    color: controlStyles.mainColor,
    border: 0,
    height: UtopiaTheme.layout.inputHeight.default,
    width: '100%',
    marginBottom: 0,
    ...getChainedBoxShadow(controlStyles, chained, focused),
    ...getBorderRadiusStyles(chained, roundCorners),
    disabled: !controlStyles.interactive,
  }),
)

export const InspectorInput = betterReactMemo(
  'InspectorInput',
  React.forwardRef<HTMLInputElement, InspectorInputProps>((props, ref) => {
    return <StyledInput {...props} ref={ref} data-inspector-input={true} />
  }),
)
