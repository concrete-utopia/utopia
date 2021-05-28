import styled from '@emotion/styled'
import { getChainSegmentEdge } from '../../utils/utils'
import { ControlStyles, betterReactMemo, ControlStatus } from '../../uuiui-deps'
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
  hovered: boolean,
) {
  const controlStatusEdges = getChainSegmentEdge(controlStyles)
  const hoveredBoxShadow = `0 0 0 1px hsl(0,0%,83%) inset`
  const focusedBoxShadow = `0 0 0 1px ${UtopiaTheme.color.inspectorFocusedColor.value} inset`

  const standardBoxShadow = {
    boxShadow: focused
      ? focusedBoxShadow
      : hovered
      ? hoveredBoxShadow
      : `0 0 0 1px ${controlStyles.borderColor} inset`,
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
            : hovered
            ? hoveredBoxShadow
            : `${controlStatusEdges.top}, ${controlStatusEdges.bottom}, ${controlStatusEdges.left}`,
        }
      }
      case 'middle': {
        return {
          boxShadow: focused
            ? focusedBoxShadow
            : hovered
            ? hoveredBoxShadow
            : `${controlStatusEdges.top}, ${controlStatusEdges.bottom}`,
        }
      }
      case 'last': {
        return {
          boxShadow: focused
            ? focusedBoxShadow
            : hovered
            ? hoveredBoxShadow
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
  testId: string
}

interface InspectorInputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  testId: string
  chained?: ChainedType
  controlStyles: ControlStyles
  controlStatus: ControlStatus
  focused: boolean
  labelInner?: string | IcnProps
  roundCorners?: BoxCorners
  mixed?: boolean
  value?: string | readonly string[] | number
}

const StyledInput = styled.input<InspectorInputProps>(
  ({ chained = 'not-chained', controlStyles, focused, labelInner, roundCorners = 'all' }) => ({
    outline: 'none',
    paddingTop: 2,
    paddingBottom: 2,
    paddingRight: 6,
    paddingLeft: labelInner != null ? 22 : 6,
    background: 'transparent',
    fontStyle: controlStyles.fontStyle,
    fontWeight: controlStyles.fontWeight,
    border: 0,
    height: UtopiaTheme.layout.inputHeight.default,
    width: '100%',
    marginBottom: 0,
    // ...getChainedBoxShadow(controlStyles, chained, focused, false),
    ...getBorderRadiusStyles(chained, roundCorners),
    disabled: !controlStyles.interactive,
    '&:hover': {
      // ...getChainedBoxShadow(controlStyles, chained, focused, true),
    },
    '&:focus': {},
  }),
)

export const InspectorInput = betterReactMemo(
  'InspectorInput',
  React.forwardRef<HTMLInputElement, InspectorInputProps>((props, ref) => {
    return (
      <StyledInput
        {...props}
        ref={ref}
        data-inspector-input={true}
        data-testid={props.testId}
        data-controlstatus={props.controlStatus}
      />
    )
  }),
)
