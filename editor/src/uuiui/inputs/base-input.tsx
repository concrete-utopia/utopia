import styled from '@emotion/styled'
import { ControlStyles } from '../../uuiui-deps'
import { UtopiaTheme } from '../styles/theme'
import { colorTheme } from 'uuiui'

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

interface BaseInputProps {
  chained?: ChainedType
  controlStyles: ControlStyles
  leftPadding: boolean
  roundCorners?: BoxCorners
  mixed?: boolean
  value?: string | string[] | number
}

export const BaseInput = styled.input<BaseInputProps>(
  ({ chained = 'not-chained', controlStyles, leftPadding, roundCorners = 'all' }) => ({
    outline: 'none',
    paddingTop: 2,
    paddingBottom: 2,
    paddingLeft: leftPadding ? 18 : 6,
    paddingRight: 6,
    fontStyle: controlStyles.fontStyle,
    fontWeight: controlStyles.fontWeight,
    color: controlStyles.mainColor,
    border: 0,
    height: UtopiaTheme.layout.inputHeight.default,
    width: '100%',
    marginBottom: 0,
    '::selection': {
      color: 'white',
      backgroundColor: UtopiaTheme.color.inspectorFocusedColor.value,
    },
    ...(controlStyles.isSet
      ? {
          boxShadow: `0 0 0 1px ${controlStyles.borderColor} inset`,
        }
      : {
          '.input-container:hover &': {
            boxShadow: `0 0 0 1px ${controlStyles.borderColor} inset`,
          },
        }),
    backgroundColor: controlStyles.backgroundColor,
    ...(controlStyles.isControlled
      ? {
          '.input-container:hover &': {
            backgroundColor: colorTheme.inspectorSetBackgroundColor.value,
            color: colorTheme.inspectorSetMainColor.value,
          },
        }
      : undefined),

    ':focus': {
      boxShadow: `0 0 0 1px ${UtopiaTheme.color.inspectorFocusedColor.value} inset`,
    },
    ...getBorderRadiusStyles(chained, roundCorners),
    disabled: !controlStyles.interactive,
  }),
)
