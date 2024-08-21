import styled from '@emotion/styled'
import type { Property } from 'csstype'
//TODO: refactor components to functional components and use 'useColorTheme()':
import { colorTheme, UtopiaTheme } from './styles/theme'

export interface ButtonProps {
  hidden?: boolean
  highlight?: boolean
  spotlight?: boolean
  outline?: boolean
  disabled?: boolean
  primary?: boolean
  danger?: boolean
  overriddenBackground?: Property.Background<string | number>
}

/**
 * Button
 * Inline button for UI elements, including small icon buttons on rows
 * @param hidden: invisible
 * @param highlight: should highlight on hover
 * @param spotlight: slightly visible background
 * @param outline: draws borders
 * @param disabled: no interactions, style opacity. Also prevents pointer events
 * @param primary: uses primary color scheme

 */

export const Button = styled.div<ButtonProps>((props: ButtonProps) => {
  let background: Property.Background<string | number> | undefined = undefined
  if (props.overriddenBackground != null) {
    background = props.overriddenBackground
  } else if (props.primary) {
    background = colorTheme.primary.value
  } else if (props.spotlight) {
    background = colorTheme.buttonBackground.value
  }

  let hoverBackground: Property.Background<string | number> | undefined = 'transparent'
  if (props.overriddenBackground != null) {
    hoverBackground = props.overriddenBackground
  } else if (props.primary && props.highlight) {
    hoverBackground = colorTheme.primary.value
  } else if (props.highlight) {
    hoverBackground = colorTheme.buttonHoverBackground.value
  }

  return {
    label: 'button',
    display: props.hidden ? 'none' : 'flex',
    flexGrow: 0,
    flexShrink: 0,
    border: 'none',
    boxSixing: 'border-box',
    flexDirection: 'row',
    alignItems: 'center',
    justifyContent: 'center',
    outline: 'none',
    borderRadius: UtopiaTheme.inputBorderRadius,
    padding: 0,
    height: UtopiaTheme.layout.inputHeight.default,
    opacity: props.disabled ? 0.5 : 1,
    pointerEvents: props.disabled ? 'none' : 'initial',
    boxShadow: props.outline ? `inset 0px 0px 0px 1px ${colorTheme.buttonShadow.value}` : undefined,
    color: props.primary ? 'white' : 'inherit',
    background: background,
    '&:hover': {
      background: hoverBackground,
    },
  }
})

export const SquareButton = styled(Button)({
  label: 'SquareButton',
  width: UtopiaTheme.layout.inputHeight.default,
  height: UtopiaTheme.layout.inputHeight.default,
})

export const ToggleButton = styled(SquareButton)<{
  value: boolean
}>((props) => ({
  background: props.value ? colorTheme.buttonBackground.value : 'transparent',
  '&:hover': {
    background: colorTheme.buttonHoverBackground.value,
  },
}))

export const FormButton = styled.button<ButtonProps>((props: ButtonProps) => ({
  justifyContent: 'center',
  fontSize: '11px',
  paddingLeft: 12,
  paddingRight: 12,
  minWidth: '90px',
  height: UtopiaTheme.layout.rowHeight.normal,
  display: props.hidden ? 'none' : 'flex',
  boxSixing: 'border-box',
  flexDirection: 'row',
  alignItems: 'center',
  borderRadius: 5,
  outline: 'none',
  opacity: props.disabled ? 0.5 : 1,
  pointerEvents: props.disabled ? 'none' : 'initial',
  cursor: 'pointer',

  // slightly subdued colors in default state
  backgroundColor: props.primary
    ? props.danger
      ? colorTheme.errorForeground.value
      : colorTheme.primary.value
    : 'transparent',

  color: props.primary ? 'white' : props.danger ? colorTheme.errorForeground.value : 'inherit',

  border: `1px solid ${
    props.danger
      ? colorTheme.errorForeground.value
      : props.primary
      ? colorTheme.denimBlue.value
      : 'transparent'
  }`,
  transition: 'all .10s ease-in-out',
  // regular background in hover state
  '&:hover': {
    backgroundColor: props.primary
      ? props.danger
        ? colorTheme.errorForeground.value
        : colorTheme.denimBlue.value
      : colorTheme.dialogBackground2.value,
  },
  '&:focus': {
    outline: 'none',
    //  solid subdued outline in focused state
    boxShadow: `0px 0px 0px 2px ${
      props.danger
        ? colorTheme.errorForeground20.value
        : props.primary
        ? colorTheme.primary30.value
        : colorTheme.subduedBorder80.value
    }`,
  },
  '&:active': {
    outline: 'none',
    // slightly brighter backgrounds while pressed
    backgroundColor: props.primary
      ? props.danger
        ? colorTheme.errorForegroundEmphasized.value
        : colorTheme.primary.value
      : colorTheme.bg2.value,
  },
}))
