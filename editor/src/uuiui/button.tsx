import styled from '@emotion/styled'
import { colorTheme, UtopiaTheme, UtopiaStyles } from './styles/theme'

export interface ButtonProps {
  hidden?: boolean
  highlight?: boolean
  spotlight?: boolean
  outline?: boolean
  disabled?: boolean
  primary?: boolean
  danger?: boolean
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

export const Button = styled.div<ButtonProps>((props: ButtonProps) => ({
  label: 'button',
  cursor: 'pointer',
  minHeight: UtopiaTheme.layout.inputHeight.default,
  display: props.hidden ? 'none' : 'flex',
  flexGrow: 0,
  flexShrink: 0,
  border: 'none',
  boxSixing: 'border-box',
  flexDirection: 'row',
  alignItems: 'center',
  justifyContent: 'center',
  outline: 'none',
  borderRadius: 1,
  padding: 0,
  opacity: props.disabled ? 0.5 : 1,
  pointerEvents: props.disabled ? 'none' : 'initial',
  boxShadow: props.outline ? 'inset 0px 0px 0px 1px hsl(0,0%,90%)' : undefined,
  color: props.primary ? 'white' : 'inherit',
  //TODO Nested ternaries
  background: props.primary
    ? UtopiaStyles.backgrounds.blue
    : props.spotlight
    ? UtopiaTheme.color.secondaryBackground.value
    : undefined,
  '&:hover': {
    background:
      props.primary && props.highlight
        ? UtopiaStyles.backgrounds.lightblue
        : props.highlight
        ? 'hsl(0,0%,95%)'
        : 'transparent',
    filter: props.highlight ? 'brightness(95%)' : undefined,
  },
  '&:active': {
    outline: 'none',
    border: 'none',
    boxShadow: props.outline ? 'inset 0px 0px 0px 1px  hsl(0,0%,80%)' : undefined,
    filter: props.highlight ? 'brightness(95%)' : undefined,
  },
}))

export const SquareButton = styled(Button)({
  label: 'SquareButton',
  width: UtopiaTheme.layout.inputHeight.default,
  height: UtopiaTheme.layout.inputHeight.default,
})

export const ToggleButton = styled(SquareButton)<{
  value: boolean
}>((props) => ({
  background: props.value ? UtopiaTheme.color.secondaryBackground.value : 'transparent',
  '&:hover': {
    background: UtopiaTheme.color.neutralBackground.value,
  },
}))

export const FormButton = styled.button<ButtonProps>((props: ButtonProps) => ({
  justifyContent: 'center',
  fontSize: '11px',
  paddingLeft: 12,
  paddingRight: 12,
  minWidth: '90px',
  height: UtopiaTheme.layout.rowHeight.medium,
  display: props.hidden ? 'none' : 'flex',
  boxSixing: 'border-box',
  flexDirection: 'row',
  alignItems: 'center',
  borderRadius: 1,
  outline: 'none',
  opacity: props.disabled ? 0.5 : 1,
  pointerEvents: props.disabled ? 'none' : 'initial',

  // slightly subdued colors in default state
  backgroundColor: props.primary
    ? props.danger
      ? colorTheme.errorForeground.shade(90).value
      : colorTheme.primary.shade(90).value
    : colorTheme.emphasizedBackground.shade(101).value,

  color: props.primary ? 'white' : props.danger ? colorTheme.errorForeground.value : 'inherit',

  border: `1px solid ${
    props.danger
      ? colorTheme.errorForeground.value
      : props.primary
      ? colorTheme.primary.value
      : colorTheme.neutralBorder.value
  }`,
  transition: 'all .10s ease-in-out',
  // regular background in hover state
  '&:hover': {
    backgroundColor: props.primary
      ? props.danger
        ? colorTheme.errorForeground.value
        : colorTheme.primary.value
      : colorTheme.emphasizedBackground.value,
  },
  '&:focus': {
    outline: 'none',
    //  solid subdued outline in focused state
    boxShadow: `0px 0px 0px 2px ${
      props.danger
        ? colorTheme.errorForeground.o(20).value
        : props.primary
        ? colorTheme.primary.o(30).value
        : colorTheme.subduedBorder.o(80).value
    }`,
  },
  '&:active': {
    outline: 'none',
    // slightly brighter backgrounds while pressed
    backgroundColor: props.primary
      ? props.danger
        ? colorTheme.errorForeground.shade(103).value
        : colorTheme.primary.shade(103).value
      : colorTheme.emphasizedBackground.shade(98).value,
  },
}))
