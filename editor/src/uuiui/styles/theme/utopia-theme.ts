import { color } from '../utopitrons'
import { dark } from './dark'
import { light } from './light'
import type { ThemeObject } from './theme-helpers'
import { generateCssVariablesFromThemeObject } from './theme-helpers'

const inspectorXPadding = 8
const canvasMenuWidth = 38
const inspectorSmallWidth = 255
const inspectorLargeWidth = 300
const inspectorSmallPaddedWidth = inspectorSmallWidth - inspectorXPadding * 2

const [lightTheme, lightThemeCssVariables] = generateCssVariablesFromThemeObject(light)
const [, darkThemeCssVariables] = generateCssVariablesFromThemeObject(dark)

export const colorTheme: ThemeObject = {
  ...lightTheme,
}

export const colorThemeCssVariables = {
  ...lightThemeCssVariables,
}

export const darkColorThemeCssVariables = {
  ...darkThemeCssVariables,
}

export const UtopiaTheme = {
  layout: {
    rowHorizontalPadding: 8,
    rowButtonSpacing: 4,
    rowHeight: {
      smallest: 21,
      smaller: 29,
      normal: 34,
      large: 42,
      max: 47,
    },
    inputHeight: {
      small: 18,
      default: 22,
      tall: 26,
    },
    inspectorXPadding,
    inspectorSmallPaddedWidth,
    inspectorSmallWidth: inspectorSmallWidth,
    inspectorLargeWidth: inspectorLargeWidth,
    canvasMenuWidth,
    inspectorModalBaseOffset: inspectorXPadding + canvasMenuWidth,
  },
  inputBorderRadius: 3,
  styles: {
    inspectorSetSelectedOpacity: 1,
    inspectorUnsetSelectedOpacity: 0.3,
    inspectorSetUnselectedOpacity: 0.5,
    inspectorUnsetUnselectedOpacity: 0.3,
  },
  panelStyles: {
    panelBorderRadius: 10,
  },
} as const

const shadowStyles = {
  //scenes directly on the canvas
  grounded: {
    boxShadow: `0px 1px 2px 0px ${colorTheme.shadow90.value}, 0px 2px 4px -1px ${colorTheme.shadow50.value}`,
  },
  low: {
    boxShadow: `0px 2px 4px -2px ${colorTheme.shadow80.value}, 0px 4px 8px -2px ${colorTheme.shadow45.value}`,
  },
  mid: {
    boxShadow: `0px 3px 6px -2px ${colorTheme.shadow70.value}, 0px 4px 8px -2px ${colorTheme.shadow40.value}`,
  },
  high: {
    boxShadow: `0px 4px 8px -2px ${colorTheme.shadow60.value}, 0px 6px 12px -2px ${colorTheme.shadow35.value}`,
  },
  highest: {
    boxShadow: `0px 6px 12px -2px ${colorTheme.shadow50.value}, 0px 8px 16px -2px ${colorTheme.shadow30.value}`,
  },
}

const flexRow: React.CSSProperties = {
  display: 'flex',
  flexDirection: 'row',
  alignItems: 'center',
  whiteSpace: 'nowrap',
}
const flexColumn: React.CSSProperties = {
  display: 'flex',
  flexDirection: 'column',
  whiteSpace: 'nowrap',
}
const flexCenter: React.CSSProperties = {
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'center',
  alignItems: 'center',
}

// uses borders, since outlines get hidden by other elements,
//   and inset box shadows get covered by scenes
// unaffected by zoom/scale/offsets, since it applies to outer canvas only

const canvas = {
  live: {
    border: `1px solid ${colorTheme.canvasLiveBorder.value}`,
  },
  editing: {
    border: '1px solid transparent',
  },
}

// see type AlertLevel in editor-state.ts

const noticeStyles: { [styleName: string]: React.CSSProperties } = {
  info: {
    backgroundColor: colorTheme.fg1.value,
    color: colorTheme.white.value,
  },
  warning: {
    backgroundColor: colorTheme.fg1.value,
    color: colorTheme.white.value,
  },
  notice: {
    backgroundColor: colorTheme.primary.value,
    color: colorTheme.white.value,
  },
  success: {
    backgroundColor: colorTheme.primary.value,
    color: colorTheme.white.value,
  },
  primary: {
    backgroundColor: colorTheme.primary.value,
    color: colorTheme.white.value,
  },
  error: {
    backgroundColor: colorTheme.error.value,
    color: colorTheme.white.value,
  },
  disconnected: {
    backgroundColor: colorTheme.fg1.value,
    color: colorTheme.white.value,
  },
}

const textNoticeStyles = {
  info: {},
  success: { color: colorTheme.brandNeonGreen.value },
  primary: { color: colorTheme.primary.value },
  notice: { color: colorTheme.fg5.value },
  warning: { color: colorTheme.error.value },
  error: { color: colorTheme.error.value },
  disconnected: { background: colorTheme.black.value, color: colorTheme.white.value },
}

const fontStyles = {
  monospaced: {
    fontFamily: 'Consolas, Menlo, monospace',
  },
}

const popup: React.CSSProperties = {
  background: colorTheme.neutralBackground.value,
  boxShadow: shadowStyles.high.boxShadow,
  paddingTop: 4,
  paddingBottom: 4,
  borderRadius: 4,
}

const checkerboardBackground: Pick<
  React.CSSProperties,
  'backgroundImage' | 'backgroundSize' | 'backgroundPosition'
> = {
  backgroundImage: `conic-gradient(
    ${colorTheme.checkerboardLight.value} 0.25turn,
    ${colorTheme.checkerboardDark.value} 0.25turn 0.5turn,
    ${colorTheme.checkerboardLight.value} 0.5turn 0.75turn,
    ${colorTheme.checkerboardDark.value} 0.75turn
    )`,
  backgroundSize: '12px 12px, 12px 12px, 12px 12px, 12px 12px',
  backgroundPosition: '-9px 0px, -3px -6px, 3px 6px, -3px 0',
}

const stripedBackground = (
  stripeColor: string,
  scale: number,
): { backgroundImage: string; backgroundSize: string } => ({
  backgroundImage: `linear-gradient(135deg, ${stripeColor} 24.5%, ${colorTheme.transparent.value} 24.5%, ${colorTheme.transparent.value} 50%, ${stripeColor} 50%, ${stripeColor} 74%, ${colorTheme.transparent.value} 74%, ${colorTheme.transparent.value} 100%)`,
  backgroundSize: `${4 / scale}px ${4 / scale}px`,
})

export const UtopiaStyles = {
  backgrounds: {
    checkerboardBackground,
    stripedBackground,
  },
  noticeStyles,
  textNoticeStyles,
  shadowStyles,
  popup,
  flexRow,
  flexColumn,
  flexCenter,
  canvas,
  fontStyles,
} as const
