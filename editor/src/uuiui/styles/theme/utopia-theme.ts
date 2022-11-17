import { base } from './base'
import { dark } from './dark'
import { light } from './light'
import { generateCssVariablesFromThemeObject, ThemeObject } from './theme-helpers'

const inspectorXPadding = 8
const canvasMenuWidth = 38
const inspectorSmallWidth = 255
const inspectorLargeWidth = 300
const inspectorSmallPaddedWidth = inspectorSmallWidth - inspectorXPadding * 2

const [lightTheme, lightThemeCssVariables] = generateCssVariablesFromThemeObject(light)
const [, lightThemeAsInvertedCssVariables] = generateCssVariablesFromThemeObject(
  light,
  '--inverted',
)
const [, darkThemeCssVariables] = generateCssVariablesFromThemeObject(dark)
const [darkThemeAsInverted, darkThemeAsInvertedCssVariables] = generateCssVariablesFromThemeObject(
  dark,
  '--inverted',
)

export interface ColorTheme extends ThemeObject {
  inverted: ThemeObject
}

export const colorTheme: ColorTheme = {
  ...lightTheme,
  inverted: darkThemeAsInverted,
}

export const colorThemeCssVariables = {
  ...lightThemeCssVariables,
  inverted: darkThemeAsInvertedCssVariables,
}

export const darkColorThemeCssVariables = {
  ...darkThemeCssVariables,
  inverted: lightThemeAsInvertedCssVariables,
}

export const UtopiaTheme = {
  layout: {
    rowHorizontalPadding: 8,
    rowButtonSpacing: 4,
    rowHeight: {
      smaller: 27,
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
  inputBorderRadius: 2,
  styles: {
    inspectorSetSelectedOpacity: 1,
    inspectorUnsetSelectedOpacity: 0.3,
    inspectorSetUnselectedOpacity: 0.5,
    inspectorUnsetUnselectedOpacity: 0.3,
  },
} as const

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
    backgroundColor: colorTheme.canvasLiveBackground.value,
  },
  editing: {
    border: '1px solid transparent',
    backgroundColor: colorTheme.canvasBackground.value,
  },
}

const scene = {
  live: {
    boxShadow: `0px 0px 1px 0px ${colorTheme.neutralInvertedBackground20.value}`,
  },
  editing: {
    boxShadow: `0px 0px 1px 0px ${colorTheme.neutralInvertedBackground30.value}`,
  },
}

const backgroundURLs = {
  primary: 'url(/editor/fills/light-primaryblue-p3.png)',
  blue: 'url(/editor/fills/light-primaryblue-p3.png)',
  lightblue: 'url(/editor/fills/light-lightblue-p3.png)',
  paleblue: 'url(/editor/fills/light-paleblue-p3.png)',
  purple: 'url(/editor/fills/dark-purple-p3.png)',
  green: 'url(/editor/fills/dark-green-p3.png)',
  lightgreen: 'url(/editor/fills/light-neongreen-p3.png)',
  citronyellow: 'url(/editor/fills/light-citronyellow-p3.png)',
  red: 'url(/editor/fills/dark-red-p3.png)',
  almostBlack: 'url(/editor/fills/dark-almostblack-p3.png)',
  noise: 'url(/editor/fills/noise.gif)',
}

// see type AlertLevel in editor-state.ts

const noticeStyles: { [styleName: string]: React.CSSProperties } = {
  success: {
    backgroundColor: base.neongreen.value,
    backgroundImage: backgroundURLs.green,
    color: 'white',
  },
  info: {
    backgroundColor: '#f1f1f1',
    color: colorTheme.darkPrimary.value,
  },
  primary: {
    backgroundColor: base.blue.value,
    backgroundImage: backgroundURLs.blue,
    color: 'white',
  },
  notice: {
    backgroundColor: base.blue.value,
    backgroundImage: backgroundURLs.paleblue,
    color: 'white',
  },
  warning: {
    backgroundColor: base.red.value,
    backgroundImage: backgroundURLs.red,
    color: 'white',
  },
  error: {
    backgroundColor: base.almostBlack.value,
    backgroundImage: backgroundURLs.almostBlack,
    color: 'white',
  },
  disconnected: {
    backgroundColor: base.almostBlack.value,
    backgroundImage: backgroundURLs.noise,
    color: 'white',
  },
}

const textNoticeStyles = {
  info: {},
  success: { color: base.neongreen.value },
  primary: { color: base.blue.value },
  notice: { color: base.darkgray.value },
  warning: { color: base.red.value },
  error: { color: base.red.value },
  disconnected: { background: backgroundURLs.noise, color: 'white' },
}

const shadowStyles = {
  small: {
    boxShadow: `0px 1p 3px 0px rgba(0,0,0,.2)`,
  },
  medium: {
    boxShadow: '0px 2px 4px 1px rgba(0,0,0,0.2)',
  },
}

const popup: React.CSSProperties = {
  background: light.neutralBackground.value,
  boxShadow: `inset 0px 0px 0px .5px ${colorTheme.border3.value} , 0px 2px 4px 0px ${colorTheme.fg650.value}`,
  paddingTop: 4,
  paddingBottom: 4,
  borderRadius: 4,
}

const checkerboardBackground: Pick<
  React.CSSProperties,
  'backgroundImage' | 'backgroundSize' | 'backgroundPosition'
> = {
  backgroundImage: `
    linear-gradient(to bottom left,   #e7e7e7 25%,  transparent 25%),
    linear-gradient(to bottom left,   transparent 75%,  #e7e7e7 75%),
    linear-gradient(to bottom right,  #e7e7e7 25%,  transparent 25%),
    linear-gradient(to bottom right,  transparent 75%,  #e7e7e7 75%)`,
  backgroundSize: '12px 12px, 12px 12px, 12px 12px, 12px 12px',
  backgroundPosition: '-9px 0px, -3px -6px, 3px 6px, -3px 0',
}

export const UtopiaStyles = {
  backgrounds: {
    ...backgroundURLs,
    checkerboardBackground,
  },
  noticeStyles,
  textNoticeStyles,
  shadowStyles,
  popup,
  flexRow,
  flexColumn,
  flexCenter,
  scene,
  canvas,
} as const
