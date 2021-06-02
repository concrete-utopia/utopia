import { Interpolation } from '@emotion/react'
import { createUtopiColor } from './utopi-color-helpers'

const base = {
  blue: createUtopiColor('#007AFF', 'converted from P3 color space', 'blue'),
  purple: createUtopiColor('#574BE2', 'Brand Purple', 'purple'),
  red: createUtopiColor('#FD003B', 'Pinkish red (eg errors)', 'red'),
  orange: createUtopiColor('#FA5E00', 'Orange (eg warnings)', 'orange'),
  neonpink: createUtopiColor('#FF00FF', 'Solid Neon Pink', 'neonpink'),
  neongreen: createUtopiColor('hsl(120, 100%, 37%)', 'Solid Neon Green', 'neongreen'),
  neonyellow: createUtopiColor('#FCFF42', 'Solid Neon Yellow', 'yellow'),
  white: createUtopiColor('white', 'white', 'white'),
  almostBlack: createUtopiColor('hsl(0,0%,10%)', '10% B ---> W', 'black'),
  transparent: createUtopiColor('hsla(0,0%,0%,0)', 'transparent', 'transparent'),
  black: createUtopiColor('black', 'black', 'black'),
  darkgray: createUtopiColor('hsl(0, 0%, 50%)', 'darkgray', 'darkgray'),
  darkorange: createUtopiColor('#D05300', 'Dark Orange (Non focusable)', 'darkorange'),
}

const lightBase = {
  darkPrimary: base.blue.shade(150),
  primary: base.blue,
  component: base.purple,
  componentChild: base.orange,
  css: base.neongreen,
  white: base.white,
  transparent: base.transparent,
  brandPurple: base.purple,
  brandNeonYellow: base.neonyellow,
  brandNeonPink: base.neonpink,
  spectrumTop: base.white,
  spectrumBottom: base.black,
  bg0: createUtopiColor('hsl(0,0%,100%)', 'brightest', 'white'),
  bg1: createUtopiColor('#FAFAFA', 'neutral - upper end of hsl(0,0%,98%)', 'white'),
  bg2: createUtopiColor('hsl(0,0%,96%)', 'eg inputs', 'white'),
  bg3: createUtopiColor('hsl(0,0%,94%)', 'visible buttons', 'white'),
  bg4: createUtopiColor('hsl(0,0%,92%)', 'hover states for buttons', 'white'),
  bg5: createUtopiColor('hsl(0,0%,90%)', 'selected elements', 'grey'),
  fg0: createUtopiColor('hsl(0,0%,0%)', 'emphasized foreground', 'black'),
  fg1: createUtopiColor('hsl(0,0%,10%)', 'default foreground', 'black'),
  fg2: createUtopiColor('hsl(0,0%,20%)', 'black', 'black'),
  fg3: createUtopiColor('hsl(0,0%,30%)', 'darkgray', 'darkgray'),
  fg4: createUtopiColor('hsl(0,0%,40%)', 'darkgray', 'darkgray'),
  fg5: createUtopiColor('hsl(0,0%,50%)', 'grey', 'grey'),
  fg6: createUtopiColor('hsl(0,0%,60%)', 'grey', 'grey'),
  fg7: createUtopiColor('hsl(0,0%,70%)', 'grey', 'grey'),
  fg8: createUtopiColor('hsl(0,0%,80%)', 'lightgrey', 'lightgrey'),
  fg9: createUtopiColor('hsl(0,0%,90%)', 'lightgrey', 'lightgrey'),
}

const lightPrimitives = {
  // backgrounds
  emphasizedBackground: lightBase.bg0,
  neutralBackground: lightBase.bg1,
  secondaryBackground: lightBase.bg2,
  subtleBackground: lightBase.bg3,
  neutralInvertedBackground: lightBase.fg1,

  emphasizedForeground: lightBase.fg0,
  neutralForeground: lightBase.fg1,
  subduedForeground: lightBase.fg5,
  verySubduedForeground: lightBase.fg8,
  neutralInvertedForeground: lightBase.bg0,

  neutralBorder: createUtopiColor('hsl(0,0%,81%)', '', ''),
  secondaryBorder: createUtopiColor('hsl(0,0%,93%)', '', ''),
  subduedBorder: createUtopiColor('hsl(0,0%,95%)', '', ''),
}

const lightErrorStates = {
  errorForeground: base.red,
  errorBgSolid: base.red.shade(70),
  warningForeground: base.orange,
  warningBgTranslucent: base.orange.o(20),
  warningBgSolid: base.orange.shade(70),
}

const light = {
  ...lightBase,
  ...lightPrimitives,
  ...lightErrorStates,

  // big sections
  leftMenuBackground: lightPrimitives.neutralBackground,
  leftPaneBackground: lightPrimitives.neutralBackground,
  inspectorBackground: lightPrimitives.neutralBackground,
  canvasBackground: lightPrimitives.secondaryBackground,
  canvasLiveBackground: lightPrimitives.secondaryBackground.shade(30),
  canvasLiveBorder: lightBase.primary,

  // tabs. Nb: active tab matches canvasBackground
  tabSelectedForeground: lightPrimitives.emphasizedForeground,
  tabHoveredBackground: lightPrimitives.secondaryBackground,

  // lists
  listNewItemFlashBackground: createUtopiColor('rgb(211, 254, 162)', '', ''),

  // canvas controls
  canvasControlsSizeBoxBackground: createUtopiColor('white', 'white', 'white'),
  canvasControlsSizeBoxShadowColor: createUtopiColor('black', 'black', 'black'),
  canvasControlsSizeBoxBorder: createUtopiColor('hsl(0,0%,15%)', '', ''),
  canvasControlsCoordinateSystemMarks: base.neonpink,
  canvasControlsImmediateParentMarks: base.black.o(25),

  canvasSelectionPrimaryOutline: lightBase.primary,
  canvasSelectionInstanceOutline: base.purple,
  canvasSelectionSceneOutline: base.purple,
  canvasSelectionRandomDOMElementInstanceOutline: base.darkgray,
  canvasSelectionAlternateOutlineYogaParent: base.neonpink,
  canvasSelectionAlternateOutlineYogaChild: base.neonpink.shade(80),
  canvasSelectionSecondaryOutline: base.almostBlack.o(50),
  CanvasSelectionNotFocusable: base.darkgray,
  canvasDraggingPlaceholderYoga: base.neonpink.o(30),

  canvasSelectionFocusable: base.purple,
  canvasSelectionIsolatedComponent: base.purple,
  //Children of isolated component
  canvasSelectionNotFocusableChild: base.darkorange,
  canvasSelectionFocusableChild: base.purple,

  canvasLayoutForeground: base.neonpink,
  canvasLayoutFillSolid: base.neonpink,
  canvasLayoutFillTranslucent: base.neonpink.shade(10).o(90),
  canvasLayoutStroke: base.neonpink,

  paddingForeground: base.neongreen,
  paddingFillSolid: base.neongreen,
  paddingFillTranslucent: base.neongreen.shade(10).o(90),
  paddingStroke: base.neongreen,

  // interface elements: buttons, segment controls, checkboxes etc

  inlineButtonColor: lightBase.primary,
  buttonBackground: lightBase.bg2,
  buttonHoverBackground: lightBase.bg3,

  // application utilities:
  resizingDisplayBackground: lightBase.fg3,
  resizingDisplayForeground: createUtopiColor('hsl(0,0%,90%)', '90%', 'light'),
  navigatorResizeHintBorder: lightBase.primary,
  navigatorComponentName: lightBase.primary,
  navigatorComponentSelected: base.orange.o(20),
  navigatorComponentIconBorder: base.orange,

  contextMenuBackground: lightPrimitives.secondaryBackground,
  contextMenuForeground: lightPrimitives.neutralForeground,
  contextMenuHighlightForeground: base.white,
  contextMenuHighlightBackground: lightBase.primary,
  contextMenuSeparator: base.black.o(10),

  inspectorFocusedColor: lightBase.primary,
  inspectorSetBorderColor: lightPrimitives.neutralBorder,
  flasherHookColor: base.neonpink,
}

export const colorTheme = light

const inspectorXPadding = 8
const canvasMenuWidth = 38
const inspectorWidth = 255
const inspectorPaddedWidth = inspectorWidth - inspectorXPadding * 2

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
    inspectorPaddedWidth,
    inspectorWidth,
    canvasMenuWidth,
    inspectorModalBaseOffset: inspectorXPadding + canvasMenuWidth,
  },
  invisibleIndicatorSize: 6,
  inputBorderRadius: 2,
  color: colorTheme,
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
    boxShadow: `0px 0px 1px 0px ${lightPrimitives.neutralInvertedBackground.o(20).value}`,
  },
  editing: {
    boxShadow: `0px 0px 1px 0px ${lightPrimitives.neutralInvertedBackground.o(30).value}`,
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
  background: lightPrimitives.neutralBackground.value,
  boxShadow: 'rgba(0, 0, 0, 0.1) 0px 0px 0px 1px, rgba(0, 0, 0, 0.1) 0px 2px 7px',
  paddingTop: 4,
  paddingBottom: 4,
  borderRadius: 4,
}

export const UtopiaStyles = {
  backgrounds: {
    ...backgroundURLs,
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
}
