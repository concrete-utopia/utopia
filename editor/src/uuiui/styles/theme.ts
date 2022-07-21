import { useEditorState } from './../../components/editor/store/store-hook'
import { Interpolation } from '@emotion/react'
import { Theme } from './../../components/editor/store/editor-state'
import { createUtopiColor } from './utopi-color-helpers'

const base = {
  blue: createUtopiColor('#007AFF'),
  purple: createUtopiColor('#574BE2'),
  red: createUtopiColor('#FD003B'),
  orange: createUtopiColor('#FA5E00'),
  neonpink: createUtopiColor('#FF00FF'),
  neongreen: createUtopiColor('hsl(120, 100%, 37%)'),
  neonyellow: createUtopiColor('#FCFF42'),
  almostBlack: createUtopiColor('hsl(0,0%,10%)'),
  white: createUtopiColor('white'),
  black: createUtopiColor('black'),
  darkgray: createUtopiColor('hsl(0, 0%, 50%)'),
  darkorange: createUtopiColor('#D05300'),
}

const lightBase = {
  darkPrimary: base.blue.shade(150),
  primary: base.blue,
  component: base.purple,
  componentChild: base.orange,
  css: base.neongreen,
  white: base.white,
  black: base.black,
  brandPurple: base.purple,
  brandNeonYellow: base.neonyellow,
  brandNeonPink: base.neonpink,

  bg0: createUtopiColor('hsl(0,0%,100%)'),
  bg1: createUtopiColor('#FDFDFD'),
  bg2: createUtopiColor('hsl(0,0%,96%)'),
  bg3: createUtopiColor('hsl(0,0%,94%)'),
  bg4: createUtopiColor('hsl(0,0%,92%)'),
  bg5: createUtopiColor('hsl(0,0%,90%)'),
  fg0: createUtopiColor('hsl(0,0%,0%)'),
  fg1: createUtopiColor('hsl(0,0%,10%)'),
  fg2: createUtopiColor('hsl(0,0%,20%)'),
  fg3: createUtopiColor('hsl(0,0%,30%)'),
  fg4: createUtopiColor('hsl(0,0%,40%)'),
  fg5: createUtopiColor('hsl(0,0%,50%)'),
  fg6: createUtopiColor('hsl(0,0%,60%)'),
  fg7: createUtopiColor('hsl(0,0%,70%)'),
  fg8: createUtopiColor('hsl(0,0%,80%)'),
  fg9: createUtopiColor('hsl(0,0%,90%)'),
  border0: createUtopiColor('hsl(0,0%,93%)'),
  border1: createUtopiColor('hsl(0,0%,91%)'),
  border2: createUtopiColor('hsl(0,0%,86%)'),
  border3: createUtopiColor('hsl(0,0%,83%)'),
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

  neutralBorder: lightBase.border3,
  secondaryBorder: lightBase.border2,
  subduedBorder: lightBase.border1,
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

  textColor: base.almostBlack,

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
  listNewItemFlashBackground: createUtopiColor('rgb(211, 254, 162)'),

  // canvas controls
  canvasControlsSizeBoxBackground: createUtopiColor('white'),
  canvasControlsSizeBoxShadowColor: createUtopiColor('black'),
  canvasControlsSizeBoxBorder: createUtopiColor('hsl(0,0%,15%)'),
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
  paddingFillTranslucent: base.neongreen.shade(10).o(70),
  paddingStroke: base.neongreen,

  // interface elements: buttons, segment controls, checkboxes etc

  inlineButtonColor: lightBase.primary,
  buttonBackground: lightBase.bg2,
  buttonHoverBackground: lightBase.bg3,

  // application utilities:
  resizingDisplayBackground: lightBase.fg3,
  resizingDisplayForeground: createUtopiColor('hsl(0,0%,90%)'),
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

/** DARK **/
const darkBase = {
  darkPrimary: base.blue.shade(150),
  primary: base.blue,
  component: base.purple,
  componentChild: base.orange,
  css: base.neongreen,
  white: base.white,
  black: base.black,
  brandPurple: base.purple,
  brandNeonYellow: base.neonyellow,
  brandNeonPink: base.neonpink,

  bg0: createUtopiColor('#000000'),
  bg1: createUtopiColor('#181C20'),
  bg2: createUtopiColor('#373C4A'),
  bg3: createUtopiColor('#55575F'),
  bg4: createUtopiColor('#4C4D5B'),
  bg5: createUtopiColor('#848998'),
  fg0: createUtopiColor('#ffffff'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
  fg3: createUtopiColor('b9bCb3'),
  fg4: createUtopiColor('a9aCa3'),
  fg5: createUtopiColor('#8B91A0'),
  fg6: createUtopiColor('#6F778B'),
  fg7: createUtopiColor('#525B72'),
  fg8: createUtopiColor('#2F374A'),
  fg9: createUtopiColor('#151A27'),
  border0: createUtopiColor('#181C20'),
  border1: createUtopiColor('#181C20'),
  border2: createUtopiColor('#181C20'),
  border3: createUtopiColor('#181C20'),
}

const darkPrimitives = {
  // backgrounds
  emphasizedBackground: darkBase.bg0,
  neutralBackground: darkBase.bg1,
  secondaryBackground: darkBase.bg2,
  subtleBackground: darkBase.bg3,
  neutralInvertedBackground: darkBase.fg1,

  emphasizedForeground: darkBase.fg0,
  neutralForeground: darkBase.fg1,
  subduedForeground: darkBase.fg5,
  verySubduedForeground: darkBase.fg8,
  neutralInvertedForeground: darkBase.bg0,

  neutralBorder: darkBase.border1,
  secondaryBorder: darkBase.border2,
  subduedBorder: darkBase.border3,
}

const darkErrorStates = {
  errorForeground: base.red,
  errorBgSolid: base.red.shade(70),
  warningForeground: base.orange,
  warningBgTranslucent: base.orange.o(20),
  warningBgSolid: base.orange.shade(70),
}
const dark: typeof light = {
  ...darkBase,
  ...darkPrimitives,
  ...darkErrorStates,

  textColor: base.white,

  // big sections
  leftMenuBackground: darkPrimitives.neutralBackground,
  leftPaneBackground: darkPrimitives.neutralBackground,
  inspectorBackground: darkPrimitives.neutralBackground,
  canvasBackground: darkPrimitives.secondaryBackground,
  canvasLiveBackground: darkPrimitives.secondaryBackground.shade(30),
  canvasLiveBorder: darkBase.primary,

  // tabs. Nb: active tab matches canvasBackground
  tabSelectedForeground: darkPrimitives.emphasizedForeground,
  tabHoveredBackground: darkPrimitives.secondaryBackground,

  // lists
  listNewItemFlashBackground: createUtopiColor('rgb(211, 254, 162)'),

  // canvas controls
  canvasControlsSizeBoxBackground: createUtopiColor('white'),
  canvasControlsSizeBoxShadowColor: createUtopiColor('black'),
  canvasControlsSizeBoxBorder: createUtopiColor('hsl(0,0%,15%)'),
  canvasControlsCoordinateSystemMarks: base.neonpink,
  canvasControlsImmediateParentMarks: base.black.o(25),

  canvasSelectionPrimaryOutline: darkBase.primary,
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

  inlineButtonColor: darkBase.primary,
  buttonBackground: darkBase.bg2,
  buttonHoverBackground: darkBase.bg3,

  // application utilities:
  resizingDisplayBackground: darkBase.fg3,
  resizingDisplayForeground: createUtopiColor('hsl(0,0%,90%)'),
  navigatorResizeHintBorder: darkBase.primary,
  navigatorComponentName: darkBase.primary,
  navigatorComponentSelected: base.orange.o(20),
  navigatorComponentIconBorder: base.orange,

  contextMenuBackground: darkPrimitives.secondaryBackground,
  contextMenuForeground: darkPrimitives.neutralForeground,
  contextMenuHighlightForeground: base.white,
  contextMenuHighlightBackground: darkBase.primary,
  contextMenuSeparator: base.black.o(10),

  inspectorFocusedColor: darkBase.primary,
  inspectorSetBorderColor: darkPrimitives.neutralBorder,
  flasherHookColor: base.neonpink,
}

export const colorTheme = { ...light, inverted: dark }
export const darkColorTheme = { ...dark, inverted: light }

// TODO: don't export colorTheme anymore and just export useUtopiaTheme() hook
// prerequisites: no class components and usage of UtopiaTheme.color instead of colorTheme
export const useColorTheme = () => {
  const currentTheme: Theme = useEditorState((store) => store.editor.theme, 'currentTheme')
  return currentTheme === 'dark' ? darkColorTheme : colorTheme
}

const inspectorXPadding = 8
const canvasMenuWidth = 38
const inspectorSmallWidth = 255
const inspectorLargeWidth = 300
const inspectorSmallPaddedWidth = inspectorSmallWidth - inspectorXPadding * 2

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
  boxShadow: `inset 0px 0px 0px .5px ${UtopiaTheme.color.border3.value} , 0px 2px 4px 0px ${
    UtopiaTheme.color.fg6.o(50).value
  }`,
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
