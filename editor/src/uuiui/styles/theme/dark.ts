import { createUtopiColor } from '../utopi-color-helpers'
import { base } from './base'
import type { light } from './light'

const darkBase = {
  primary: base.blue,
  primarySubdued: createUtopiColor('rgba(0,118,247,1)'),
  primaryEmphasized: createUtopiColor('rgba(26,135,255,1)'),
  component: base.purple,
  componentChild: base.orange,
  css: base.neongreen,
  white: base.white,
  black: base.black,
  brandPurple: base.purple,
  brandNeonPink: base.neonpink,
  brandNeonGreen: base.neongreen,
  jsYellow: base.jsYellow,
  secondaryBlue: createUtopiColor('#679AD1'),
  secondaryOrange: createUtopiColor('#E89A74'),
  denimBlue: createUtopiColor('#133763'),
  lightDenimBlue: createUtopiColor('#072140'),
  transparent: base.transparent,
  error: createUtopiColor('oklch(67.99% 0.261 22.81)'),
  componentOrange: createUtopiColor('oklch(80.6% 0.15 50)'),
  componentPurple: createUtopiColor('oklch(76% 0.155 300)'),
  dynamicBlue: createUtopiColor('oklch(81% 0.11 241)'),
  dynamicBlue10: createUtopiColor('oklch(81% 0.11 241 / 10%)'),

  bg0: createUtopiColor('#000000'),
  bg1: createUtopiColor('#181C20'),
  bg2: createUtopiColor('#23262F'),
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
  bg1transparentgradient: createUtopiColor('radial-gradient(circle, #181C20 15%, #181C2000 80%)'),
}

const darkPrimitives = {
  // backgrounds
  emphasizedBackground: darkBase.bg0,
  emphasizedBackgroundPop: createUtopiColor('rgba(0,0,0,1)'),
  emphasizedBackgroundReduced: createUtopiColor('rgba(5,5,5,1)'),
  neutralBackground: darkBase.bg1,
  secondaryBackground: darkBase.bg2,
  subtleBackground: darkBase.bg2,
  neutralInvertedBackground: darkBase.fg1,

  emphasizedForeground: darkBase.fg0,
  neutralForeground: darkBase.fg1,
  subduedForeground: darkBase.fg5,
  verySubduedForeground: darkBase.fg8,
  neutralInvertedForeground: darkBase.bg0,

  neutralBorder: darkBase.border1,
  secondaryBorder: darkBase.border2,
  subduedBorder: darkBase.border3,

  checkerboardLight: createUtopiColor('rgb(67,67,67)'),
  checkerboardDark: createUtopiColor('rgb(44, 45, 48)'),
}

const darkErrorStates = {
  errorForeground: darkBase.error,
  // TODO vv only used by button, refactor button and remove
  errorForegroundEmphasized: createUtopiColor('rgba(245,0,57,1)'),
  warningForeground: base.orange,
  // TODO vv only used by image-thumbnail-control, consider removing
  warningBgTranslucent: createUtopiColor('rgba(250, 94, 0, 0.2)'),
  warningBgSolid: createUtopiColor('rgba(252,142,77,1)'),
}

// TEMP colors with preset opacity pulled from within the app
const colorsWithOpacity = {
  fg0Opacity10: createUtopiColor('hsla(0,100%,100%,0.1)'),
  fg6Opacity50: createUtopiColor('rgba(111, 119, 139, 0.5)'),
  canvasControlsSizeBoxShadowColor20: createUtopiColor('rgba(255,255,255,0.20)'),
  canvasControlsSizeBoxShadowColor50: createUtopiColor('rgba(255,255,255,0.5)'),
  neutralInvertedBackground10: createUtopiColor('rgba(217, 220, 227, 0.1)'),
  neutralInvertedBackground20: createUtopiColor('rgba(217, 220, 227, 0.2)'),
  neutralInvertedBackground30: createUtopiColor('rgba(217, 220, 227, 0.3)'),
  listNewItemFlashBackground0: createUtopiColor('rgba(211, 254, 162, 0)'),
  brandPurple70: base.purple70,
  // TODO vv only used by button, refactor & remove
  errorForeground20: createUtopiColor('rgba(253, 0, 59, 0.2)'),
  primary10: base.blue10,
  primary30: base.blue30,
  subduedBorder80: createUtopiColor('rgba(24, 28, 32, 0.8)'),
}

export const dark: typeof light = {
  ...colorsWithOpacity,
  ...darkBase,
  ...darkPrimitives,
  ...darkErrorStates,

  textColor: base.white,

  panelShadowColor: createUtopiColor('rgba(0,0,0, .3)'),

  // big sections
  leftMenuBackground: darkPrimitives.neutralBackground,
  leftPaneBackground: darkPrimitives.neutralBackground,
  inspectorBackground: darkPrimitives.neutralBackground,
  canvasBackground: darkPrimitives.secondaryBackground,
  canvasLiveBackground: createUtopiColor('rgba(195,197,201,1)'),
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
  canvasControlReorderSliderBoxShadowPrimary: createUtopiColor('rgba(52,52,52,0.35)'),
  canvasControlReorderSliderBoxShadowSecondary: createUtopiColor('rgba(166,166,166,0.82)'),
  canvasControlsCoordinateSystemMarks: base.neonpink,
  canvasControlsImmediateParentMarks: createUtopiColor('rgba(0,0,0,0.25)'),
  // TODO vv refactor - only used by self-layout-subsection indirection
  canvasControlsInlineIndicatorInactive: createUtopiColor('rgba(179,215,255,1)'),
  // TODO vv refactor - following four *only* used by inline button
  canvasControlsInlineToggleUnsetText: createUtopiColor('rgba(179,215,255,1)'),
  canvasControlsInlineToggleHoverBackground: createUtopiColor('rgba(242,248,255,1)'),
  canvasControlsInlineToggleHoverText: createUtopiColor('rgba(26,135,255,1)'),
  canvasControlsInlineToggleActiveBackground: createUtopiColor('rgba(230,242,255,1)'),

  canvasControlsCornerOutline: createUtopiColor('rgba(103, 142, 255, 1)'),
  canvasControlsDimensionableControlShadow: createUtopiColor('rgba(140,140,140,.9)'),

  canvasSelectionPrimaryOutline: darkBase.primary,
  canvasSelectionInstanceOutline: base.purple,
  canvasSelectionSceneOutline: base.purple,
  canvasSelectionRandomDOMElementInstanceOutline: base.darkgray,
  canvasSelectionSecondaryOutline: createUtopiColor('rgba(217, 220, 227, 0.5)'), // fg1
  canvasSelectionNotFocusable: base.darkgray,

  canvasSelectionFocusable: base.purple,
  canvasSelectionIsolatedComponent: base.purple,
  //Children of isolated component
  canvasSelectionNotFocusableChild: base.darkorange,
  canvasSelectionFocusableChild: base.purple,

  canvasLayoutStroke: base.neonpink,

  paddingForeground: base.neongreen,
  paddingFillTranslucent: createUtopiColor('rgba(230,248,230,0.9)'),

  canvasElementBackground: createUtopiColor('rgba(230,242,255,1)'),
  canvasComponentButtonFocusable: createUtopiColor('rgba(238,237,252,1)'),
  canvasComponentButtonFocused: createUtopiColor('rgba(255,239,230,1)'),
  inspectorControlledBackground: createUtopiColor('rgba(242,248,255,1)'),

  textEditableFill: createUtopiColor('rgba(255,128,255,.07)'),
  textEditableOutline: createUtopiColor('rgba(255,128,255,1)'),

  // interface elements: buttons, segment controls, checkboxes etc

  inlineButtonColor: darkBase.primary,
  buttonBackground: darkBase.bg2,
  buttonHoverBackground: darkBase.bg3,
  buttonShadow: darkBase.fg9,
  buttonShadowActive: darkBase.fg8,

  // application utilities:
  navigatorResizeHintBorder: darkBase.primary,
  navigatorComponentName: darkBase.primary,
  navigatorComponentSelected: base.orange20,
  navigatorComponentIconBorder: base.orange,

  contextMenuBackground: darkPrimitives.secondaryBackground,
  contextMenuForeground: darkPrimitives.neutralForeground,
  contextMenuHighlightForeground: base.white,
  contextMenuHighlightBackground: darkBase.primary,
  contextMenuSeparator: createUtopiColor('rgba(0,0,0,0.1)'),

  inspectorHoverColor: darkBase.fg8,
  inspectorFocusedColor: darkBase.primary,
  inspectorSetBorderColor: darkPrimitives.neutralBorder,
  flasherHookColor: base.neonpink,

  // Github pane
  githubBoxesBorder: createUtopiColor('#282a2d'),
  gitubIndicatorConnectorLine: createUtopiColor('#686a6d'),
  githubIndicatorSuccessful: createUtopiColor('#1FCCB7'),
  githubIndicatorFailed: createUtopiColor('#FF7759'),
  githubIndicatorIncomplete: createUtopiColor('#FFFFFF00'),
  githubMUDUntracked: createUtopiColor('#09f'),
  githubMUDModified: createUtopiColor('#f90'),
  githubMUDDeleted: createUtopiColor('#f22'),
  githubMUDDefault: createUtopiColor('#ccc'),

  // Code editor loading screen
  codeEditorShimmerPrimary: darkBase.bg4,
  codeEditorShimmerSecondary: darkBase.bg5,
  codeEditorTabRowBg: darkBase.bg2,
  codeEditorTabSelectedBG: darkBase.bg1,
  codeEditorTabSelectedFG: darkBase.fg0,
  codeEditorTabSelectedBorder: darkBase.bg2,
  codeEditorBreadcrumbs: darkBase.fg5,
  codeEditorTabRowFg: darkBase.fg5,
  codeEditorGrid: createUtopiColor('#6d705b'),

  // Gap controls
  gapControls: base.neongreen,
}
