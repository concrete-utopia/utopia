import { createUtopiColor, enforceUtopiColorTheme } from '../utopi-color-helpers'
import type { light } from './light'

const darkBase = {
  primary: createUtopiColor('oklch(59% 0.25 254)'),
  primary10solid: createUtopiColor('oklch(0.98 0.01 253.75)'),
  primary10: createUtopiColor('oklch(59% 0.25 254 / 10%)'),
  primary25: createUtopiColor('oklch(59% 0.25 254 / 25%)'),
  primary30: createUtopiColor('oklch(59% 0.25 254 / 30%)'),
  primary50: createUtopiColor('oklch(59% 0.25 254 / 50%)'),
  component: createUtopiColor('oklch(53% 0.31 290)'),
  componentChild: createUtopiColor('oklch(83.6% 0.198 81.5)'),
  componentChild20: createUtopiColor('oklch(83.6% 0.198 81.5 / 20%)'),
  css: createUtopiColor('oklch(86.6% 0.27 158.6)'),
  white: createUtopiColor('oklch(100% 0 0)'),
  black: createUtopiColor('oklch(0% 0 0)'),
  brandPurple: createUtopiColor('oklch(53% 0.31 290)'),
  brandPurple70: createUtopiColor('oklch(53% 0.31 290 / 70%)'),
  brandNeonPink: createUtopiColor('oklch(78.64% 0.237 327.81)'),
  brandNeonPink10: createUtopiColor('oklch(78.64% 0.237 327.81 / 10%)'),
  brandNeonPink60: createUtopiColor('oklch(78.64% 0.237 327.81 / 60%)'),
  brandNeonGreen: createUtopiColor('oklch(86.6% 0.27 158.6)'),
  green: createUtopiColor('oklch(64.6% 0.17 150.6)'),
  pinkSubdued: createUtopiColor('oklch(33% 0.07 327)'),
  secondaryBlue: createUtopiColor('oklch(75.44% 0.138 251.22)'),
  secondaryOrange: createUtopiColor('oklch(81.8% 0.141 47)'),
  denimBlue: createUtopiColor('oklch(33.65% 0.09 255)'),
  lightDenimBlue: createUtopiColor('oklch(25% 0.07 255)'),
  selectionBlue: createUtopiColor('oklch(66.9% 0.18 248.8)'),
  childSelectionBlue: createUtopiColor('oklch(35.15% 0.11 243)'),
  selectionPurple: createUtopiColor('oklch(53.22% 0.28 289.7)'),
  childSelectionPurple: createUtopiColor('oklch(32.25% 0.13 293.16)'),
  transparent: createUtopiColor('oklch(0% 0 0 / 0%)'),
  error: createUtopiColor('oklch(67.99% 0.261 22.81)'),
  componentOrange: createUtopiColor('oklch(80.6% 0.15 50)'),
  componentPurple: createUtopiColor('oklch(76% 0.155 300)'),
  componentPurple05: createUtopiColor('oklch(76% 0.155 300 / 5%)'),
  componentPurple05solid: createUtopiColor('oklch(0.4 0.04 284.66)'),
  dynamicBlue: createUtopiColor('oklch(81% 0.11 241)'),
  dynamicBlue10: createUtopiColor('oklch(81% 0.11 241 / 10%)'),
  dynamicBlue30: createUtopiColor('oklch(81% 0.11 241 / 30%)'),
  unavailable: createUtopiColor('oklch(0% 0 0 / 5%)'),
  unavailableGrey: createUtopiColor('oklch(100% 0 0 / 22%)'),
  unavailableGrey10: createUtopiColor('oklch(100% 0 0 / 10%)'),
  aqua: createUtopiColor('oklch(86% 0.135 208.71)'),
  aqua10: createUtopiColor('oklch(86% 0.135 208.71 / 10%)'),
  aqua05solid: createUtopiColor('oklch(0.41 0.03 238.48)'),
  bg510solid: createUtopiColor('oklch(0.41 0.02 269.74)'),
  bg0: createUtopiColor('#000000'),
  bg1: createUtopiColor('#181C20'),
  bg1subdued: createUtopiColor('#1e2226'),
  bg2: createUtopiColor('#232630'),
  bg3: createUtopiColor('#393d49'),
  bg4: createUtopiColor('#55575f'),
  bg5: createUtopiColor('#848998'),
  fg0: createUtopiColor('#ffffff'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
  fg3: createUtopiColor('#b9bCb3'),
  fg4: createUtopiColor('#a9aCa3'),
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
  dialogBackground: darkBase.bg2,
  dialogBackground2: darkBase.bg3,
  popupBorder: darkBase.bg0,

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
  warningForeground: darkBase.componentChild,
  // TODO vv only used by image-thumbnail-control, consider removing
  warningBgTranslucent: createUtopiColor('rgba(250, 94, 0, 0.2)'),
  warningBgSolid: createUtopiColor('rgba(252,142,77,1)'),
}

// TEMP colors with preset opacity pulled from within the app
const colorsWithOpacity = {
  isolator: createUtopiColor('#00000080'),
  shadow90: createUtopiColor('#00000090'),
  shadow85: createUtopiColor('#00000085'),
  shadow80: createUtopiColor('#00000080'),
  shadow75: createUtopiColor('#00000075'),
  shadow70: createUtopiColor('#00000070'),
  shadow65: createUtopiColor('#00000065'),
  shadow60: createUtopiColor('#00000060'),
  shadow55: createUtopiColor('#00000055'),
  shadow50: createUtopiColor('#00000050'),
  shadow45: createUtopiColor('#00000045'),
  shadow40: createUtopiColor('#00000040'),
  shadow35: createUtopiColor('#00000035'),
  shadow30: createUtopiColor('#00000030'),
  fg0Opacity10: createUtopiColor('hsla(0,100%,100%,0.1)'),
  fg6Opacity50: createUtopiColor('rgba(111, 119, 139, 0.5)'),
  canvasControlsSizeBoxShadowColor20: createUtopiColor('rgba(255,255,255,0.20)'),
  canvasControlsSizeBoxShadowColor50: createUtopiColor('rgba(255,255,255,0.5)'),
  neutralInvertedBackground10: createUtopiColor('rgba(217, 220, 227, 0.1)'),
  neutralInvertedBackground20: createUtopiColor('rgba(217, 220, 227, 0.2)'),
  neutralInvertedBackground30: createUtopiColor('rgba(217, 220, 227, 0.3)'),
  listNewItemFlashBackground0: createUtopiColor('rgba(211, 254, 162, 0)'),

  // TODO vv only used by button, refactor & remove
  errorForeground20: createUtopiColor('rgba(253, 0, 59, 0.2)'),
  subduedBorder80: createUtopiColor('rgba(24, 28, 32, 0.8)'),
}

const darkTheme: typeof light = {
  ...colorsWithOpacity,
  ...darkBase,
  ...darkPrimitives,
  ...darkErrorStates,

  textColor: darkBase.white,
  panelShadowColor: createUtopiColor('rgba(0,0,0, .3)'),
  seperator: createUtopiColor('#282B35'),

  // big sections
  leftMenuBackground: darkPrimitives.neutralBackground,
  leftPaneBackground: darkPrimitives.neutralBackground,
  inspectorBackground: darkPrimitives.neutralBackground,
  canvasBackground: darkBase.bg3,
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
  canvasControlsCoordinateSystemMarks: darkBase.brandNeonPink,
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
  canvasSelectionInstanceOutline: darkBase.brandPurple,
  canvasSelectionSceneOutline: darkBase.brandPurple,
  canvasSelectionRandomDOMElementInstanceOutline: createUtopiColor('oklch(59.82% 0 0)'),
  canvasSelectionSecondaryOutline: createUtopiColor('rgba(217, 220, 227, 0.5)'), // fg1
  canvasSelectionNotFocusable: createUtopiColor('oklch(59.82% 0 0)'),

  canvasSelectionFocusable: darkBase.brandPurple,
  canvasSelectionIsolatedComponent: darkBase.brandPurple,
  //Children of isolated component
  canvasSelectionNotFocusableChild: createUtopiColor('oklch(63% 0.22 41)'),
  canvasSelectionFocusableChild: darkBase.brandPurple,

  canvasLayoutStroke: darkBase.brandNeonPink,

  paddingForeground: darkBase.brandNeonGreen,
  paddingFillTranslucent: createUtopiColor('rgba(230,248,230,0.9)'),

  canvasElementBackground: createUtopiColor('rgba(230,242,255,1)'),
  canvasComponentButtonFocusable: createUtopiColor('rgba(238,237,252,1)'),
  canvasComponentButtonFocused: createUtopiColor('rgba(255,239,230,1)'),
  inspectorControlledBackground: createUtopiColor('rgba(242,248,255,1)'),

  textEditableOutline: darkBase.primary,

  // interface elements: buttons, segment controls, checkboxes etc

  inlineButtonColor: darkBase.primary,
  buttonBackground: darkBase.bg2,
  buttonHoverBackground: darkBase.bg3,
  buttonShadow: darkBase.fg9,
  buttonShadowActive: darkBase.fg8,

  // application utilities:
  navigatorResizeHintBorder: darkBase.primary,
  navigatorComponentName: darkBase.primary,
  navigatorComponentSelected: darkBase.componentChild20,
  navigatorComponentIconBorder: darkBase.componentChild,

  contextMenuBackground: darkBase.bg0,
  contextMenuForeground: darkBase.white,
  contextMenuHighlightForeground: darkBase.white,
  contextMenuHighlightBackground: darkBase.primary,
  contextMenuSeparator: createUtopiColor('rgba(255,255,255,0.35)'),

  inspectorHoverColor: darkBase.fg8,
  inspectorFocusedColor: darkBase.dynamicBlue,
  inspectorSetBorderColor: darkPrimitives.neutralBorder,
  flasherHookColor: darkBase.brandNeonPink,

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
  gapControlsBg: darkBase.brandNeonGreen,
}

export const dark = enforceUtopiColorTheme(darkTheme)
