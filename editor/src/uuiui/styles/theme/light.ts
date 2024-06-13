import { UtopiColor, createUtopiColor, enforceUtopiColorTheme } from '../utopi-color-helpers'
import type { dark } from './dark'

const lightBase = {
  primary: createUtopiColor('oklch(59% 0.25 254)'),
  primary10solid: createUtopiColor('oklch(0.98 0.01 253.75)'),
  primary10: createUtopiColor('oklch(59% 0.25 254 / 10%)'),
  primary25: createUtopiColor('oklch(59% 0.25 254 / 25%)'),
  primary30: createUtopiColor('oklch(59% 0.25 254 / 30%)'),
  primary50: createUtopiColor('oklch(59% 0.25 254 / 50%)'),
  component: createUtopiColor('oklch(53% 0.31 290)'),
  componentChild: createUtopiColor('oklch(83.6% 0.198 81.5)'),
  componentChild20: createUtopiColor('oklch(83.6% 0.198 81.5 / 20%)'),
  css: createUtopiColor('oklch(69% 0.18 166.76)'),
  white: createUtopiColor('oklch(100% 0 0)'),
  black: createUtopiColor('oklch(0% 0 0)'),
  brandPurple: createUtopiColor('oklch(53% 0.31 290)'),
  brandPurple70: createUtopiColor('oklch(53% 0.31 290 / 70%)'),
  brandNeonPink: createUtopiColor('oklch(72.2% 0.36 331.7)'),
  brandNeonPink10: createUtopiColor('oklch(72.53% 0.353 331.69 / 10%)'),
  brandNeonPink60: createUtopiColor('oklch(72.53% 0.353 331.69 / 30%)'),
  brandNeonGreen: createUtopiColor('oklch(86.6% 0.27 158.6)'),
  green: createUtopiColor('oklch(64.6% 0.17 150.6)'),
  green10: createUtopiColor('oklch(64.6% 0.17 150.6 / 10%)'),
  green20: createUtopiColor('oklch(64.6% 0.17 150.6 / 20%)'),
  pinkSubdued: createUtopiColor('oklch(92% 0.076 326)'),
  secondaryBlue: createUtopiColor('oklch(74.5% 0.14 241.9)'),
  secondaryOrange: createUtopiColor('oklch(78.97% 0.192 70)'),
  denimBlue: createUtopiColor('oklch(91.3% 0.04 252)'),
  lightDenimBlue: createUtopiColor('oklch(97% 0.02 254)'),
  selectionBlue: createUtopiColor('oklch(66.9% 0.18 248.8)'),
  selectionBlue10: createUtopiColor('oklch(66.9% 0.18 248.8 / 10%)'),
  selectionBlue20: createUtopiColor('oklch(66.9% 0.18 248.8 / 20%)'),
  childSelectionBlue: createUtopiColor('oklch(92.39% 0.05 236.2)'),
  selectionPurple: createUtopiColor('oklch(53.22% 0.28 289.7)'),
  childSelectionPurple: createUtopiColor('oklch(89.45% 0.06 302.7)'),
  transparent: createUtopiColor('oklch(0% 0 0 / 0%)'),
  error: createUtopiColor('oklch(66% 0.3 11.65)'),
  componentOrange: createUtopiColor('oklch(68% 0.2 42)'),
  componentPurple: createUtopiColor('oklch(53% 0.31 290)'),
  componentPurple05: createUtopiColor('oklch(53.19% 0.307 289.7 / 5%)'),
  componentPurple05solid: createUtopiColor('oklch(91.14% 0.018 303.4)'),
  dynamicBlue: createUtopiColor('oklch(59% 0.25 254)'),
  dynamicBlue10: createUtopiColor('oklch(58.98% 0.246 254.39 / 10%)'),
  dynamicBlue30: createUtopiColor('oklch(58.98% 0.246 254.39 / 30%)'),
  unavailable: createUtopiColor('oklch(54.52% 0 0 / 5%)'),
  unavailableGrey: createUtopiColor('oklch(0% 0 0 / 22%)'),
  unavailableGrey10: createUtopiColor('oklch(0% 0 0 / 10%)'),
  aqua: createUtopiColor('oklch(51.89% 0.09 211.12)'),
  aqua10: createUtopiColor('oklch(51.89% 0.09 211.12 / 10%)'),
  aqua05solid: createUtopiColor('oklch(91.86% 0.016 216.68)'),
  bg510solid: createUtopiColor('oklch(90.97% 0 0)'),
  bg0: createUtopiColor('hsl(0,0%,100%)'),
  bg1: createUtopiColor('lch(99.5 0.01 0)'),
  bg1subdued: createUtopiColor('lch(98 0.01 0)'),
  bg2: createUtopiColor('lch(96.0 0.01 0)'),
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
  bg1transparentgradient: createUtopiColor('radial-gradient(circle, #ffffff 15%, #ffffff00 80%)'),
}

const lightPrimitives = {
  // backgrounds
  emphasizedBackground: lightBase.bg0,
  emphasizedBackgroundPop: lightBase.bg1,
  emphasizedBackgroundReduced: lightBase.white,
  neutralBackground: lightBase.bg1,
  secondaryBackground: lightBase.bg2,
  subtleBackground: lightBase.bg3,
  neutralInvertedBackground: lightBase.fg1,
  dialogBackground: lightBase.bg1,
  dialogBackground2: lightBase.bg4,
  popupBorder: lightBase.bg5,

  emphasizedForeground: lightBase.fg0,
  neutralForeground: lightBase.fg1,
  subduedForeground: lightBase.fg5,
  verySubduedForeground: lightBase.fg8,
  neutralInvertedForeground: lightBase.bg0,

  neutralBorder: lightBase.border3,
  secondaryBorder: lightBase.border2,
  subduedBorder: lightBase.border1,

  checkerboardLight: createUtopiColor('oklch(100% 0 0)'),
  checkerboardDark: createUtopiColor('rgb(247 247 247)'),
}

const lightErrorStates = {
  errorForeground: createUtopiColor('oklch(66% 0.3 11.65)'),
  errorForegroundEmphasized: createUtopiColor('rgba(245,0,57,1)'),
  // TODO vv only used by button, refactor button and remove
  warningForeground: createUtopiColor('oklch(83.6% 0.198 81.5)'),
  // TODO vv only used by image-thumbnail-control, consider removing
  warningBgTranslucent: createUtopiColor('rgba(250, 94, 0, 0.2)'),
  warningBgSolid: createUtopiColor('rgba(252,142,77,1)'),
}

// TEMP colors with preset opacity pulled from within the app
const colorsWithOpacity = {
  isolator: createUtopiColor('#00000025'),
  shadow90: createUtopiColor('#00000065'),
  shadow85: createUtopiColor('#00000055'),
  shadow80: createUtopiColor('#00000045'),
  shadow75: createUtopiColor('#00000040'),
  shadow70: createUtopiColor('#00000040'),
  shadow65: createUtopiColor('#00000035'),
  shadow60: createUtopiColor('#00000035'),
  shadow55: createUtopiColor('#00000025'),
  shadow50: createUtopiColor('#00000020'),
  shadow45: createUtopiColor('#00000015'),
  shadow40: createUtopiColor('#00000010'),
  shadow35: createUtopiColor('#00000005'),
  shadow30: createUtopiColor('#00000005'),

  fg0Opacity10: createUtopiColor('hsla(0,0%,0%,0.1)'),
  fg0Opacity20: createUtopiColor('hsla(0,0%,0%,0.2)'),
  fg6Opacity50: createUtopiColor('hsla(0,0%,0%,0.5)'),
  whiteOpacity20: createUtopiColor('oklch(100% 0 0 /20%)'),
  whiteOpacity30: createUtopiColor('oklch(100% 0 0 /30%)'),
  whiteOpacity35: createUtopiColor('oklch(100% 0 0 /35%)'),
  canvasControlsSizeBoxShadowColor20: createUtopiColor('rgba(0,0,0,0.20)'),
  canvasControlsSizeBoxShadowColor50: createUtopiColor('rgba(0,0,0,0.5)'),
  neutralInvertedBackground10: createUtopiColor('hsla(0,0%,0%,0.1)'),
  neutralInvertedBackground20: createUtopiColor('hsla(0,0%,0%,0.2)'),
  neutralInvertedBackground30: createUtopiColor('hsla(0,0%,0%,0.3)'),
  // the following is used with an animation to zero opacity but same colour value
  listNewItemFlashBackground0: createUtopiColor('rgba(211, 254, 162, 0)'),
  // TODO vv only used by button, refactor & remove
  errorForeground20: createUtopiColor('rgba(253, 0, 59, 0.2)'),
  subduedBorder80: createUtopiColor('hsla(0, 0%, 91%, 0.8)'),

  cartoucheLiteralHighlightDefault: createUtopiColor('rgba(43, 43, 43, 0.1)'),
  cartoucheLiteralHighlightHovered: createUtopiColor('rgba(43, 43, 43, 0.2)'),
  cartoucheLiteralHighlightSelected: createUtopiColor('rgba(43, 43, 43, 0.5)'),
}

const lightTheme = {
  ...colorsWithOpacity,
  ...lightBase,
  ...lightPrimitives,
  ...lightErrorStates,

  textColor: createUtopiColor('oklch(21.56% 0 0)'),

  panelShadowColor: createUtopiColor('rgba(0,0,0, .3)'),
  seperator: createUtopiColor('hsl(0,0%,92%)'),

  // big sections
  leftMenuBackground: lightPrimitives.neutralBackground,
  leftPaneBackground: lightPrimitives.neutralBackground,
  inspectorBackground: lightPrimitives.neutralBackground,
  canvasBackground: lightBase.bg4,
  canvasLiveBackground: createUtopiColor('rgba(252,252,252,1)'),
  canvasLiveBorder: lightBase.primary,

  // tabs. Nb: active tab matches canvasBackground
  tabSelectedForeground: lightPrimitives.emphasizedForeground,
  tabHoveredBackground: lightPrimitives.secondaryBackground,

  // lists
  listNewItemFlashBackground: createUtopiColor('rgba(211, 254, 162, 1)'),

  // canvas controls
  canvasControlsSizeBoxBackground: createUtopiColor('white'),
  canvasControlsSizeBoxShadowColor: createUtopiColor('black'),
  canvasControlsSizeBoxBorder: createUtopiColor('hsl(0,0%,15%)'),
  canvasControlReorderSliderBoxShadowPrimary: createUtopiColor('rgba(52,52,52,0.35)'),
  canvasControlReorderSliderBoxShadowSecondary: createUtopiColor('hsl(0,0%,0%,0.5)'),
  canvasControlsCoordinateSystemMarks: lightBase.brandNeonPink,
  canvasControlsImmediateParentMarks: createUtopiColor('rgba(0, 0, 0, 0.25)'),
  canvasControlsInlineIndicatorInactive: createUtopiColor('rgba(179,215,255,1)'),
  canvasControlsInlineToggleUnsetText: createUtopiColor('rgba(179,215,255,1)'),
  canvasControlsInlineToggleHoverBackground: createUtopiColor('rgba(242,248,255,1)'),
  canvasControlsInlineToggleHoverText: createUtopiColor('rgba(26,135,255,1)'),
  canvasControlsInlineToggleActiveBackground: createUtopiColor('rgba(230,242,255,1)'),

  canvasControlsCornerOutline: createUtopiColor('rgba(103, 142, 255, 1)'),
  canvasControlsDimensionableControlShadow: createUtopiColor('rgba(140,140,140,.9)'),

  canvasSelectionPrimaryOutline: lightBase.primary,
  canvasSelectionInstanceOutline: lightBase.brandPurple,
  canvasSelectionSceneOutline: lightBase.brandPurple,
  canvasSelectionRandomDOMElementInstanceOutline: createUtopiColor('oklch(59.82% 0 0)'),
  canvasSelectionSecondaryOutline: createUtopiColor('hsla(0,0%,10%,0.5)'),
  canvasSelectionNotFocusable: createUtopiColor('oklch(59.82% 0 0)'),

  canvasSelectionFocusable: lightBase.brandPurple,
  canvasSelectionIsolatedComponent: lightBase.brandPurple,
  //Children of isolated component
  canvasSelectionNotFocusableChild: createUtopiColor('oklch(63% 0.22 41)'),
  canvasSelectionFocusableChild: lightBase.brandPurple,

  canvasLayoutStroke: lightBase.brandNeonPink,

  paddingForeground: lightBase.brandNeonGreen,
  paddingFillTranslucent: createUtopiColor('rgba(230,248,230,0.7)'),

  canvasElementBackground: createUtopiColor('rgba(230,242,255,1)'),
  canvasComponentButtonFocusable: createUtopiColor('rgba(238,237,252,1)'),
  canvasComponentButtonFocused: createUtopiColor('rgba(255,239,230,1)'),
  inspectorControlledBackground: createUtopiColor('rgba(242,248,255,1)'),

  textEditableOutline: lightBase.primary,

  // interface elements: buttons, segment controls, checkboxes etc

  inlineButtonColor: lightBase.primary,
  buttonBackground: lightBase.bg2,
  buttonHoverBackground: lightBase.bg3,
  buttonShadow: lightBase.fg9,
  buttonShadowActive: lightBase.fg8,

  // application utilities:
  navigatorResizeHintBorder: lightBase.primary,
  navigatorComponentName: lightBase.primary,
  navigatorComponentSelected: lightBase.componentChild20,
  navigatorComponentIconBorder: lightBase.componentChild,

  contextMenuBackground: createUtopiColor('#181C20'),
  contextMenuForeground: lightBase.white,
  contextMenuHighlightForeground: lightBase.white,
  contextMenuHighlightBackground: lightBase.primary,
  contextMenuSeparator: createUtopiColor('rgba(255,255,255,0.35)'),

  inspectorHoverColor: lightBase.fg8,
  inspectorFocusedColor: lightBase.primary,
  inspectorSetBorderColor: lightPrimitives.neutralBorder,
  flasherHookColor: lightBase.brandNeonPink,

  // Github pane
  githubBoxesBorder: createUtopiColor('#2D2E33'),
  gitubIndicatorConnectorLine: lightBase.black,
  githubIndicatorSuccessful: createUtopiColor('#1FCCB7'),
  githubIndicatorFailed: createUtopiColor('#FF7759'),
  githubIndicatorIncomplete: createUtopiColor('#FFFFFF00'),
  githubMUDUntracked: createUtopiColor('#09f'),
  githubMUDModified: createUtopiColor('#f90'),
  githubMUDDeleted: createUtopiColor('#f22'),
  githubMUDDefault: createUtopiColor('#ccc'),

  // Code editor loading screen
  codeEditorShimmerPrimary: lightBase.bg4,
  codeEditorShimmerSecondary: createUtopiColor('#D6D6D6'),
  codeEditorTabRowBg: lightBase.bg2,
  codeEditorTabSelectedBG: lightBase.bg1,
  codeEditorTabSelectedFG: lightBase.fg0,
  codeEditorTabSelectedBorder: lightBase.bg2,
  codeEditorBreadcrumbs: lightBase.fg5,
  codeEditorTabRowFg: lightBase.fg5,
  codeEditorGrid: createUtopiColor('#6d705b'),

  // Gap controls
  gapControlsBg: createUtopiColor('#FFA500'),
}

// all values in light must be of the type UtopiColor! This will break if you made a mistake.
export const light = enforceUtopiColorTheme(lightTheme)
