import { createUtopiColor } from '../utopi-color-helpers'
import { base } from './base'
import { light } from './light'

const darkBase = {
  darkPrimary: createUtopiColor('rgba(0,61,128,1)'),
  primary: base.blue,
  primarySubdued: createUtopiColor('rgba(0,118,247,1)'),
  primaryEmphasized: createUtopiColor('rgba(26,135,255,1)'),
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
  emphasizedBackgroundPop: createUtopiColor('rgba(0,0,0,1)'),
  emphasizedBackgroundReduced: createUtopiColor('rgba(5,5,5,1)'),
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
  errorForegroundSubdued: createUtopiColor('rgba(253,26,79,1)'),
  errorForegroundEmphasized: createUtopiColor('rgba(245,0,57,1)'),
  errorBgSolid: createUtopiColor('rgba(254,77,118,1)'),
  warningForeground: base.orange,
  warningBgTranslucent: base.orange.o(20),
  warningBgSolid: createUtopiColor('rgba(252,142,77,1)'),
}
export const dark: typeof light = {
  ...darkBase,
  ...darkPrimitives,
  ...darkErrorStates,

  textColor: base.white,

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
  canvasControlsCoordinateSystemMarks: base.neonpink,
  canvasControlsImmediateParentMarks: base.black.o(25),
  canvasControlsInlineIndicatorInactive: createUtopiColor('rgba(179,215,255,1)'),
  canvasControlsInlineToggleUnsetText: createUtopiColor('rgba(179,215,255,1)'),
  canvasControlsInlineToggleHoverBackground: createUtopiColor('rgba(242,248,255,1)'),
  canvasControlsInlineToggleHoverText: createUtopiColor('rgba(26,135,255,1)'),
  canvasControlsInlineToggleActiveBackground: createUtopiColor('rgba(230,242,255,1)'),

  canvasSelectionPrimaryOutline: darkBase.primary,
  canvasSelectionInstanceOutline: base.purple,
  canvasSelectionSceneOutline: base.purple,
  canvasSelectionRandomDOMElementInstanceOutline: base.darkgray,
  canvasSelectionAlternateOutlineYogaParent: base.neonpink,
  canvasSelectionAlternateOutlineYogaChild: createUtopiColor('rgba(255,51,255,1)'),
  canvasSelectionSecondaryOutline: base.almostBlack.o(50),
  canvasSelectionNotFocusable: base.darkgray,
  canvasDraggingPlaceholderYoga: base.neonpink.o(30),
  canvasDragOutlineBlock: darkBase.primary,
  canvasDragOutlineInline: base.red,

  canvasSelectionFocusable: base.purple,
  canvasSelectionIsolatedComponent: base.purple,
  //Children of isolated component
  canvasSelectionNotFocusableChild: base.darkorange,
  canvasSelectionFocusableChild: base.purple,

  canvasLayoutForeground: base.neonpink,
  canvasLayoutFillSolid: base.neonpink,
  canvasLayoutFillTranslucent: createUtopiColor('rgba(255,230,255,0.9)'),
  canvasLayoutStroke: base.neonpink,

  paddingForeground: base.neongreen,
  paddingFillSolid: base.neongreen,
  paddingFillTranslucent: createUtopiColor('rgba(230,248,230,0.9)'),
  paddingStroke: base.neongreen,

  selectionOutlines: createUtopiColor('rgba(255,128,255,1)'),
  canvasElementBackground: createUtopiColor('rgba(230,242,255,1)'),
  canvasComponentButtonFocusable: createUtopiColor('rgba(238,237,252,1)'),
  canvasComponentButtonFocused: createUtopiColor('rgba(255,239,230,1)'),
  inspectorControlledBackground: createUtopiColor('rgba(242,248,255,1)'),

  // interface elements: buttons, segment controls, checkboxes etc

  inlineButtonColor: darkBase.primary,
  inlineButtonColorDisabled: createUtopiColor('rgba(128,189,255,1)'),
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
