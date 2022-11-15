import { createUtopiColor } from '../utopi-color-helpers'
import { base } from './base'

const lightBase = {
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
  emphasizedBackgroundPop: createUtopiColor('rgba(252,252,252,1)'),
  emphasizedBackgroundReduced: createUtopiColor('rgba(255,255,255,1)'),
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
  errorForegroundSubdued: createUtopiColor('rgba(253,26,79,1)'),
  errorForegroundEmphasized: createUtopiColor('rgba(245,0,57,1)'),
  errorBgSolid: createUtopiColor('rgba(254,77,118,1)'),
  warningForeground: base.orange,
  warningBgTranslucent: base.orange.o(20),
  warningBgSolid: createUtopiColor('rgba(252,142,77,1)'),
}

export const light = {
  ...lightBase,
  ...lightPrimitives,
  ...lightErrorStates,

  textColor: base.almostBlack,

  // big sections
  leftMenuBackground: lightPrimitives.neutralBackground,
  leftPaneBackground: lightPrimitives.neutralBackground,
  inspectorBackground: lightPrimitives.neutralBackground,
  canvasBackground: lightPrimitives.secondaryBackground,
  canvasLiveBackground: createUtopiColor('rgba(252,252,252,1)'),
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
  canvasControlsInlineIndicatorInactive: createUtopiColor('rgba(179,215,255,1)'),
  canvasControlsInlineToggleUnsetText: createUtopiColor('rgba(179,215,255,1)'),
  canvasControlsInlineToggleHoverBackground: createUtopiColor('rgba(242,248,255,1)'),
  canvasControlsInlineToggleHoverText: createUtopiColor('rgba(26,135,255,1)'),
  canvasControlsInlineToggleActiveBackground: createUtopiColor('rgba(230,242,255,1)'),

  canvasSelectionPrimaryOutline: lightBase.primary,
  canvasSelectionInstanceOutline: base.purple,
  canvasSelectionSceneOutline: base.purple,
  canvasSelectionRandomDOMElementInstanceOutline: base.darkgray,
  canvasSelectionAlternateOutlineYogaParent: base.neonpink,
  canvasSelectionAlternateOutlineYogaChild: createUtopiColor('rgba(255,51,255,1)'),
  canvasSelectionSecondaryOutline: base.almostBlack.o(50),
  canvasSelectionNotFocusable: base.darkgray,
  canvasDraggingPlaceholderYoga: base.neonpink.o(30),
  canvasDragOutlineBlock: lightBase.primary,
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
  paddingFillTranslucent: createUtopiColor('rgba(230,248,230,0.7)'),
  paddingStroke: base.neongreen,

  selectionOutlines: createUtopiColor('rgba(255,128,255,1)'),
  canvasElementBackground: createUtopiColor('rgba(230,242,255,1)'),
  canvasComponentButtonFocusable: createUtopiColor('rgba(238,237,252,1)'),
  canvasComponentButtonFocused: createUtopiColor('rgba(255,239,230,1)'),
  inspectorControlledBackground: createUtopiColor('rgba(242,248,255,1)'),

  // interface elements: buttons, segment controls, checkboxes etc

  inlineButtonColor: lightBase.primary,
  inlineButtonColorDisabled: createUtopiColor('rgba(128,189,255,1)'),
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
