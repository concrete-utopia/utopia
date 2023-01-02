import { UtopiColor } from '../utopi-color-helpers'

export type SubThemeObject = {
  name: string
  iconColor: string
  bg0: UtopiColor
  fg0: UtopiColor
  fg1: UtopiColor
  fg2: UtopiColor
}

type ThemeBase = {
  darkPrimary: UtopiColor
  primary: UtopiColor
  primarySubdued: UtopiColor
  primaryEmphasized: UtopiColor
  component: UtopiColor
  componentChild: UtopiColor
  css: UtopiColor
  white: UtopiColor
  black: UtopiColor
  brandPurple: UtopiColor
  brandNeonYellow: UtopiColor
  brandNeonPink: UtopiColor
  jsYellow: UtopiColor
  secondaryBlue: UtopiColor
  secondaryOrange: UtopiColor
  transparent: UtopiColor

  bg0: UtopiColor
  bg1: UtopiColor
  bg2: UtopiColor
  bg3: UtopiColor
  bg4: UtopiColor
  bg5: UtopiColor
  fg0: UtopiColor
  fg1: UtopiColor
  fg2: UtopiColor
  fg3: UtopiColor
  fg4: UtopiColor
  fg5: UtopiColor
  fg6: UtopiColor
  fg7: UtopiColor
  fg8: UtopiColor
  fg9: UtopiColor
  fgError: UtopiColor
  fgWarning: UtopiColor
  border0: UtopiColor
  border1: UtopiColor
  border2: UtopiColor
  border3: UtopiColor
}

type ThemePrimitives = {
  // backgrounds
  emphasizedBackground: UtopiColor
  emphasizedBackgroundPop: UtopiColor
  emphasizedBackgroundReduced: UtopiColor
  neutralBackground: UtopiColor
  secondaryBackground: UtopiColor
  subtleBackground: UtopiColor
  neutralInvertedBackground: UtopiColor

  emphasizedForeground: UtopiColor
  neutralForeground: UtopiColor
  subduedForeground: UtopiColor
  verySubduedForeground: UtopiColor
  neutralInvertedForeground: UtopiColor

  neutralBorder: UtopiColor
  secondaryBorder: UtopiColor
  subduedBorder: UtopiColor

  checkerboardLight: UtopiColor
  checkerboardDark: UtopiColor
}

type ThemeErrorStates = {
  errorForeground: UtopiColor
  errorForegroundSubdued: UtopiColor
  errorForegroundEmphasized: UtopiColor
  errorBgSolid: UtopiColor
  warningForeground: UtopiColor
  warningBgTranslucent: UtopiColor
  warningBgSolid: UtopiColor
}

// TEMP colors with preset opacity pulled from within the app
type ThemeOpacities = {
  bg0Opacity90: UtopiColor
  bg0Opacity10: UtopiColor
  fg0Opacity10: UtopiColor
  fg6Opacity50: UtopiColor
  canvasControlsSizeBoxShadowColor20: UtopiColor
  canvasControlsSizeBoxShadowColor21: UtopiColor
  canvasControlsSizeBoxShadowColor50: UtopiColor
  canvasLayoutStroke20: UtopiColor
  brandNeonPink30: UtopiColor
  neutralForeground40: UtopiColor
  neutralInvertedBackground10: UtopiColor
  neutralInvertedBackground20: UtopiColor
  neutralInvertedBackground30: UtopiColor
  listNewItemFlashBackground0: UtopiColor
  brandPurple70: UtopiColor
  errorForeground20: UtopiColor
  primary30: UtopiColor
  subduedBorder80: UtopiColor
}

export type FlatThemeObject = ThemeOpacities &
  ThemeErrorStates &
  ThemePrimitives &
  ThemeBase & {
    textColor: UtopiColor

    // big sections
    leftMenuBackground: UtopiColor
    leftPaneBackground: UtopiColor
    inspectorBackground: UtopiColor
    canvasBackground: UtopiColor
    canvasLiveBackground: UtopiColor
    canvasLiveBorder: UtopiColor

    // tabs. Nb:UtopiColord
    tabSelectedForeground: UtopiColor
    tabHoveredBackground: UtopiColor

    // lists
    listNewItemFlashBackground: UtopiColor

    // canvas controls
    canvasControlsSizeBoxBackground: UtopiColor
    canvasControlsSizeBoxShadowColor: UtopiColor
    canvasControlsSizeBoxBorder: UtopiColor
    canvasControlReorderSliderBoxShadowPrimary: UtopiColor
    canvasControlReorderSliderBoxShadowSecondary: UtopiColor
    canvasControlsCoordinateSystemMarks: UtopiColor
    canvasControlsImmediateParentMarks: UtopiColor
    canvasControlsInlineIndicatorInactive: UtopiColor
    canvasControlsInlineToggleUnsetText: UtopiColor
    canvasControlsInlineToggleHoverBackground: UtopiColor
    canvasControlsInlineToggleHoverText: UtopiColor
    canvasControlsInlineToggleActiveBackground: UtopiColor

    canvasControlsCornerOutline: UtopiColor
    canvasControlsDimensionableControlShadow: UtopiColor

    canvasSelectionPrimaryOutline: UtopiColor
    canvasSelectionInstanceOutline: UtopiColor
    canvasSelectionSceneOutline: UtopiColor
    canvasSelectionRandomDOMElementInstanceOutline: UtopiColor
    canvasSelectionAlternateOutlineYogaParent: UtopiColor
    canvasSelectionAlternateOutlineYogaChild: UtopiColor
    canvasSelectionSecondaryOutline: UtopiColor
    canvasSelectionNotFocusable: UtopiColor
    canvasDraggingPlaceholderYoga: UtopiColor
    canvasDragOutlineBlock: UtopiColor
    canvasDragOutlineInline: UtopiColor

    canvasSelectionFocusable: UtopiColor
    canvasSelectionIsolatedComponent: UtopiColor
    //Children of isolated component
    canvasSelectionNotFocusableChild: UtopiColor
    canvasSelectionFocusableChild: UtopiColor

    canvasLayoutForeground: UtopiColor
    canvasLayoutFillSolid: UtopiColor
    canvasLayoutFillTranslucent: UtopiColor
    canvasLayoutStroke: UtopiColor

    paddingForeground: UtopiColor
    paddingFillSolid: UtopiColor
    paddingFillTranslucent: UtopiColor
    paddingStroke: UtopiColor

    selectionOutlines: UtopiColor
    canvasElementBackground: UtopiColor
    canvasComponentButtonFocusable: UtopiColor
    canvasComponentButtonFocused: UtopiColor
    inspectorControlledBackground: UtopiColor

    // interface elements:UtopiColorc

    inlineButtonColor: UtopiColor
    inlineButtonColorDisabled: UtopiColor
    buttonBackground: UtopiColor
    buttonHoverBackground: UtopiColor
    buttonShadow: UtopiColor
    buttonShadowActive: UtopiColor

    // application utilitiesUtopiColor
    resizingDisplayBackground: UtopiColor
    resizingDisplayForeground: UtopiColor
    navigatorResizeHintBorder: UtopiColor
    navigatorComponentName: UtopiColor
    navigatorComponentSelected: UtopiColor
    navigatorComponentIconBorder: UtopiColor

    contextMenuBackground: UtopiColor
    contextMenuForeground: UtopiColor
    contextMenuHighlightForeground: UtopiColor
    contextMenuHighlightBackground: UtopiColor
    contextMenuSeparator: UtopiColor

    inspectorHoverColor: UtopiColor
    inspectorFocusedColor: UtopiColor
    inspectorSetBorderColor: UtopiColor
    flasherHookColor: UtopiColor

    // Github pane
    githubBoxesBorder: UtopiColor
    gitubIndicatorConnectorLine: UtopiColor
    githubIndicatorSuccessful: UtopiColor
    githubIndicatorFailed: UtopiColor
    githubIndicatorIncomplete: UtopiColor
    githubMUDUntracked: UtopiColor
    githubMUDModified: UtopiColor
    githubMUDDeleted: UtopiColor
    githubMUDDefault: UtopiColor

    // Code editor
    codeEditorShimmerPrimary: UtopiColor
    codeEditorShimmerSecondary: UtopiColor
    codeEditorTabRowBg: UtopiColor
    codeEditorTabSelectedBG: UtopiColor
    codeEditorTabSelectedFG: UtopiColor
    codeEditorTabSelectedBorder: UtopiColor
    codeEditorBreadcrumbs: UtopiColor
    codeEditorTabRowFg: UtopiColor
    codeEditorGrid: UtopiColor
  }

export type SubThemesParent = {
  errorEmphasized: SubThemeObject
  pullLozenge: SubThemeObject
  pushLozenge: SubThemeObject
  warningEmphasized: SubThemeObject
  navigatorItemHighlighted: SubThemeObject
  errorForegroundTheme: SubThemeObject
}

export type ThemeObject = FlatThemeObject & SubThemesParent
