import { createUtopiColor } from './utopi-color-helpers'
import { ObjectInterpolation, css } from '@emotion/core'

const base = {
  blue: createUtopiColor('#00ABFF', 'converted from P3 color space', 'blue'),
  violet: createUtopiColor('violet', 'Component Violet', 'violet'),
  purple: createUtopiColor('#574BE2', 'Brand Purple', 'violet'),
  red: createUtopiColor('#FD003B', 'Pinkish red (eg errors)', 'red'),
  orange: createUtopiColor('#FF6300', 'Orange (eg warnings)', 'orange'),
  neonpink: createUtopiColor('#FF00FF', 'Solid Neon Pink', 'neonpink'),
  neongreen: createUtopiColor('hsl(120, 100%, 37%)', 'Solid Neon Green', 'neongreen'),
  neonyellow: createUtopiColor('#FCFF42', 'Solid Neon Yellow', 'yellow'),
  white: createUtopiColor('white', 'white', 'white'),
  almostBlack: createUtopiColor('hsl(0,0%,10%)', '10% B ---> W', 'black'),
  transparent: createUtopiColor('hsla(0,0%,0%,0)', 'transparent', 'transparent'),
  black: createUtopiColor('black', 'black', 'black'),
  darkgray: createUtopiColor('hsl(0, 0%, 50%)', 'darkgray', 'darkgray'),
}

const lightBase = {
  darkPrimary: base.blue.shade(150),
  primary: base.blue,
  component: base.violet,
  white: base.white,
  transparent: base.transparent,
  brandPurple: base.purple,
  brandNeonYellow: base.neonyellow,
  brandNeonPink: base.neonpink,
  spectrumTop: base.white,
  spectrumBottom: base.black,
}

const darkBase = lightBase

const lightPrimitives = {
  // backgrounds
  emphasizedBackground: createUtopiColor('hsl(0,0%,100%)', 'eg Navigator, Inspector', 'white'),
  slightlyEmphasizedBackground: createUtopiColor('hsl(0,0%,99.5%)', '', 'offWhite'),
  aboveNeutralBackground: createUtopiColor('hsl(0,0%,99%)', '', ''),
  neutralBackground: createUtopiColor('hsl(0,0%,98%)', 'eg Navigator, Inspector', 'offWhite'),
  secondaryBackground: createUtopiColor('hsl(0,0%,96%)', 'eg Canvas', 'offWhite'),
  subtleBackground: createUtopiColor('hsl(0,0%,82%)', 'eg Canvas', 'offWhite'),
  neutralInvertedBackground: createUtopiColor('hsl(0,0%,10%)', 'almost fully inverted', 'black'),
  emphasizedInvertedBackground: createUtopiColor('hsl(0,0%,0%)', 'black', 'black'),

  emphasizedForeground: createUtopiColor('hsl(0,0%,0%)', 'Pure Black', 'black'),
  neutralForeground: createUtopiColor('hsl(0,0%,10%)', '10% Black', 'offBlack'),
  secondaryForeground: createUtopiColor('hsl(0,0%,30%)', '30% Black', 'offBlack'),
  tertiaryForeground: createUtopiColor('hsl(0,0%,40%)', '40% Black', 'offBlack'),
  subduedForeground: createUtopiColor('hsl(0,0%,50%)', '50% Black', 'offBlack'),
  verySubduedForeground: createUtopiColor('hsl(0,0%,70%)', '70% Black', 'offBlack'),

  neutralInvertedForeground: createUtopiColor('rgb(255,255,255)', 'white', 'white'),

  neutralBorder: createUtopiColor('hsl(0,0%,81%)', '', ''),
  secondaryBorder: createUtopiColor('hsl(0,0%,93%)', '', ''),
  subduedBorder: createUtopiColor('hsl(0,0%,95%)', '', ''),

  controlledBlue: createUtopiColor('#0091FF', '', ''),
}

const darkPrimitives = {
  emphasizedBackground: createUtopiColor('hsl(0,0%,2%)', 'eg Navigator, Inspector', 'white'),
  slightlyEmphasizedBackground: createUtopiColor('hsl(0,0%,5%)', '', 'offWhite'),
  aboveNeutralBackground: createUtopiColor('hsl(0,0%,8%)', '', ''),
  neutralBackground: createUtopiColor('hsl(0,0%,10%)', 'eg Navigator, Inspector', 'offWhite'),
  secondaryBackground: createUtopiColor('hsl(0,0%,12%)', 'eg Canvas', 'offWhite'),
  subtleBackground: createUtopiColor('hsl(0,0%,20%)', 'eg Canvas', 'offWhite'),
  neutralInvertedBackground: createUtopiColor('hsl(0,0%,90%)', 'almost fully inverted', 'black'),
  emphasizedInvertedBackground: createUtopiColor('hsl(0,0%,100%)', 'white', 'black'),

  emphasizedForeground: createUtopiColor('hsl(0,0%,100%)', 'Pure Black', 'black'),
  neutralForeground: createUtopiColor('hsl(0,0%,90%)', '10% Black', 'offBlack'),
  secondaryForeground: createUtopiColor('hsl(0,0%,70%)', '30% Black', 'offBlack'),
  tertiaryForeground: createUtopiColor('hsl(0,0%,50%)', '40% Black', 'offBlack'),
  subduedForeground: createUtopiColor('hsl(0,0%,50%)', '50% Black', 'offBlack'),
  verySubduedForeground: createUtopiColor('hsl(0,0%,70%)', '70% Black', 'offBlack'),

  neutralBorder: createUtopiColor('hsl(0,0%,12%)', '', ''),
  secondaryBorder: createUtopiColor('hsl(0,0%,15%)', '', ''),
  subduedBorder: createUtopiColor('hsl(0,0%,20%)', '', ''),
}

const lightErrorStates = {
  errorForeground: base.red,
  errorBgTranslucent: base.red.o(20),
  errorBgSolid: base.red.shade(70),
  warningForeground: base.orange,
  warningBgTranslucent: base.orange.o(20),
  warningBgSolid: base.orange.shade(70),
}

const darkErrorStates = lightErrorStates

const lightTypography = {
  titleForeground: createUtopiColor('hsl(0,0%,10%)', '10% Black', 'offBlack'),
  h1Foreground: createUtopiColor('hsl(0,0%,10%)', '10% Black', 'offBlack'),
  h2Foreground: createUtopiColor('hsl(0,0%,10%)', '10% Black', 'offBlack'),
  h3Foreground: createUtopiColor('hsl(0,0%,30%)', '30% Black', 'offBlack'),
  h2ForegroundSubdued: createUtopiColor('hsl(0,0%,37%)', '37% Black', 'offBlack'),
}

const darkTypography = {
  titleForeground: createUtopiColor('hsl(0,0%,90%)', '10% Black', 'offBlack'),
  h1Foreground: createUtopiColor('hsl(0,0%,90%)', '10% Black', 'offBlack'),
  h2Foreground: createUtopiColor('hsl(0,0%,90%)', '10% Black', 'offBlack'),
  h3Foreground: createUtopiColor('hsl(0,0%,70%)', '30% Black', 'offBlack'),
  h2ForegroundSubdued: createUtopiColor('hsl(0,0%,60%)', '37% Black', 'offBlack'),
}

const lightControls = {
  inputBackground: lightPrimitives.emphasizedBackground,
  inputBorder: lightPrimitives.neutralBorder,
  inputColor: base.black, // i dunno what this should actually be but it needs to be black somehow
}

const darkControls = {
  inputBackground: darkPrimitives.emphasizedBackground,
  inputBorder: darkPrimitives.secondaryBorder,
  inputColor: darkPrimitives.neutralForeground,
}

const light = {
  ...lightBase,
  ...lightPrimitives,
  ...lightTypography,
  ...lightErrorStates,
  ...lightControls,

  // big sections
  leftMenuBackground: lightPrimitives.slightlyEmphasizedBackground,
  leftPaneBackground: lightPrimitives.slightlyEmphasizedBackground,
  canvasBackground: lightPrimitives.secondaryBackground,
  canvasLiveBackground: lightPrimitives.secondaryBackground.shade(30),
  canvasLiveBorder: lightBase.primary,
  inspectorBackground: lightPrimitives.neutralBackground,
  inspectorEmphasizedBackground: lightPrimitives.slightlyEmphasizedBackground,
  tabRailBackground: createUtopiColor('rgba(255,255,255,0)', 'transparent', 'transparent'),

  // tabs. Nb: active tab matches canvasBackground
  tabSelectedBackground: lightPrimitives.secondaryBackground,
  tabSelectedForeground: lightPrimitives.emphasizedForeground,
  tabHoveredBackground: lightPrimitives.secondaryBackground,
  tabNotSelectedBackground: createUtopiColor('rgba(255,255,255,0)', 'transparent', 'transparent'),
  tabNotSelectedForeground: lightPrimitives.tertiaryForeground,

  // lists
  listDropTargetBackground: lightBase.primary.shade(10).value,
  listDropTargetOutline: lightBase.primary,
  listNewItemFlashBackground: createUtopiColor('rgb(211, 254, 162)', '', ''),

  // sections and subsections
  sectionHeaderBackground: lightPrimitives.slightlyEmphasizedBackground,

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
  canvasDraggingPlaceholderYoga: base.neonpink.o(30),

  canvasLayoutForeground: base.neonpink,
  canvasLayoutFillSolid: base.neonpink,
  canvasLayoutFillTranslucent: base.neonpink.shade(10).o(90),
  canvasLayoutStroke: base.neonpink,

  paddingForeground: base.neongreen,
  paddingFillSolid: base.neongreen,
  paddingFillTranslucent: base.neongreen.shade(10).o(90),
  paddingStroke: base.neongreen,

  // interface elements: buttons, segment controls, checkboxes etc
  inputBackground: base.white,
  segmentControlBackground: base.white.shade(90),
  segmentControlActiveSegmentBackground: base.white.shade(95),

  buttonBackground: base.black.shade(0),
  buttonActiveBackground: base.black.shade(3),
  buttonDisabledBackground: base.black.shade(15).o(50),

  // application utilities:
  resizingDisplayBackground: createUtopiColor(
    'hsl(0,0%,30%)',
    '30%, eg used in sizeBox',
    'darkgray',
  ),
  resizingDisplayForeground: createUtopiColor('hsl(0,0%,90%)', '90%', 'light'),
  navigatorResizeHintBorder: lightBase.primary,
  navigatorComponentName: lightBase.primary,

  // inverted: toasts, context menu, notifications
  contextMenuBackground: lightPrimitives.neutralBackground,
  contextMenuForeground: lightPrimitives.neutralForeground,
  contextMenuHighlightForeground: base.white,
  contextMenuHighlightBackground: lightBase.primary,
  contextMenuSeparator: base.black.o(10),

  notificationUnivesalForeground: base.white,
  notificationSuccessBackground: base.neongreen,
  notificationInfoBackground: base.blue,
  notificationAlertBackground: base.purple,
  notificationWarningBackground: base.red,
  notificationErrorBackground: base.red,

  inspectorSetMainColor: lightControls.inputColor,
  inspectorSetSecondaryColor: createUtopiColor('hsl(0,0%,85%)', 'eg unset pins ', ''),
  inspectorSetBorderColor: lightControls.inputBorder,
  inspectorSetBackgroundColor: lightControls.inputBackground,
  inspectorSetSegmentSelectorColor: lightControls.inputBackground,
  inspectorSetSegmentTrackColor: createUtopiColor('rgb(246, 246, 246)', '', ''),
  inspectorUnsetMainColor: lightControls.inputColor,
  inspectorUnsetSecondaryColor: createUtopiColor('hsl(0,0%,85%)', 'eg unset pins ', ''),
  inspectorUnsetBorderColor: lightControls.inputBorder,
  inspectorUnsetBackgroundColor: lightPrimitives.neutralBackground.o(0),
  inspectorUnsetSegmentSelectorColor: createUtopiColor('rgb(246, 246, 246)', '', ''),
  inspectorUnsetSegmentTrackColor: createUtopiColor('rgb(246, 246, 246)', '', ''),
  inspectorDisabledMainColor: createUtopiColor('rgba(102, 102, 102, 1)', '', ''),
  inspectorDisabledSecondaryColor: createUtopiColor('rgba(102, 102, 102, 1)', '', ''),
  inspectorDisabledBackgroundColor: createUtopiColor('rgb(246, 246, 246)', '', ''),
  inspectorDisabledSegmentSelectorColor: createUtopiColor('rgb(246, 246, 246)', '', ''),
  inspectorDisabledSegmentTrackColor: createUtopiColor('rgb(246, 246, 246)', '', ''),
  inspectorDisabledBorderColor: lightControls.inputBorder,
  inspectorUneditableMainColor: createUtopiColor('#007AFF', '', ''),
  inspectorUneditableSecondaryColor: createUtopiColor('#007AFF', '', ''),
  inspectorUneditableBackgroundColor: createUtopiColor('#007AFF', '', ''),
  inspectorUneditableBorderColor: createUtopiColor('rgb(204, 204, 204)', '', ''),
  inspectorControlledMainColor: createUtopiColor('white', '', ''),
  inspectorControlledBorderColor: createUtopiColor('#0091FF', '', ''),
  inspectorControlledBackgroundColor: createUtopiColor('#0091FF', '', ''),
  inspectorControlledSegmentSelectorColor: createUtopiColor('#0091FF', '', ''),
  inspectorControlledSegmentTrackColor: createUtopiColor('#EDEDED', '', ''),
  inspectorDetectedMainColor: lightBase.primary,
  inspectorDetectedSecondaryColor: createUtopiColor('hsl(0,0%,85%)', 'eg unset pins ', ''),
  inspectorDetectedBorderColor: lightControls.inputBorder,
  inspectorDetectedBackgroundColor: lightControls.inputBackground,
  inspectorDetectedSegmentSelectorColor: lightControls.inputBackground,
  inspectorDetectedSegmentTrackColor: createUtopiColor('rgb(246, 246, 246)', '', ''),
  inspectorOffMainColor: createUtopiColor('rgb(255, 255, 255)', '', ''),
  inspectorOffSecondaryColor: createUtopiColor('rgb(255, 255, 255)', '', ''),
  inspectorOffBackgroundColor: createUtopiColor('rgb(255, 255, 255)', '', ''),
  inspectorOffSegmentSelectorColor: createUtopiColor('rgb(255, 255, 255)', '', ''),
  inspectorOffSegmentTrackColor: createUtopiColor('rgb(255, 255, 255)', '', ''),
  inspectorOffBorderColor: createUtopiColor('rgb(240, 240, 240)', '', ''),
  inspectorFocusedColor: createUtopiColor('#0091FF', '', ''),

  flasherHookColor: base.neonpink,
}

const dark = {
  ...darkBase,
  ...darkPrimitives,
  ...darkTypography,
  ...darkErrorStates,
  ...darkControls,

  // big sections
  leftMenuBackground: darkPrimitives.slightlyEmphasizedBackground,
  leftPaneBackground: darkPrimitives.slightlyEmphasizedBackground,
  canvasBackground: darkPrimitives.emphasizedBackground,
  canvasLiveBackground: darkPrimitives.secondaryBackground.shade(120),
  inspectorBackground: darkPrimitives.neutralBackground,
  inspectorEmphasizedBackground: darkPrimitives.slightlyEmphasizedBackground,
  tabRailBackground: createUtopiColor('rgba(0,0,0,0)', 'transparent', 'transparent'),

  // tabs. Nb: these are inverted, so that *active* tab matches canvas, code editor
  tabSelectedBackground: darkPrimitives.secondaryBackground,
  tabSelectedForeground: darkPrimitives.emphasizedForeground,
  tabHoveredBackground: darkPrimitives.secondaryBackground,
  tabNotSelectedBackground: darkPrimitives.neutralBackground,
  tabNotSelectedForeground: darkPrimitives.secondaryForeground,

  // lists
  listDropTargetBackground: darkBase.primary.shade(130).value,

  // sections and subsections
  sectionHeaderBackground: darkPrimitives.slightlyEmphasizedBackground,

  // canvas controls

  canvasControlsSizeBoxBackground: darkBase.primary,
  canvasControlsSizeBoxBorder: darkBase.primary,

  canvasSelectionPrimaryOutline: darkBase.primary,
  canvasSelectionInstanceOutline: base.purple,
  canvasSelectionSceneOutline: base.purple,
  canvasSelectionRandomDOMElementInstanceOutline: base.darkgray,
  canvasSelectionAlternateOutlineYogaParent: base.neonpink,
  canvasSelectionAlternateOutlineYogaChild: base.neonpink.shade(80),
  canvasSelectionSecondaryOutline: base.almostBlack.o(50),
  canvasDraggingPlaceholderYoga: base.neonpink.o(30),

  canvasLayoutForeground: base.neonpink,
  canvasLayoutFillSolid: base.neonpink,
  canvasLayoutFillTranslucent: base.neonpink.shade(10).o(90),
  canvasLayoutStroke: base.neonpink,

  paddingForeground: base.neongreen,
  paddingFillSolid: base.neongreen,
  paddingFillTranslucent: base.neongreen.shade(10).o(90),
  paddingStroke: base.neongreen,

  // interface elements: buttons, segment controls, checkboxes etc
  inputBackground: base.black,
  segmentControlBackground: base.black.shade(110),
  segmentControlActiveSegmentBackground: base.black.shade(105),

  buttonBackground: base.black.shade(15),
  buttonDisabledBackground: base.black.shade(15).o(50),

  // application utilities:
  resizingDisplayBackground: createUtopiColor('orange', '', 'darkgray'),
  resizingDisplayForeground: createUtopiColor('hsl(0,0%,90%)', '90%', 'light'),
  navigatorResizeHintBorder: lightBase.primary,
  navigatorComponentName: lightBase.primary,

  // inverted: toasts, context menu, notifications
  toastBackground: base.purple,
  toastForeground: base.white,
  contextMenuBackground: lightPrimitives.neutralBackground,
  contextMenuForeground: lightPrimitives.neutralForeground,
  contextMenuHighlightForeground: base.white,
  contextMenuHighlightBackground: lightBase.primary,

  contextMenuSeparator: base.white.o(10),
  notificationAlertBackground: base.purple,
  notificationAlertForeground: base.white,
  notificationInfoBackground: base.neonyellow,
  notificationInfoForeground: base.neonpink,

  inspectorSetMainColor: darkControls.inputColor,
  inspectorSetSecondaryColor: darkControls.inputColor.shade(90),
  inspectorSetBorderColor: darkControls.inputBorder,
  inspectorSetBackgroundColor: darkControls.inputBackground,
  inspectorSetSegmentSelectorColor: darkControls.inputBackground,
  inspectorSetSegmentTrackColor: darkControls.inputColor.shade(90),
  inspectorUnsetMainColor: darkControls.inputColor.shade(70),
  inspectorUnsetSecondaryColor: darkControls.inputColor.shade(70),
  inspectorUnsetBorderColor: darkControls.inputBorder,
  inspectorUnsetBackgroundColor: lightPrimitives.neutralBackground.o(0),
  inspectorUnsetSegmentSelectorColor: darkControls.inputBackground.shade(110),
  inspectorUnsetSegmentTrackColor: darkControls.inputBackground.shade(110),
  inspectorDisabledMainColor: createUtopiColor('rgba(102, 102, 102, 1)', '', ''),
  inspectorDisabledSecondaryColor: createUtopiColor('rgba(102, 102, 102, 1)', '', ''),
  inspectorDisabledBackgroundColor: darkControls.inputBackground.shade(110),
  inspectorDisabledSegmentSelectorColor: darkControls.inputBackground.shade(110),
  inspectorDisabledSegmentTrackColor: darkControls.inputBackground.shade(110),
  inspectorDisabledBorderColor: darkControls.inputBorder,
  inspectorUneditableMainColor: darkBase.primary,
  inspectorUneditableSecondaryColor: createUtopiColor('rgb(204, 204, 204)', '', ''),
  inspectorUneditableBackgroundColor: darkBase.primary,
  inspectorUneditableBorderColor: createUtopiColor('rgb(204, 204, 204)', '', ''),
  inspectorControlledMainColor: darkBase.primary,
  inspectorControlledBorderColor: darkBase.primary.o(50),
  inspectorControlledBackgroundColor: darkBase.primary.o(25),
  inspectorControlledSegmentSelectorColor: darkBase.primary.o(25),
  inspectorControlledSegmentTrackColor: darkBase.primary.o(5),
  inspectorDetectedMainColor: darkBase.primary,
  inspectorDetectedSecondaryColor: darkControls.inputColor.shade(90),
  inspectorDetectedBorderColor: darkControls.inputBorder,
  inspectorDetectedBackgroundColor: darkControls.inputBackground,
  inspectorDetectedSegmentSelectorColor: darkControls.inputBackground,
  inspectorDetectedSegmentTrackColor: darkControls.inputColor.shade(90),
  inspectorOffMainColor: base.black,
  inspectorOffSecondaryColor: base.black,
  inspectorOffBackgroundColor: base.black,
  inspectorOffSegmentSelectorColor: base.black,
  inspectorOffSegmentTrackColor: base.black,
  inspectorOffBorderColor: base.black,
  inspectorFocusedColor: darkBase.primary,
}

const lightVisualScriptTheme = {
  Set: light.primary,
  Get: base.neongreen,
  Math: light.neutralForeground,
  Animation: light.primary,
  Logic: light.neutralForeground,
  Utility: light.neutralForeground,
  Color: light.neutralForeground,
  Strings: light.primary,
  Interaction: base.neonpink,
  Debug: base.neonyellow,
}

export const colorTheme = light

export const colorThemeVisualScript = lightVisualScriptTheme

export const UtopiaTheme = {
  layout: {
    rowHorizontalPadding: 8,
    rowButtonSpacing: 4,
    rowHeight: {
      smaller: 29,
      small: 32,
      medium: 34,
      mediumLarge: 38,
      large: 42,
      max: 47,
    },
    gridRowHeight: {
      normal: 34,
      tall: 47,
    },
    inputHeight: {
      small: 18,
      default: 22,
      tall: 26,
    },
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

const input: ObjectInterpolation<any> = {
  borderRadius: '2px',
  border: '1px solid grey',
  backgroundColor: 'white',
  outline: 'none',
  padding: '2px',
  '&:focus': { border: '1px solid #0091FF' },
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

const noticeStyles = {
  success: { background: backgroundURLs.green, color: 'white' },
  info: { background: 'white', color: colorTheme.darkPrimary.value },
  primary: { background: backgroundURLs.blue, color: 'white' },
  notice: { background: backgroundURLs.paleblue, color: 'white' },
  warning: { background: backgroundURLs.red, color: 'white' },
  error: { background: backgroundURLs.almostBlack, color: 'white' },
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

const textBackgroundStyles = {
  primary: {
    color: colorTheme.primary.value,
    background: `${backgroundURLs.blue}`,
    WebkitBackgroundClip: 'text',
    WebkitTextFillColor: 'transparent',
  },
}

export const UtopiaStyles = {
  backgrounds: {
    ...backgroundURLs,
  },
  noticeStyles,
  textBackgroundStyles,
  shadowStyles,
  input,
  flexRow,
  flexColumn,
  flexCenter,
  scene,
  canvas,
}
