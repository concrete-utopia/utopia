import * as React from 'react'
import { Icn, IcnProps } from './icn'
import { betterReactMemo } from '../utils/react-performance'

const makeIcon = (appliedProps: IcnProps): React.FunctionComponent<Omit<IcnProps, 'type'>> =>
  betterReactMemo(`icon-${appliedProps.type}`, (props) => <Icn {...appliedProps} {...props} />)

/**
 * Provides a set of Icon components with overrideable props
 * The main props (from <Icn />) are
 * @param color: white, blue, purple, darkgray, gray, red, others
 * @param size: number, typically 18 or 16. For irregular shapes, use width and height instead
 * @param disabled: boolean
 * @param tooltipText: string. Add to wrap the icon in a tooltip
 * as well as all standard React component props, and width and height
 */

export const LargerIcons = {
  MagnifyingGlass: makeIcon({
    type: 'magnifyingglass-larger',
    color: 'black',
    width: 18,
    height: 18,
  }),
  MagnifyingGlassPlus: makeIcon({
    type: 'magnifyingglass-plus-larger',
    color: 'black',
    width: 18,
    height: 18,
  }),
  MagnifyingGlassMinus: makeIcon({
    type: 'magnifyingglass-minus-larger',
    color: 'black',
    width: 18,
    height: 18,
  }),
  Code: makeIcon({ type: 'codymccodeface-larger', color: 'black', width: 18, height: 18 }),
  Node: makeIcon({ type: 'nodymcnodeface-larger', color: 'black', width: 18, height: 18 }),
  DesignTool: makeIcon({ type: 'designtool-larger', color: 'black', width: 18, height: 18 }),
  PlayButton: makeIcon({ type: 'playbutton-larger', color: 'black', width: 18, height: 18 }),
  PlusButton: makeIcon({ type: 'plusbutton-larger', color: 'black', width: 18, height: 18 }),
  Hamburgermenu: makeIcon({ type: 'hamburgermenu-larger', color: 'black', width: 18, height: 18 }),
  HamburgermenuRotated: makeIcon({
    type: 'hamburgermenu-rotated-larger',
    color: 'black',
    width: 18,
    height: 18,
  }),
  Canvas: makeIcon({ type: 'canvas-larger', color: 'black', width: 18, height: 18 }),
  Inspector: makeIcon({ type: 'inspector-larger', color: 'black', width: 18, height: 18 }),
  StopButton: makeIcon({ type: 'stopbutton', color: 'black', width: 18, height: 18 }),
  Refresh: makeIcon({ type: 'refresh-larger', color: 'black', width: 18, height: 18 }),
  Mobilephone: makeIcon({ type: 'mobilephone', color: 'black', width: 18, height: 18 }),
  ExternalLink: makeIcon({ type: 'externallink', color: 'black', width: 18, height: 18 }),
  Divider: makeIcon({ type: 'divider', color: 'lightgray', width: 5, height: 18 }),
  PreviewPane: makeIcon({ type: 'previewpane', color: 'black', width: 22, height: 18 }),
  PixelatedPalm: makeIcon({
    category: 'special',
    type: 'palm',
    color: 'black',
    width: 21,
    height: 21,
  }),
  NpmLogo: makeIcon({
    category: 'special',
    type: 'npm',
    color: 'colourful',
    width: 28,
    height: 11,
  }),
}

export const SmallerIcons = {
  ExpansionArrowDown: makeIcon({
    category: 'controls/input',
    type: 'down',
    color: 'darkgray',
    width: 11,
    height: 11,
  }),
}

export const Icons = {
  Bin: makeIcon({ type: 'bin', color: 'gray' }),
  BracketedPointer: makeIcon({ type: 'bracketed-pointer', color: 'gray' }),
  Cross: makeIcon({ type: 'cross-medium', color: 'gray' }),
  Cube: makeIcon({ type: 'd', color: 'gray' }),
  Checkmark: makeIcon({ type: 'checkmark', color: 'gray' }),
  DragHandle: makeIcon({ type: 'draghandle', color: 'gray' }),
  Code: makeIcon({ type: 'codymccodeface-larger', color: 'black' }),
  EditPencil: makeIcon({ type: 'editpencil', color: 'gray' }),
  ExpansionArrow: makeIcon({ type: 'expansionarrow-down', color: 'gray' }),
  ExpansionArrowControlled: makeIcon({ type: 'expansionarrow-down', color: 'blue' }),
  ExpansionArrowDown: makeIcon({ type: 'expansionarrow-down', color: 'gray' }),
  ExpansionArrowRight: makeIcon({ type: 'expansionarrow-right', color: 'gray' }),
  ExternalLink: makeIcon({ type: 'externallink', color: 'gray' }),
  ExternalLinkSmaller: makeIcon({ type: 'externallink-smaller', color: 'gray' }),
  EyeStrikethrough: makeIcon({ type: 'eye-strikethrough', color: 'gray' }),
  EyeOpen: makeIcon({ type: 'eyeopen', color: 'gray' }),
  FourDots: makeIcon({ type: 'fourdots', color: 'gray' }),
  Download: makeIcon({ type: 'download', color: 'gray', width: 18, height: 18 }),
  Downloaded: makeIcon({ type: 'downloaded', color: 'gray', width: 18, height: 18 }),
  Gear: makeIcon({ type: 'gear', color: 'black' }),
  LinkClosed: makeIcon({ type: 'link-closed', color: 'gray' }),
  LinkStrikethrough: makeIcon({ type: 'link-strikethrough', color: 'gray' }),
  LockClosed: makeIcon({ type: 'lockclosed', color: 'gray' }),
  LockOpen: makeIcon({ type: 'lockopen', color: 'gray' }),
  Minus: makeIcon({ type: 'minus', color: 'black' }),
  Plus: makeIcon({ type: 'plus', color: 'black' }),
  Play: makeIcon({ type: 'play', color: 'black' }),

  React: makeIcon({ type: 'react', color: 'blue' }),
  Refresh: makeIcon({ type: 'refresh', color: 'black' }),
  SmallCross: makeIcon({ type: 'cross-small', color: 'gray' }),
  Smiangle: makeIcon({ type: 'smiangle', color: 'purple' }),
  WarningTriangle: makeIcon({ type: 'warningtriangle', color: 'gray' }),

  NewTextFile: makeIcon({
    category: 'filetype',
    type: 'other',
    width: 18,
    height: 18,
  }),
  NewFolder: makeIcon({
    category: 'filetype',
    type: 'folder-small',
    width: 18,
    height: 18,
  }),
  NewUIFile: makeIcon({
    category: 'filetype',
    type: 'ui',
    width: 18,
    height: 18,
  }),
  NewUIJSFile: makeIcon({
    category: 'filetype',
    type: 'ui',
    width: 18,
    height: 18,
  }),
  NewImageAsset: makeIcon({
    category: 'filetype',
    type: 'img',
    width: 18,
    height: 18,
  }),
  Component: makeIcon({
    category: 'element',
    type: 'component',
    width: 18,
    height: 18,
  }),
  CircleSmall: makeIcon({ type: 'circle-small', color: 'gray' }),
  CrossSmall: makeIcon({ type: 'cross-small', color: 'gray' }),
  CrossInTranslucentCircle: makeIcon({ type: 'cross-in-translucent-circle', color: 'gray' }),
}

export const FunctionIcons = {
  Add: Icons.Plus,
  Remove: Icons.Minus,
  Delete: Icons.SmallCross,
  Confirm: Icons.Checkmark,
  Close: Icons.SmallCross,
  Drag: Icons.DragHandle,
  Edit: Icons.EditPencil,
  Expand: Icons.ExpansionArrowRight,
  Expanded: Icons.ExpansionArrowDown,
  Refresh: Icons.Refresh,
  Reset: Icons.Refresh,
  RefreshingAnimated: Icons.Refresh,
}

export const MenuIcons = {
  Menu: makeIcon({
    category: 'semantic',
    type: 'hamburgermenu',
    width: 24,
    height: 24,
    color: 'black',
  }),
  Smiangle: makeIcon({
    category: 'semantic',
    type: 'smiangle',
    width: 24,
    height: 24,
    color: 'black',
  }),
  Octocat: makeIcon({
    category: 'semantic',
    type: 'octocat',
    width: 24,
    height: 24,
    color: 'black',
  }),
  TwoGhosts: makeIcon({
    category: 'semantic',
    type: 'twoghosts',
    width: 24,
    height: 24,
    color: 'black',
  }),
  FileSkewed: makeIcon({
    category: 'semantic',
    type: 'file-skewed',
    width: 24,
    height: 24,
    color: 'black',
  }),
  CodeSkewed: makeIcon({
    category: 'semantic',
    type: 'code-skewed',
    width: 24,
    height: 24,
    color: 'black',
  }),
  Pyramid: makeIcon({
    category: 'semantic',
    type: 'pyramid',
    width: 24,
    height: 24,
    color: 'black',
  }),
  Insert: makeIcon({
    category: 'semantic',
    type: 'pluscube',
    width: 24,
    height: 24,
    color: 'black',
  }),
  ExternalLink: makeIcon({
    category: 'semantic',
    type: 'externallink-large',
    width: 24,
    height: 24,
  }),
  Project: makeIcon({
    category: 'semantic',
    type: 'closedcube',
    width: 24,
    height: 24,
    color: 'black',
  }),
  Folder: makeIcon({
    category: 'semantic',
    type: 'openfolder',
    width: 24,
    height: 24,
    color: 'black',
  }),
  Filestack: makeIcon({
    category: 'semantic',
    type: 'filestack',
    width: 24,
    height: 24,
    color: 'black',
  }),
  Settings: makeIcon({
    category: 'semantic',
    type: 'gear',
    width: 24,
    height: 24,
    color: 'black',
  }),
  Columns: makeIcon({
    category: 'semantic',
    type: 'columnmenu',
    width: 24,
    height: 24,
    color: 'black',
  }),
  Play: makeIcon({
    category: 'semantic',
    type: 'playbutton',
    width: 24,
    height: 24,
    color: 'black',
  }),
  Navigator: makeIcon({
    category: 'semantic',
    type: 'navigator',
    width: 16,
    height: 16,
    color: 'black',
  }),
}
