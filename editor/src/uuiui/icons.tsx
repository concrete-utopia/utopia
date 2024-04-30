import React from 'react'
import type { IcnProps } from './icn'
import { Icn } from './icn'
import type { RegularControlType } from 'utopia-api/core'
import { memoize } from '../core/shared/memoize'
import { assertNever } from '../core/shared/utils'

const makeIcon = (
  appliedProps: IcnProps,
): React.FunctionComponent<React.PropsWithChildren<Omit<IcnProps, 'type'>>> =>
  React.memo((props) => <Icn {...appliedProps} {...props} />)

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
    color: 'main',
    width: 18,
    height: 18,
  }),
  MagnifyingGlassPlus: makeIcon({
    type: 'magnifyingglass-plus-larger',
    color: 'main',
    width: 18,
    height: 18,
  }),
  MagnifyingGlassMinus: makeIcon({
    type: 'magnifyingglass-minus-larger',
    color: 'main',
    width: 18,
    height: 18,
  }),
  Code: makeIcon({ type: 'codymccodeface-larger', color: 'main', width: 18, height: 18 }),
  Node: makeIcon({ type: 'nodymcnodeface-larger', color: 'main', width: 18, height: 18 }),
  DesignTool: makeIcon({ type: 'designtool-larger', color: 'main', width: 18, height: 18 }),
  PlayButton: makeIcon({ type: 'playbutton-larger', color: 'main', width: 18, height: 18 }),
  PlusButton: makeIcon({ type: 'plusbutton-larger', color: 'main', width: 18, height: 18 }),
  Hamburgermenu: makeIcon({ type: 'hamburgermenu-larger', color: 'main', width: 18, height: 18 }),
  HamburgermenuRotated: makeIcon({
    type: 'hamburgermenu-rotated-larger',
    color: 'main',
    width: 18,
    height: 18,
  }),
  Canvas: makeIcon({ type: 'canvas-larger', color: 'main', width: 18, height: 18 }),
  Inspector: makeIcon({ type: 'inspector-larger', color: 'main', width: 18, height: 18 }),
  Navigator: makeIcon({ type: 'navigator-larger', color: 'main', width: 18, height: 18 }),
  StopButton: makeIcon({ type: 'stopbutton', color: 'main', width: 18, height: 18 }),
  Refresh: makeIcon({ type: 'refresh-larger', color: 'main', width: 18, height: 18 }),
  Mobilephone: makeIcon({ type: 'mobilephone', color: 'main', width: 18, height: 18 }),
  ExternalLink: makeIcon({ type: 'externallink', color: 'main', width: 18, height: 18 }),
  Divider: makeIcon({ type: 'divider', color: 'subdued', width: 5, height: 18 }),
  PreviewPane: makeIcon({ type: 'previewpane', color: 'main', width: 22, height: 18 }),
  PixelatedPalm: makeIcon({
    category: 'special',
    type: 'palm',
    color: 'main',
    width: 21,
    height: 21,
  }),
  NpmLogo: makeIcon({
    category: 'special',
    type: 'npm',
    color: 'main',
    width: 28,
    height: 11,
  }),
}

export const InspectorSectionIcons = {
  Layout: makeIcon({
    category: 'inspector',
    type: 'layout',
    color: 'main',
    width: 16,
    height: 16,
  }),
  LayoutSystem: makeIcon({
    category: 'inspector',
    type: 'layout-system',
    color: 'main',
    width: 16,
    height: 16,
  }),
  Code: makeIcon({
    category: 'element',
    type: 'lists',
    color: 'main',
    width: 18,
    height: 18,
  }),
  Conditionals: makeIcon({
    category: 'element',
    type: 'conditional',
    color: 'main',
    width: 18,
    height: 18,
  }),
  Layer: makeIcon({
    category: 'inspector',
    type: 'layer',
    color: 'main',
    width: 16,
    height: 16,
  }),
  Background: makeIcon({
    category: 'inspector',
    type: 'background',
    color: 'main',
    width: 16,
    height: 16,
  }),
  Border: makeIcon({
    category: 'inspector',
    type: 'border',
    color: 'main',
    width: 16,
    height: 16,
  }),
  Shadow: makeIcon({
    category: 'inspector',
    type: 'shadow',
    color: 'main',
    width: 16,
    height: 16,
  }),
  Typography: makeIcon({
    category: 'inspector',
    type: 'typography',
    color: 'main',
    width: 16,
    height: 16,
  }),
  Transforms: makeIcon({
    category: 'inspector',
    type: 'transform',
    color: 'main',
    width: 16,
    height: 16,
  }),
  TextShadow: makeIcon({
    category: 'inspector',
    type: 'text-shadow',
    color: 'main',
    width: 16,
    height: 16,
  }),
  Image: makeIcon({
    category: 'inspector',
    type: 'image',
    color: 'main',
    width: 16,
    height: 16,
  }),
  Interactions: makeIcon({
    category: 'inspector',
    type: 'interactions',
    color: 'main',
    width: 16,
    height: 16,
  }),
  SplitFull: makeIcon({
    category: 'inspector',
    type: 'split-full',
    color: 'main',
    width: 13,
    height: 13,
  }),
  SplitHalf: makeIcon({
    category: 'inspector',
    type: 'split-half',
    color: 'main',
    width: 13,
    height: 13,
  }),
  SplitQuarter: makeIcon({
    category: 'inspector',
    type: 'split-quarter',
    color: 'main',
    width: 13,
    height: 13,
  }),
}

export const SmallerIcons = {
  ExpansionArrowDown: makeIcon({
    category: 'controls/input',
    type: 'down',
    color: 'subdued',
    width: 11,
    height: 11,
  }),
}

export const Icons = {
  Bin: makeIcon({ type: 'bin', color: 'main' }),
  BracketedPointer: makeIcon({ type: 'bracketed-pointer', color: 'main' }),
  Cross: makeIcon({ type: 'cross-medium', color: 'main' }),
  Cube: makeIcon({ type: 'd', color: 'main' }),
  Checkmark: makeIcon({ type: 'checkmark', color: 'main' }),
  DragHandle: makeIcon({ type: 'draghandle', color: 'main' }),
  Code: makeIcon({ type: 'codymccodeface-larger', color: 'main' }),
  EditPencil: makeIcon({ type: 'editpencil', color: 'main' }),
  ExpansionArrow: makeIcon({ type: 'expansionarrow-down', color: 'main' }),
  ExpansionArrowControlled: makeIcon({ type: 'expansionarrow-down', color: 'primary' }),
  ExpansionArrowDown: makeIcon({ type: 'expansionarrow-down', color: 'main' }),
  ExpansionArrowRight: makeIcon({ type: 'expansionarrow-right', color: 'main' }),
  ExpansionArrowRightWhite: makeIcon({ type: 'expansionarrow-right', color: 'white' }),
  ExternalLink: makeIcon({ type: 'externallink', color: 'main' }),
  ExternalLinkSmaller: makeIcon({ type: 'externallink-smaller', color: 'main' }),
  EyeStrikethrough: makeIcon({ type: 'eye-strikethrough', color: 'main' }),
  EyeOpen: makeIcon({ type: 'eyeopen', color: 'main' }),
  Flip: makeIcon({ type: 'flip', color: 'main' }),
  FourDots: makeIcon({ type: 'fourdots', color: 'main' }),
  FlexRow: makeIcon({
    type: 'flexDirection-row-regular-nowrap',
    color: 'main',
    category: 'layout/flex',
  }),
  FlexColumn: makeIcon({
    type: 'flexDirection-column-regular-nowrap',
    color: 'main',
    category: 'layout/flex',
  }),
  Downloaded: makeIcon({ type: 'downloaded', color: 'main', width: 18, height: 18 }),
  Gear: makeIcon({ type: 'gear', color: 'main' }),
  GroupClosed: makeIcon({
    type: 'group-closed',
    category: 'element',
    color: 'main',
    width: 18,
    height: 18,
  }),
  FolderClosed: makeIcon({
    type: 'folder-closed',
    category: 'filetype',
    color: 'main',
    width: 12,
    height: 12,
  }),
  Threedots: makeIcon({ type: 'threedots', color: 'main' }),
  LinkClosed: makeIcon({ type: 'link-closed', color: 'main' }),
  LinkStrikethrough: makeIcon({ type: 'link-strikethrough', color: 'main' }),
  LockClosed: makeIcon({ type: 'lockclosed', color: 'main' }),
  LockClosedDot: makeIcon({ type: 'lockcloseddot', color: 'main' }),
  LockOpen: makeIcon({ type: 'lockopen', color: 'main' }),
  Dot: makeIcon({ type: 'dot', color: 'main' }),
  PinFilled: makeIcon({ type: 'pinfilled', color: 'main' }),
  PinLeftFilled: makeIcon({ type: 'pinleftfilled', color: 'main' }),
  PinRightFilled: makeIcon({ type: 'pinrightfilled', color: 'main' }),
  PinOutline: makeIcon({ type: 'pinoutline', color: 'main' }),
  PinLeftOutline: makeIcon({ type: 'pinleftoutline', color: 'main' }),
  PinRightOutline: makeIcon({ type: 'pinrightoutline', color: 'main' }),
  Pipette: makeIcon({ type: 'pipette', color: 'main' }),
  Minus: makeIcon({ type: 'minus', color: 'main' }),
  Plus: makeIcon({ type: 'plus', color: 'main' }),
  Play: makeIcon({ type: 'play', color: 'main' }),
  React: makeIcon({ type: 'react', color: 'primary' }),
  Refresh: makeIcon({ type: 'refresh', color: 'main' }),
  SmallCross: makeIcon({ type: 'cross-small', color: 'main' }),
  Smiangle: makeIcon({ type: 'smiangle', color: 'primary' }),
  WarningTriangle: makeIcon({ type: 'warningtriangle', color: 'main' }),
  DotDotDot: makeIcon({ type: 'dotdotdot', color: 'main' }),
  ConvertObject: makeIcon({ type: 'convertobject', color: 'main' }),
  NewTextFile: makeIcon({
    category: 'filetype',
    type: 'other',
    width: 12,
    height: 12,
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
  ComponentInstance: makeIcon({
    category: 'element',
    type: 'componentinstance',
    width: 18,
    height: 18,
  }),
  CircleSmall: makeIcon({ type: 'circle-small', color: 'secondary' }),
  CrossSmall: makeIcon({ type: 'cross-small', color: 'secondary' }),
  CrossInTranslucentCircle: makeIcon({ type: 'cross-in-translucent-circle', color: 'main' }),
  Download: makeIcon({
    category: 'semantic',
    type: 'download',
    width: 18,
    height: 18,
    color: 'main',
  }),
  Upload: makeIcon({
    category: 'semantic',
    type: 'upload',
    width: 18,
    height: 18,
    color: 'main',
  }),
  Branch: makeIcon({
    category: 'semantic',
    type: 'branch',
    width: 18,
    height: 18,
    color: 'main',
  }),
  GroupDed: makeIcon({
    type: 'group-ded',
    color: 'main',
    category: 'element',
    width: 18,
    height: 18,
  }),
  GroupProblematic: makeIcon({
    type: 'group-problematic',
    color: 'main',
    category: 'element',
    width: 18,
    height: 18,
  }),
  ExclamationMark: makeIcon({
    type: 'exclamationmark',
    color: 'main',
    category: 'semantic',
    width: 3,
    height: 8,
  }),
  StringInputControl: makeIcon({
    category: 'controltype',
    type: 'string-input',
    color: 'main',
    width: 18,
    height: 18,
  }),
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

export const ModalityIcons = {
  MoveAbsolute: makeIcon({
    category: 'modalities',
    type: 'moveabs-large',
    color: 'main',
    width: 18,
    height: 18,
  }),
  Reorder: makeIcon({
    category: 'modalities',
    type: 'reorder-large',
    color: 'main',
    width: 18,
    height: 18,
  }),
  Reparent: makeIcon({
    category: 'modalities',
    type: 'reparent-large',
    color: 'main',
    width: 18,
    height: 18,
  }),
  Replant: makeIcon({
    category: 'modalities',
    type: 'replant-large',
    color: 'main',
    width: 18,
    height: 18,
  }),
  Magic: makeIcon({
    category: 'modalities',
    type: 'magic-large',
    color: 'main',
    width: 18,
    height: 18,
  }),
}

export const MenuIcons = {
  Menu: makeIcon({
    category: 'semantic',
    type: 'hamburgermenu',
    width: 24,
    height: 24,
    color: 'main',
  }),
  Smiangle: makeIcon({
    category: 'semantic',
    type: 'smiangle',
    width: 24,
    height: 24,
    color: 'main',
  }),
  Octocat: makeIcon({
    category: 'semantic',
    type: 'octocat',
    width: 24,
    height: 24,
    color: 'main',
  }),
  TwoGhosts: makeIcon({
    category: 'semantic',
    type: 'twoghosts',
    width: 24,
    height: 24,
    color: 'main',
  }),
  FileSkewed: makeIcon({
    category: 'semantic',
    type: 'file-skewed',
    width: 24,
    height: 24,
    color: 'main',
  }),
  CodeSkewed: makeIcon({
    category: 'semantic',
    type: 'code-skewed',
    width: 24,
    height: 24,
    color: 'main',
  }),
  Pyramid: makeIcon({
    category: 'semantic',
    type: 'pyramid',
    width: 24,
    height: 24,
    color: 'main',
  }),
  Insert: makeIcon({
    category: 'semantic',
    type: 'pluscube',
    width: 24,
    height: 24,
    color: 'main',
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
    color: 'main',
  }),
  Folder: makeIcon({
    category: 'semantic',
    type: 'openfolder',
    width: 24,
    height: 24,
    color: 'main',
  }),
  Filestack: makeIcon({
    category: 'semantic',
    type: 'filestack',
    width: 24,
    height: 24,
    color: 'main',
  }),
  Settings: makeIcon({
    category: 'semantic',
    type: 'gear',
    width: 24,
    height: 24,
    color: 'main',
  }),
  Columns: makeIcon({
    category: 'semantic',
    type: 'columnmenu',
    width: 24,
    height: 24,
    color: 'main',
  }),
  Play: makeIcon({
    category: 'semantic',
    type: 'playbutton',
    width: 24,
    height: 24,
    color: 'main',
  }),
  Navigator: makeIcon({
    category: 'semantic',
    type: 'navigator',
    width: 16,
    height: 16,
    color: 'main',
  }),
}

export const iconForControlType = memoize(
  (controlType: RegularControlType): ReturnType<typeof makeIcon> => {
    switch (controlType) {
      case 'color':
      case 'expression-input':
      case 'expression-popuplist':
      case 'html-input':
      case 'jsx':
      case 'none':
      case 'number-input':
      case 'string-input':
      case 'style-controls':
        return makeIcon({
          category: 'controltype',
          type: controlType,
          width: 18,
          height: 18,
          color: 'on-highlight-main',
        })
      case 'array':
      case 'checkbox':
      case 'euler':
      case 'matrix3':
      case 'matrix4':
      case 'object':
      case 'popuplist':
      case 'radio':
      case 'tuple':
      case 'union':
      case 'vector2':
      case 'vector3':
      case 'vector4':
        // fallback icon for the missing cases
        return makeIcon({
          category: 'controltype',
          type: 'expression-input',
          width: 18,
          height: 18,
          color: 'on-highlight-main',
        })
      default:
        // If we add a new control type, make sure it has an icon!
        assertNever(controlType)
    }
  },
)
