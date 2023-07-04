import { ReactElement } from 'react'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { PropertyPath, ElementPath } from '../../core/shared/project-file-types'
import { KeysPressed } from '../../utils/keyboard'
import { Modifiers } from '../../utils/modifiers'
import { keepDeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  CoordinateMarker,
  Rectangle,
  WindowPoint,
} from '../../core/shared/math-utils'
import { EditorPanel } from '../common/actions/index'
import { Mode } from '../editor/editor-modes'
import { EditorState, OriginalCanvasAndLocalFrame } from '../editor/store/editor-state'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { assertNever, xor } from '../../core/shared/utils'
import { LayoutTargetableProp } from '../../core/layout/layout-helpers-new'
import {
  DragInteractionData,
  InteractionSession,
  InteractionSessionWithoutMetadata,
} from './canvas-strategies/interaction-state'
import { CanvasStrategyId } from './canvas-strategies/canvas-strategy-types'
import { MouseButtonsPressed } from '../../utils/mouse'

export const CanvasContainerID = 'canvas-container'

export enum CSSCursor {
  Select = "-webkit-image-set( url( '/editor/cursors/cursor-default.png ') 1x, url( '/editor/cursors/cursor-default@2x.png ') 2x ) 4 4, default",
  PhysicsDefault = "-webkit-image-set( url( '/editor/cursors/cursor-default.png ') 1x, url( '/editor/cursors/cursor-default@2x.png ') 2x ) 4 4, default",
  Crosshair = "-webkit-image-set( url( '/editor/cursors/cursor-insert.png ') 1x, url( '/editor/cursors/cursor-insert@2x.png ') 2x ) 8 8, crosshair",
  Insert = "-webkit-image-set( url( '/editor/cursors/cursor-insert.png ') 1x, url( '/editor/cursors/cursor-insert@2x.png ') 2x ) 8 8, crosshair",
  ResizeNESW = "-webkit-image-set( url( '/editor/cursors/cursor-nesw-resize.png ') 1x, url( '/editor/cursors/cursor-nesw-resize@2x.png ') 2x ) 7 7, nesw-resize",
  ResizeNWSE = "-webkit-image-set( url( '/editor/cursors/cursor-nwse-resize.png ') 1x, url( '/editor/cursors/cursor-nwse-resize@2x.png ') 2x ) 7 7, nwse-resize",
  ResizeNS = "-webkit-image-set( url( '/editor/cursors/cursor-ns-resize.png ') 1x, url( '/editor/cursors/cursor-ns-resize@2x.png ') 2x ) 4 9, ns-resize",
  ResizeEW = "-webkit-image-set( url( '/editor/cursors/cursor-ew-resize.png ') 1x, url( '/editor/cursors/cursor-ew-resize@2x.png ') 2x ) 9 4, ew-resize",
  Move = "-webkit-image-set( url( '/editor/cursors/cursor-moving.png ') 1x, url( '/editor/cursors/cursor-moving@2x.png ') 2x ) 4 4, default",
  ZoomIn = "-webkit-image-set( url( '/editor/cursors/cursor-zoom-in.png ') 1x, url( '/editor/cursors/cursor-zoom-in@2x.png ') 2x ) 8 8, zoom-in",
  ZoomOut = "-webkit-image-set( url( '/editor/cursors/cursor-zoom-out.png ') 1x, url( '/editor/cursors/cursor-zoom-out@2x.png ') 2x ) 8 8, zoom-out",
  VectorDefault = "-webkit-image-set( url( '/editor/cursors/cursor-vector-default.png ') 1x, url( '/editor/cursors/cursor-vector-default@2x.png ') 2x ) 4 4, zoom-out",
  VectorInsert = "-webkit-image-set( url( '/editor/cursors/cursor-vector-default.png ') 1x, url( '/editor/cursors/cursor-vector-default@2x.png ') 2x ) 4 4, zoom-out",
  VectorDelete = "-webkit-image-set( url( '/editor/cursors/cursor-vector-default.png ') 1x, url( '/editor/cursors/cursor-vector-default@2x.png ') 2x ) 4 4, zoom-out",
  VectorMove = "-webkit-image-set( url( '/editor/cursors/cursor-vector-default.png ') 1x, url( '/editor/cursors/cursor-vector-default@2x.png ') 2x ) 4 4, zoom-out",
  VertexDefault = "-webkit-image-set( url( '/editor/cursors/cursor-vertex-default.png ') 1x, url( '/editor/cursors/cursor-vector-default@2x.png ') 2x ) 4 4, zoom-out",
  VertexInsert = "-webkit-image-set( url( '/editor/cursors/cursor-vertex-default.png ') 1x, url( '/editor/cursors/cursor-vertex-default@2x.png ') 2x ) 4 4, zoom-out",
  VertexDelete = "-webkit-image-set( url( '/editor/cursors/cursor-vertex-default.png ') 1x, url( '/editor/cursors/cursor-vertex-default@2x.png ') 2x ) 4 4, zoom-out",
  VertexMove = "-webkit-image-set( url( '/editor/cursors/cursor-vertex-default.png ') 1x, url( '/editor/cursors/cursor-vertex-default@2x.png ') 2x ) 4 4, zoom-out",
  Text = 'text',
  TextInsert = 'text',
  BrowserAuto = 'auto',
  Duplicate = "-webkit-image-set( url( '/editor/cursors/cursor-duplicate.png ') 1x, url( '/editor/cursors/cursor-duplicate@2x.png ') 2x ) 4 4, default",
  OpenHand = "-webkit-image-set( url( '/editor/cursors/cursor-open-hand.png ') 1x, url( '/editor/cursors/cursor-open-hand@2x.png ') 2x ) 4 4, default",
  NotPermitted = "-webkit-image-set( url( '/editor/cursors/cursor-no-reparent.png ') 1x, url( '/editor/cursors/cursor-no-reparent@2x.png ') 2x ) 4 4, default",
  DefaultMagic = "-webkit-image-set( url( '/editor/cursors/cursor-default-magic.png ') 1x, url( '/editor/cursors/cursor-default-magic@2x.png ') 2x ) 4 4, default",
  DuplicateMagic = "-webkit-image-set( url( '/editor/cursors/cursor-duplicate-magic.png ') 1x, url( '/editor/cursors/cursor-duplicate-magic@2x.png ') 2x ) 4 4, default",
  ResizeEWMagic = "-webkit-image-set( url( '/editor/cursors/cursor-ew-resize-magic.png ') 1x, url( '/editor/cursors/cursor-ew-resize-magic@2x.png ') 2x ) 4 4, default",
  MovingMagic = "-webkit-image-set( url( '/editor/cursors/cursor-moving-magic.png ') 1x, url( '/editor/cursors/cursor-moving-magic@2x.png ') 2x ) 4 4, default",
  NESWResizeMagic = "-webkit-image-set( url( '/editor/cursors/cursor-nesw-resize-magic.png ') 1x, url( '/editor/cursors/cursor-nesw-resize-magic@2x.png ') 2x ) 4 4, default",
  NSResizeMagic = "-webkit-image-set( url( '/editor/cursors/cursor-ns-resize-magic.png ') 1x, url( '/editor/cursors/cursor-ns-resize-magic@2x.png ') 2x ) 4 4, default",
  NWSEResizeMagic = "-webkit-image-set( url( '/editor/cursors/cursor-nwse-resize-magic.png ') 1x, url( '/editor/cursors/cursor-nwse-resize-magic@2x.png ') 2x ) 4 4, default",
  PointerMagic = "-webkit-image-set( url( '/editor/cursors/cursor-pointer-magic.png ') 1x, url( '/editor/cursors/cursor-pointer-magic@2x.png ') 2x ) 4 4, default",
  ColResize = 'col-resize',
  RowResize = 'row-resize',
  Radius = "-webkit-image-set( url( '/editor/cursors/cursor-radius.png ') 1x, url( '/editor/cursors/cursor-radius@2x.png ') 2x ) 4 4, default",
  PaddingWest = "-webkit-image-set( url( '/editor/cursors/cursor-padding-west.png ') 1x, url( '/editor/cursors/cursor-padding-west@2x.png ') 2x ) 4 9, ew-resize",
  PaddingEast = "-webkit-image-set( url( '/editor/cursors/cursor-padding-east.png ') 1x, url( '/editor/cursors/cursor-padding-east@2x.png ') 2x ) 4 9, ew-resize",
  PaddingNorth = "-webkit-image-set( url( '/editor/cursors/cursor-padding-north.png ') 1x, url( '/editor/cursors/cursor-padding-north@2x.png ') 2x ) 9 4, ns-resize",
  PaddingSouth = "-webkit-image-set( url( '/editor/cursors/cursor-padding-south.png ') 1x, url( '/editor/cursors/cursor-padding-south@2x.png ') 2x ) 9 4, ns-resize",
  GapNS = "-webkit-image-set( url( '/editor/cursors/cursor-gap-ns.png ') 1x, url( '/editor/cursors/cursor-gap-ns@2x.png ') 2x ) 8 8, ns-resize",
  GapEW = "-webkit-image-set( url( '/editor/cursors/cursor-gap-ew.png ') 1x, url( '/editor/cursors/cursor-gap-ew@2x.png ') 2x ) 8 8, ew-resize",
}

export type VerticalRectangles = {
  type: 'verticalrectangles'
}

export type Fill = VerticalRectangles

export type CircleControlProps = {
  className?: string
  cx: number
  cy: number
  r: number
  strokeWidth: number
}

export type EllipseControlProps = {
  className?: string
  cx: number
  cy: number
  rx: number
  ry: number
  strokeWidth: number
}

export type ImageControlProps = {
  className?: string
  x: number
  y: number
  width: number
  height: number
}

export type PathControlProps = {
  className?: string
  d: string
  strokeWidth: number
  stroke?: string
  strokeLinecap?: string
  transform?: string
  strokeDasharray?: string
}

export type TextControlProps = {
  className?: string
  alignmentBaseline: string
  fill: string
  fontFamily: string
  fontSize: string
  text: string
  textAnchor: string
  transform?: string
  x: number
  y: number
  width?: number
  height?: number
  fontWeight?: number
}

export type RectControlProps = {
  x: number
  y: number
  width: number
  height: number
  strokeWidth: number
  className?: string
  filter?: string
  fill?: string
}

export interface ControlContextMenu {
  name: string
  renderFn?: () => ReactElement<any>
}

export interface ControlComponentProps<C extends HigherOrderControl> {
  control: C
  canvasOffset: CanvasPoint
  index: number
}

export interface SvgFragmentControlBase<T, P> {
  type: T
  className?: string
  controlid: string
  target: ElementPath | null
  cursor: CSSCursor | null
  scaleFn: (props: P, scale: number) => P
  props: P
  fill: Fill | null
  propertyPath: PropertyPath | null
  contextMenu?: ControlContextMenu
  filter?: string
}

export interface ControlBase {
  controlid: string
  followCanvas: boolean | 'x' | 'y'
  offset?: CanvasPoint
}

export interface SvgControl extends ControlBase {
  type: 'svgControl'
  component: React.ComponentClass<ControlComponentProps<SvgControl>>
  controls: Array<SvgFragmentControl>
}

export interface DivControl extends ControlBase {
  type: 'divControl'
  component: React.ComponentClass<ControlComponentProps<DivControl>>
  controls: Array<SvgControl | DivControl>
}

export type CircleControl = SvgFragmentControlBase<'circle', CircleControlProps>
export type EllipseControl = SvgFragmentControlBase<'ellipse', EllipseControlProps>
export type ImageControl = SvgFragmentControlBase<'image', ImageControlProps>
export type PathControl = SvgFragmentControlBase<'path', PathControlProps>
export type TextControl = SvgFragmentControlBase<'text', TextControlProps>
export type RectControl = SvgFragmentControlBase<'rect', RectControlProps>

export type SvgFragmentControl =
  | CircleControl
  | EllipseControl
  | ImageControl
  | PathControl
  | TextControl
  | RectControl

export type HigherOrderControl = SvgControl | DivControl

export type ControlOrHigherOrderControl = SvgFragmentControl | HigherOrderControl

export interface FrameAndTarget<C extends CoordinateMarker> {
  target: ElementPath
  frame: Rectangle<C>
}

export type CanvasFrameAndTarget = FrameAndTarget<CanvasRectangle>

export interface PinFrameChange extends CanvasFrameAndTarget {
  type: 'PIN_FRAME_CHANGE'
  edgePosition: EdgePosition | null
}

export interface PinSizeChange extends CanvasFrameAndTarget {
  type: 'PIN_SIZE_CHANGE'
  edgePosition: EdgePosition | null
}

export interface PinMoveChange {
  type: 'PIN_MOVE_CHANGE'
  target: ElementPath
  delta: CanvasVector
}

export interface FlexMoveChange {
  type: 'FLEX_MOVE'
  target: ElementPath
  newIndex: number
}

export interface FlexResizeChange {
  type: 'FLEX_RESIZE'
  target: ElementPath
  targetProperty: LayoutTargetableProp
  delta: number
}

export interface SingleResizeChange {
  type: 'SINGLE_RESIZE'
  target: ElementPath
  sizeDelta: CanvasVector
  edgePosition: EdgePosition
}

export type PinOrFlexFrameChange =
  | PinFrameChange
  | PinSizeChange
  | PinMoveChange
  | FlexMoveChange
  | FlexResizeChange
  | SingleResizeChange

export function pinFrameChange(
  target: ElementPath,
  frame: CanvasRectangle,
  edgePos: EdgePosition | null = null,
): PinFrameChange {
  return {
    type: 'PIN_FRAME_CHANGE',
    target: target,
    frame: frame,
    edgePosition: edgePos,
  }
}

export function pinSizeChange(
  target: ElementPath,
  frame: CanvasRectangle,
  edgePos: EdgePosition | null = null,
): PinSizeChange {
  return {
    type: 'PIN_SIZE_CHANGE',
    target: target,
    frame: frame,
    edgePosition: edgePos,
  }
}

export function pinMoveChange(target: ElementPath, delta: CanvasVector): PinMoveChange {
  return {
    type: 'PIN_MOVE_CHANGE',
    target: target,
    delta: delta,
  }
}

export function flexMoveChange(target: ElementPath, newIndex: number): FlexMoveChange {
  return {
    type: 'FLEX_MOVE',
    target: target,
    newIndex: newIndex,
  }
}

export function flexResizeChange(
  target: ElementPath,
  targetProperty: LayoutTargetableProp,
  delta: number,
): FlexResizeChange {
  return {
    type: 'FLEX_RESIZE',
    target: target,
    targetProperty: targetProperty,
    delta: delta,
  }
}

export function singleResizeChange(
  target: ElementPath,
  edgePos: EdgePosition,
  sizeDelta: CanvasVector,
): SingleResizeChange {
  return {
    type: 'SINGLE_RESIZE',
    target: target,
    edgePosition: edgePos,
    sizeDelta: sizeDelta,
  }
}

export interface DuplicateNewUID {
  originalPath: ElementPath
  newUID: string
}

export interface CanvasPositions {
  windowPosition: WindowPoint
  canvasPositionRaw: CanvasPoint
  canvasPositionRounded: CanvasPoint
}

export type IMouseEvent = CanvasPositions & {
  modifiers: Modifiers
  cursor: CSSCursor | null
  nativeEvent: MouseEvent
}

type MouseDown = IMouseEvent & {
  event: 'MOUSE_DOWN'
}

type DoubleClick = IMouseEvent & {
  event: 'DOUBLE_CLICK'
}

type Drag = IMouseEvent & {
  event: 'DRAG'
}

type Move = IMouseEvent & {
  event: 'MOVE'
  interactionSession: InteractionSessionWithoutMetadata | null
}

type Click = IMouseEvent & {
  event: 'CLICK'
}

type ContextMenu = IMouseEvent & {
  event: 'CONTEXT_MENU'
  nativeEvent: MouseEvent
}

type MouseUp = IMouseEvent & {
  event: 'MOUSE_UP'
  nativeEvent: MouseEvent
}

type DragEnd = IMouseEvent & {
  event: 'DRAG_END'
}

type MouseLeftWindow = IMouseEvent & {
  event: 'MOUSE_LEFT_WINDOW'
}

type Wheel = IMouseEvent & {
  event: 'WHEEL'
  delta: WindowPoint
}

// Separate these out as they have some common attributes from IMouseEvent.
export type CanvasMouseEvent =
  | MouseDown
  | DoubleClick
  | MouseUp
  | Drag
  | DragEnd
  | Move
  | MouseLeftWindow
  | Click
  | ContextMenu
  | Wheel

type ScrollCanvas = {
  action: 'SCROLL_CANVAS'
  delta: CanvasVector
}

interface PositionCanvas {
  action: 'POSITION_CANVAS'
  position: CanvasVector
}

export interface CreateInteractionSession {
  action: 'CREATE_INTERACTION_SESSION'
  interactionSession: InteractionSessionWithoutMetadata
}

export interface ClearInteractionSession {
  action: 'CLEAR_INTERACTION_SESSION'
  applyChanges: boolean
}

export interface UpdateInteractionSession {
  action: 'UPDATE_INTERACTION_SESSION'
  interactionSessionUpdate: Partial<InteractionSessionWithoutMetadata>
}

export interface UpdateDragInteractionData {
  action: 'UPDATE_DRAG_INTERACTION_DATA'
  dragInteractionUpdate: Partial<DragInteractionData>
}

type SetSelectionControlsVisibility = {
  action: 'SET_SELECTION_CONTROLS_VISIBILITY'
  selectionControlsVisible: boolean
}

type Zoom = {
  action: 'ZOOM'
  scale: number
  focusPoint: CanvasPoint | null
}

type ZoomUI = {
  action: 'ZOOMUI'
  zoomIn: boolean
}

type SetUsersPreferredStrategy = {
  action: 'SET_USERS_PREFERRED_STRATEGY'
  strategyId: CanvasStrategyId
}

export type CanvasAction =
  | ScrollCanvas
  | PositionCanvas
  | CreateInteractionSession
  | ClearInteractionSession
  | UpdateInteractionSession
  | UpdateDragInteractionData
  | Zoom
  | ZoomUI
  | SetSelectionControlsVisibility
  | SetUsersPreferredStrategy

export interface CanvasModel {
  controls: Array<HigherOrderControl>
  keysPressed: KeysPressed
  mouseButtonsPressed: MouseButtonsPressed
  mode: Mode
  scale: number
  highlightedviews: Array<ElementPath>
  selectedViews: Array<ElementPath>
  canvasOffset: CanvasPoint
  focusedPanel: EditorPanel | null
  editorState: EditorState
}

export type EdgePositionPart = 0 | 0.5 | 1

export type EdgePosition = { x: EdgePositionPart; y: EdgePositionPart }

export function edgePosition(x: EdgePositionPart, y: EdgePositionPart): EdgePosition {
  return {
    x: x,
    y: y,
  }
}

export function oppositeEdgePositionPart(part: EdgePositionPart): EdgePositionPart {
  switch (part) {
    case 0:
      return 1
    case 0.5:
      return 0.5
    case 1:
      return 0
    default:
      const _exhaustiveCheck: never = part
      throw new Error(`Unhandled edge position part ${JSON.stringify(part)}`)
  }
}

export function oppositeEdgePosition(edgePos: EdgePosition): EdgePosition {
  return {
    x: oppositeEdgePositionPart(edgePos.x),
    y: oppositeEdgePositionPart(edgePos.y),
  }
}

export type EdgePiece = 'top' | 'bottom' | 'left' | 'right'

export function isHorizontalEdgePiece(edgePiece: EdgePiece): boolean {
  return edgePiece === 'left' || edgePiece === 'right'
}

export function oppositeEdgePiece(edgePiece: EdgePiece): EdgePiece {
  switch (edgePiece) {
    case 'left':
      return 'right'
    case 'right':
      return 'left'
    case 'top':
      return 'bottom'
    case 'bottom':
      return 'top'
    default:
      assertNever(edgePiece)
  }
}

export type EnabledDirection = EdgePosition

export const DirectionAll: EnabledDirection = { x: 1, y: 1 }
export const DirectionHorizontal: EnabledDirection = { x: 1, y: 0 }
export const DirectionVertical: EnabledDirection = { x: 0, y: 1 }

export const EdgePositionTop: EdgePosition = { x: 0.5, y: 0 }
export const EdgePositionLeft: EdgePosition = { x: 0, y: 0.5 }
export const EdgePositionBottom: EdgePosition = { x: 0.5, y: 1 }
export const EdgePositionRight: EdgePosition = { x: 1, y: 0.5 }
export const EdgePositionTopLeft: EdgePosition = { x: 0, y: 0 }
export const EdgePositionBottomLeft: EdgePosition = { x: 0, y: 1 }
export const EdgePositionBottomRight: EdgePosition = { x: 1, y: 1 }
export const EdgePositionTopRight: EdgePosition = { x: 1, y: 0 }

export type SelectionLocked = 'locked' | 'locked-hierarchy' | 'selectable'
