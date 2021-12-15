import { ReactElement } from 'react'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { PropertyPath, ElementPath } from '../../core/shared/project-file-types'
import { KeyCharacter, KeysPressed } from '../../utils/keyboard'
import { Modifiers } from '../../utils/modifiers'
import { keepDeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  CoordinateMarker,
  Rectangle,
  Size,
  WindowPoint,
} from '../../core/shared/math-utils'
import { EditorPanel } from '../common/actions/index'
import { EditorAction } from '../editor/action-types'
import { Mode } from '../editor/editor-modes'
import {
  EditorState,
  OriginalCanvasAndLocalFrame,
  TransientCanvasState,
} from '../editor/store/editor-state'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { xor } from '../../core/shared/utils'
import {
  LayoutFlexElementNumericProp,
  LayoutFlexElementProp,
  LayoutTargetableProp,
} from '../../core/layout/layout-helpers-new'
import { pickDefaultCanvasStrategy } from './canvas-strategies/canvas-strategies'

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
  frame: Rectangle<C> | null
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
  edgePosition: EdgePosition | null = null,
): PinFrameChange {
  return {
    type: 'PIN_FRAME_CHANGE',
    target: target,
    frame: frame,
    edgePosition: edgePosition,
  }
}

export function pinSizeChange(
  target: ElementPath,
  frame: CanvasRectangle,
  edgePosition: EdgePosition | null = null,
): PinSizeChange {
  return {
    type: 'PIN_SIZE_CHANGE',
    target: target,
    frame: frame,
    edgePosition: edgePosition,
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
  edgePosition: EdgePosition,
  sizeDelta: CanvasVector,
): SingleResizeChange {
  return {
    type: 'SINGLE_RESIZE',
    target: target,
    edgePosition: edgePosition,
    sizeDelta: sizeDelta,
  }
}

export interface InsertDragState {
  type: 'INSERT_DRAG_STATE'
  start: CanvasPoint
  drag: CanvasVector | null
  metadata: ElementInstanceMetadataMap
}

export function insertDragState(
  start: CanvasPoint,
  drag: CanvasVector | null,
  metadata: ElementInstanceMetadataMap,
): InsertDragState {
  return {
    type: 'INSERT_DRAG_STATE',
    start: start,
    drag: drag,
    metadata: metadata,
  }
}

export interface DuplicateNewUID {
  originalPath: ElementPath
  newUID: string
}

export interface DragStatePositions {
  start: CanvasPoint
  drag: CanvasVector | null
}

export interface MoveDragState extends DragStatePositions {
  type: 'MOVE_DRAG_STATE'
  prevDrag: CanvasVector | null
  originalFrames: Array<CanvasFrameAndTarget>
  dragSelectionBoundingBox: CanvasRectangle | null
  enableSnapping: boolean
  constrainDragAxis: boolean
  duplicate: boolean
  reparent: boolean
  duplicateNewUIDs: Array<DuplicateNewUID> | null
  canvasPosition: CanvasPoint
  metadata: ElementInstanceMetadataMap
  draggedElements: ElementPath[]
}

export interface ResizeDragStatePropertyChange extends DragStatePositions {
  enableSnapping: boolean
  centerBasedResize: boolean
  keepAspectRatio: boolean
  targetProperty: LayoutTargetableProp | undefined
}

export interface ResizeDragState {
  type: 'RESIZE_DRAG_STATE'
  originalSize: CanvasRectangle
  originalFrames: Array<OriginalCanvasAndLocalFrame>
  edgePosition: EdgePosition
  enabledDirection: EnabledDirection
  metadata: ElementInstanceMetadataMap
  draggedElements: ElementPath[]
  isMultiSelect: boolean
  properties: Array<ResizeDragStatePropertyChange>
}

type BoundingArea = {
  type: 'BOUNDING_AREA'
}

type ResizeHandle = {
  type: 'RESIZE_HANDLE'
  edgePosition: EdgePosition
}

type CanvasControlType = BoundingArea | ResizeHandle

export interface CanvasStrategy {
  name: string
  updateFn: CanvasStrategyUpdateFn
  fitnessFn: CanvasStrategyFitnessFn
}

export type CanvasStrategyUpdateFn = (
  editorState: EditorState,
  currentSession: SelectModeCanvasSession,
  previousTransientState: TransientCanvasState | null,
) => TransientCanvasState

export type CanvasStrategyFitnessFn = (
  editorState: EditorState,
  currentSession: SelectModeCanvasSession,
  previousTransientState: TransientCanvasState | null,
) => number

export interface SelectModeCanvasSession {
  type: 'SELECT_MODE_CANVAS_SESSION'
  activeStrategy: CanvasStrategyUpdateFn | null
  start: CanvasPoint
  drag: CanvasVector | null
  activeControl: CanvasControlType
  globalTime: number
  lastTimeMouseMoved: number
  translateStrategyData?: {
    dragDeltaMinimumPassed: boolean
  }
}

export function startNewSelectModeCanvasSession(
  start: CanvasPoint,
  activeControl: CanvasControlType,
): SelectModeCanvasSession {
  return {
    type: 'SELECT_MODE_CANVAS_SESSION',
    start: start,
    activeControl: activeControl,
    activeStrategy: null,
    drag: null,
    globalTime: Date.now(),
    lastTimeMouseMoved: Date.now(),
  }
}

export function updateSelectModeCanvasSessionDragVector(
  current: SelectModeCanvasSession,
  drag: CanvasVector | null,
): SelectModeCanvasSession {
  return {
    ...current,
    drag: drag,
    lastTimeMouseMoved: Date.now(),
  }
}

export type DragState = InsertDragState | MoveDragState | ResizeDragState | SelectModeCanvasSession

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
  dragState: DragState | null
}

type Move = IMouseEvent & {
  event: 'MOVE'
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
  dragState: DragState | null
  nativeEvent: MouseEvent
}

type DragEnd = IMouseEvent & {
  event: 'DRAG_END'
  dragState: DragState | null
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

interface ClearDragState {
  action: 'CLEAR_DRAG_STATE'
  applyChanges: boolean
}

export type CreateDragState = {
  action: 'CREATE_DRAG_STATE'
  dragState: DragState
}

export type UpdateDragState = {
  action: 'UPDATE_DRAG_STATE'
  patch: Partial<SelectModeCanvasSession>
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

export type CanvasAction =
  | ScrollCanvas
  | ClearDragState
  | CreateDragState
  | UpdateDragState
  | Zoom
  | ZoomUI
  | SetSelectionControlsVisibility

export type CanvasModel = {
  controls: Array<HigherOrderControl>
  dragState: DragState | null
  keysPressed: KeysPressed
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

export function oppositeEdgePosition(edgePosition: EdgePosition): EdgePosition {
  return {
    x: oppositeEdgePositionPart(edgePosition.x),
    y: oppositeEdgePositionPart(edgePosition.y),
  }
}

export type EnabledDirection = EdgePosition

export const DirectionAll: EnabledDirection = { x: 1, y: 1 }
export const DirectionHorizontal: EnabledDirection = { x: 1, y: 0 }
export const DirectionVertical: EnabledDirection = { x: 0, y: 1 }
