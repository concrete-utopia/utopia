import { ReactElement } from 'react'
import { ComponentMetadata } from '../../core/shared/element-template'
import { PropertyPath, TemplatePath } from '../../core/shared/project-file-types'
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
import { EditorState, OriginalCanvasAndLocalFrame } from '../editor/store/editor-state'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { xor } from '../../core/shared/utils'
import {
  LayoutFlexElementNumericProp,
  LayoutFlexElementProp,
  LayoutTargetableProp,
} from '../../core/layout/layout-helpers-new'
import { FlexAlignment } from 'utopia-api'

export const enum CSSCursor {
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
  target: TemplatePath | null
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
  target: TemplatePath
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
  target: TemplatePath
  delta: CanvasVector
}

export interface MoveTranslateChange {
  type: 'MOVE_TRANSLATE_CHANGE'
  target: TemplatePath
  delta: CanvasVector
}

export interface FlexMoveChange {
  type: 'FLEX_MOVE'
  target: TemplatePath
  newIndex: number
}

export interface FlexAlignChange {
  type: 'FLEX_ALIGN'
  target: TemplatePath
  alignment: FlexAlignment
}

export interface FlexResizeChange {
  type: 'FLEX_RESIZE'
  target: TemplatePath
  targetProperty: LayoutTargetableProp
  delta: number
}

export interface SingleResizeChange {
  type: 'SINGLE_RESIZE'
  target: TemplatePath
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
  | MoveTranslateChange
  | FlexAlignChange

export function pinFrameChange(
  target: TemplatePath,
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
  target: TemplatePath,
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

export function pinMoveChange(target: TemplatePath, delta: CanvasVector): PinMoveChange {
  return {
    type: 'PIN_MOVE_CHANGE',
    target: target,
    delta: delta,
  }
}

export function moveTranslateChange(
  target: TemplatePath,
  delta: CanvasVector,
): MoveTranslateChange {
  return {
    type: 'MOVE_TRANSLATE_CHANGE',
    target: target,
    delta: delta,
  }
}

export function flexMoveChange(target: TemplatePath, newIndex: number): FlexMoveChange {
  return {
    type: 'FLEX_MOVE',
    target: target,
    newIndex: newIndex,
  }
}

export function flexAlignChange(target: TemplatePath, alignment: FlexAlignment): FlexAlignChange {
  return {
    type: 'FLEX_ALIGN',
    target: target,
    alignment: alignment,
  }
}

export function flexResizeChange(
  target: TemplatePath,
  targetProperty: LayoutTargetableProp,
  delta: number,
): FlexResizeChange {
  return {
    type: 'FLEX_RESIZE',
    target: target,
    targetProperty,
    delta,
  }
}

export function singleResizeChange(
  target: TemplatePath,
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
  metadata: Array<ComponentMetadata>
}

export function insertDragState(
  start: CanvasPoint,
  drag: CanvasVector | null,
  metadata: Array<ComponentMetadata>,
): InsertDragState {
  return {
    type: 'INSERT_DRAG_STATE',
    start: start,
    drag: drag,
    metadata: metadata,
  }
}

export interface DuplicateNewUID {
  originalPath: TemplatePath
  newUID: string
}

export interface MoveDragState {
  type: 'MOVE_DRAG_STATE'
  start: CanvasPoint
  drag: CanvasVector | null
  prevDrag: CanvasVector | null
  originalFrames: Array<CanvasFrameAndTarget>
  dragSelectionBoundingBox: CanvasRectangle | null
  enableSnapping: boolean
  constrainDragAxis: boolean
  duplicate: boolean
  reparent: boolean
  reparentMove: boolean
  localReparent: boolean
  duplicateNewUIDs: Array<DuplicateNewUID> | null
  canvasPosition: CanvasPoint
  metadata: Array<ComponentMetadata>
  draggedElements: TemplatePath[]
  translate: boolean
}

export function moveDragState(
  start: CanvasPoint,
  drag: CanvasVector | null,
  prevDrag: CanvasVector | null,
  originalFrames: Array<CanvasFrameAndTarget>,
  dragSelectionBoundingBox: CanvasRectangle | null,
  enableSnapping: boolean,
  constrainDragAxis: boolean,
  duplicate: boolean,
  reparent: boolean,
  reparentMove: boolean,
  localReparent: boolean,
  duplicateNewUIDs: Array<DuplicateNewUID> | null,
  canvasPosition: CanvasPoint,
  metadata: Array<ComponentMetadata>,
  draggedElements: TemplatePath[],
  translate: boolean,
): MoveDragState {
  if (duplicate === true && duplicateNewUIDs == null) {
    throw new Error('duplicateNewUIDs cannot be null when duplicate is true')
  }

  const invertReparenting = isFeatureEnabled('Dragging Reparents By Default')
  const actuallyEnableSnapping = xor(invertReparenting, enableSnapping)
  const actuallyReparent = xor(invertReparenting, reparent)
  return {
    type: 'MOVE_DRAG_STATE',
    start: start,
    drag: drag,
    prevDrag: prevDrag,
    originalFrames: originalFrames,
    dragSelectionBoundingBox: dragSelectionBoundingBox,
    enableSnapping: actuallyEnableSnapping,
    constrainDragAxis: constrainDragAxis,
    duplicate: duplicate,
    reparent: actuallyReparent,
    reparentMove: reparentMove,
    localReparent: localReparent,
    duplicateNewUIDs: duplicateNewUIDs,
    canvasPosition: canvasPosition,
    metadata: metadata,
    draggedElements: draggedElements,
    translate: translate,
  }
}

export function updateMoveDragState(
  current: MoveDragState,
  drag: CanvasVector | null | undefined,
  prevDrag: CanvasVector | null | undefined,
  enableSnapping: boolean | undefined,
  constrainDragAxis: boolean | undefined,
  duplicate: boolean | undefined,
  reparent: boolean | undefined,
  duplicateNewUIDs: Array<DuplicateNewUID> | null | undefined,
  canvasPosition: CanvasPoint | undefined,
): MoveDragState {
  const newEnableSnapping = enableSnapping === undefined ? current.enableSnapping : enableSnapping
  const newReparent = reparent === undefined ? current.reparent : reparent

  const invertReparenting = isFeatureEnabled('Dragging Reparents By Default')
  const actuallyEnableSnapping = xor(invertReparenting, newEnableSnapping)
  const actuallyReparent = xor(invertReparenting, newReparent)

  const updatedState = keepDeepReferenceEqualityIfPossible(current, {
    ...current,
    drag: drag === undefined ? current.drag : drag,
    prevDrag: prevDrag === undefined ? current.prevDrag : prevDrag,
    enableSnapping: actuallyEnableSnapping,
    constrainDragAxis:
      constrainDragAxis === undefined ? current.constrainDragAxis : constrainDragAxis,
    duplicate: duplicate === undefined ? current.duplicate : duplicate,
    reparent: current.reparent,
    duplicateNewUIDs: duplicateNewUIDs === undefined ? current.duplicateNewUIDs : duplicateNewUIDs,
    canvasPosition: canvasPosition === undefined ? current.canvasPosition : canvasPosition,
  })
  if (updatedState.duplicate === true && updatedState.duplicateNewUIDs == null) {
    throw new Error('duplicateNewUIDs cannot be null when duplicate is true')
  }
  return updatedState
}

export interface ResizeDragState {
  type: 'RESIZE_DRAG_STATE'
  start: CanvasPoint
  drag: CanvasVector | null
  enableSnapping: boolean
  centerBasedResize: boolean
  keepAspectRatio: boolean
  originalSize: CanvasRectangle
  originalFrames: Array<OriginalCanvasAndLocalFrame>
  edgePosition: EdgePosition
  enabledDirection: EnabledDirection
  metadata: Array<ComponentMetadata>
  draggedElements: TemplatePath[]
  isMultiSelect: boolean
  targetProperty: LayoutTargetableProp
}

export function resizeDragState(
  start: CanvasPoint,
  drag: CanvasVector | null,
  enableSnapping: boolean,
  centerBasedResize: boolean,
  keepAspectRatio: boolean,
  originalSize: CanvasRectangle,
  originalFrames: Array<OriginalCanvasAndLocalFrame>,
  edgePosition: EdgePosition,
  enabledDirection: EnabledDirection,
  metadata: Array<ComponentMetadata>,
  draggedElements: TemplatePath[],
  isMultiSelect: boolean,
  targetProperty: LayoutTargetableProp,
): ResizeDragState {
  return {
    type: 'RESIZE_DRAG_STATE',
    start: start,
    drag: drag,
    enableSnapping: enableSnapping,
    centerBasedResize: centerBasedResize,
    keepAspectRatio: keepAspectRatio,
    originalSize: originalSize,
    originalFrames: originalFrames,
    edgePosition: edgePosition,
    enabledDirection: enabledDirection,
    metadata: metadata,
    draggedElements: draggedElements,
    isMultiSelect: isMultiSelect,
    targetProperty: targetProperty,
  }
}

export function updateResizeDragState(
  current: ResizeDragState,
  drag: CanvasVector | null | undefined,
  targetProperty: LayoutTargetableProp,
  enableSnapping: boolean | undefined,
  centerBasedResize: boolean | undefined,
  keepAspectRatio: boolean | undefined,
): ResizeDragState {
  return keepDeepReferenceEqualityIfPossible(current, {
    ...current,
    drag: drag === undefined ? current.drag : drag,
    enableSnapping: enableSnapping === undefined ? current.enableSnapping : enableSnapping,
    centerBasedResize:
      centerBasedResize === undefined ? current.centerBasedResize : centerBasedResize,
    keepAspectRatio: keepAspectRatio === undefined ? current.keepAspectRatio : keepAspectRatio,
    targetProperty: targetProperty,
  })
}

export type DragState = InsertDragState | MoveDragState | ResizeDragState

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

type CreateDragState = {
  action: 'CREATE_DRAG_STATE'
  dragState: DragState
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
  | Zoom
  | ZoomUI
  | SetSelectionControlsVisibility

export type CanvasModel = {
  controls: Array<HigherOrderControl>
  dragState: DragState | null
  keysPressed: KeysPressed
  mode: Mode
  scale: number
  highlightedviews: Array<TemplatePath>
  selectedViews: Array<TemplatePath>
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
