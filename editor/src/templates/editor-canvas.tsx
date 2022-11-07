import update from 'immutability-helper'
import React from 'react'
import { PROBABLY_ELECTRON, requireElectron } from '../common/env-vars'
import { isAspectRatioLockedNew } from '../components/aspect-ratio'
import CanvasActions from '../components/canvas/canvas-actions'
import { CanvasComponentEntry } from '../components/canvas/canvas-component-entry'
import {
  CanvasAction,
  CanvasModel,
  CanvasMouseEvent,
  CanvasPositions,
  ControlOrHigherOrderControl,
  CSSCursor,
  DragState,
  DuplicateNewUID,
  ResizeDragStatePropertyChange,
  SvgFragmentControl,
  updateMoveDragState,
  updateResizeDragState,
} from '../components/canvas/canvas-types'
import {
  anyDragStarted,
  clearDragState,
  createDuplicationNewUIDsFromEditorState,
  createOrUpdateDragState,
  dragExceededThreshold,
  getCanvasOffset,
  getDragStateDrag,
  getDragStatePositions,
  getDragStateStart,
} from '../components/canvas/canvas-utils'
import { NewCanvasControls } from '../components/canvas/controls/new-canvas-controls'
import { setFocus } from '../components/common/actions/index'
import { EditorAction, EditorDispatch } from '../components/editor/action-types'
import * as EditorActions from '../components/editor/actions/action-creators'
import { EditorModes, Mode, isLiveMode } from '../components/editor/editor-modes'
import {
  BaseSnappingThreshold,
  CanvasCursor,
  DerivedState,
  draggingFromFS,
  EditorState,
  editorStateCanvasControls,
  isOpenFileUiJs,
  notDragging,
  UserState,
} from '../components/editor/store/editor-state'
import {
  didWeHandleMouseMoveForThisFrame,
  didWeHandleWheelForThisFrame,
  mouseMoveHandled,
  mouseWheelHandled,
  resetMouseStatus,
} from '../components/mouse-move'
import * as EP from '../core/shared/element-path'
import { MetadataUtils } from '../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../core/shared/element-template'
import { ElementPath } from '../core/shared/project-file-types'
import { getActionsForClipboardItems, parseClipboardData } from '../utils/clipboard'
import Keyboard, { KeyCharacter, KeysPressed } from '../utils/keyboard'
import { emptyModifiers, Modifier } from '../utils/modifiers'
import RU from '../utils/react-utils'
import Utils from '../utils/utils'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  CoordinateMarker,
  offsetPoint,
  Point,
  RawPoint,
  resize,
  Size,
  WindowPoint,
  WindowRectangle,
  zeroCanvasPoint,
} from '../core/shared/math-utils'
import { UtopiaStyles } from '../uuiui'
import {
  CanvasMousePositionRaw,
  CanvasMousePositionRounded,
  updateGlobalPositions,
} from '../utils/global-positions'
import { last, reverse } from '../core/shared/array-utils'
import {
  boundingArea,
  createInteractionViaMouse,
  reparentTargetsToFilter,
  ReparentTargetsToFilter,
  updateInteractionViaDragDelta,
  updateInteractionViaMouse,
} from '../components/canvas/canvas-strategies/interaction-state'
import { MouseButtonsPressed } from '../utils/mouse'
import {
  existingReparentSubjects,
  getReparentTargetUnified,
} from '../components/canvas/canvas-strategies/strategies/reparent-strategy-helpers'
import { getDragTargets } from '../components/canvas/canvas-strategies/strategies/shared-move-strategies-helpers'
import { pickCanvasStateFromEditorState } from '../components/canvas/canvas-strategies/canvas-strategies'
import { BuiltInDependencies } from '../core/es-modules/package-manager/built-in-dependencies-list'
import { generateUidWithExistingComponents } from '../core/model/element-template-utils'
import { createJsxImage, JSXImageOptions } from '../components/images'
import {
  cancelInsertModeActions,
  HandleInteractionSession,
} from '../components/editor/actions/meta-actions'
import { DropHandlers } from './image-drop'
import { isFeatureEnabled } from '../utils/feature-switches'
import { saveAssets } from '../components/editor/server'

const webFrame = PROBABLY_ELECTRON ? requireElectron().webFrame : null

export const NodeConnectorsDivId = 'node-connectors'

let lastPinchZoomedAt = Date.now()

let unhandledWheelDeltaX = 0
let unhandledWheelDeltaY = 0

resetMouseStatus()

function cursorForKeysPressed(
  keysPressed: KeysPressed,
  mouseButtonsPressed: MouseButtonsPressed,
): CSSCursor | null {
  if (keysPressed['z']) {
    return keysPressed['alt'] ? CSSCursor.ZoomOut : CSSCursor.ZoomIn
  }
  if (keysPressed['space']) {
    // Primary button pressed.
    if (mouseButtonsPressed.has(0)) {
      return CSSCursor.Move
    } else {
      return CSSCursor.OpenHand
    }
  }
  return null
}

function cursorForHoveredControl(
  controls: Array<ControlOrHigherOrderControl>,
  mousePosition: CanvasPoint | null,
): CSSCursor | null {
  const hoveredControl =
    mousePosition == null ? null : topMostControlFragmentContainingPoint(controls, mousePosition)
  return hoveredControl == null ? null : hoveredControl.cursor
}

function getDefaultCursorForMode(mode: Mode): CSSCursor {
  switch (mode.type) {
    case 'select':
      return CSSCursor.Select
    case 'insert':
      return CSSCursor.Insert
    case 'live':
      return CSSCursor.BrowserAuto
    default:
      const _exhaustiveCheck: never = mode
      throw `Unable to get default cursor for unsupported mode ${(mode as any).type}`
  }
}

function isDragging(editorState: EditorState): boolean {
  return (
    editorState.canvas.dragState != null &&
    getDragStateStart(editorState.canvas.dragState, editorState.canvas.resizeOptions) != null
  )
}

function roundPointForScale<C extends CoordinateMarker>(point: Point<C>, scale: number): Point<C> {
  return scale <= 1 ? Utils.roundPointTo(point, 0) : Utils.roundPointToNearestHalf(point)
}

function handleCanvasEvent(
  model: CanvasModel,
  event: CanvasMouseEvent,
  isInsideCanvas: boolean,
): Array<EditorAction> {
  if (event.event === 'WHEEL') {
    return []
  }

  let optionalDragStateAction: Array<EditorAction> = []
  if ('interactionSession' in event && event.interactionSession != null) {
    optionalDragStateAction = [
      model.editorState.canvas.interactionSession != null
        ? CanvasActions.updateInteractionSession(event.interactionSession)
        : CanvasActions.createInteractionSession(event.interactionSession),
    ]
  }

  const insertMode = model.mode.type === 'insert'
  if (insertMode) {
    if (
      event.event === 'MOUSE_UP' &&
      model.editorState.canvas.interactionSession?.interactionData.type === 'DRAG'
    ) {
      const boundingAreaActive =
        model.editorState.canvas.interactionSession?.activeControl.type === 'BOUNDING_AREA'

      const shouldApplyChanges: HandleInteractionSession =
        !isInsideCanvas && boundingAreaActive ? 'do-not-apply-changes' : 'apply-changes'

      optionalDragStateAction = cancelInsertModeActions(shouldApplyChanges)
    } else if (event.event === 'MOUSE_DOWN') {
      if (model.editorState.canvas.interactionSession == null) {
        optionalDragStateAction = [
          CanvasActions.createInteractionSession(
            createInteractionViaMouse(event.canvasPositionRounded, event.modifiers, {
              type: 'RESIZE_HANDLE',
              edgePosition: { x: 1, y: 1 },
            }),
          ),
        ]
      } else if (
        model.editorState.canvas.interactionSession.interactionData.type === 'DRAG' ||
        model.editorState.canvas.interactionSession.interactionData.type === 'HOVER'
      ) {
        optionalDragStateAction = [
          CanvasActions.updateInteractionSession(
            updateInteractionViaMouse(
              model.editorState.canvas.interactionSession,
              'DRAG',
              event.canvasPositionRounded,
              event.modifiers,
              {
                type: 'RESIZE_HANDLE',
                edgePosition: { x: 1, y: 1 },
              },
            ),
          ),
        ]
      }
    }
  } else if (!(insertMode && isOpenFileUiJs(model.editorState))) {
    switch (event.event) {
      case 'DRAG':
        if (event.dragState != null) {
          optionalDragStateAction = [CanvasActions.createDragState(event.dragState)]
        }
        break

      case 'MOUSE_UP':
        if (model.dragState != null) {
          optionalDragStateAction = [
            CanvasActions.clearDragState(
              getDragStateDrag(
                model.editorState.canvas.dragState,
                model.editorState.canvas.resizeOptions,
              ) != null,
            ),
          ]
        }
        if (model.editorState.canvas.interactionSession?.interactionData.type === 'DRAG') {
          optionalDragStateAction = [CanvasActions.clearInteractionSession(true)]
        }
        break

      case 'MOVE':
        break
      case 'MOUSE_LEFT_WINDOW':
        break

      // TODO This will prevent strategies that rely on these events
      case 'DRAG_END':
      case 'CLICK':
      case 'DOUBLE_CLICK':
        return []
    }
  }

  let optionalControlIdClearAction: EditorAction[] = []
  if (
    event.event === 'MOUSE_DOWN' &&
    model.mode.type === 'select' &&
    model.mode.controlId != null
  ) {
    optionalControlIdClearAction = [EditorActions.switchEditorMode(EditorModes.selectMode())]
  }

  // Balazs: for the sake of not breaking too much things, I update the mouse position variable here. I removed it from the state for performance reasons
  updateGlobalPositions(event.canvasPositionRaw, event.canvasPositionRounded, event.windowPosition)

  let optionalRedrawControlsAction: EditorAction[] = []
  if (model.mode.type === 'insert') {
    // in insert mode, always trigger a canvas controls rerender by dispatching this
    // so that the insert snap guidelines can update when the mouse moves
    optionalRedrawControlsAction = [EditorActions.redrawOldCanvasControls()]
  }

  return [
    ...optionalControlIdClearAction,
    ...optionalDragStateAction,
    ...optionalRedrawControlsAction,
  ]
}

function on(
  canvas: CanvasModel,
  event: CanvasMouseEvent,
  canvasBounds: WindowRectangle | null,
): Array<EditorAction> {
  let additionalEvents: Array<EditorAction> = []

  if (event.event === 'MOVE' && event.nativeEvent.buttons === 4) {
    return [
      CanvasActions.scrollCanvas(
        canvasPoint({
          x: -event.nativeEvent.movementX / canvas.scale,
          y: -event.nativeEvent.movementY / canvas.scale,
        }),
      ),
    ]
  }

  if (canvas.keysPressed['space']) {
    if (event.event === 'MOVE' && event.nativeEvent.buttons === 1) {
      return [
        CanvasActions.scrollCanvas(
          canvasPoint({
            x: -event.nativeEvent.movementX / canvas.scale,
            y: -event.nativeEvent.movementY / canvas.scale,
          }),
        ),
      ]
    } else {
      return []
    }
  } else if (canvas.keysPressed['z']) {
    if (event.event === 'MOUSE_UP' && !isDragging(canvas.editorState)) {
      let scale: number
      if (canvas.keysPressed['alt']) {
        scale = Utils.decreaseScale(canvas.scale)
      } else {
        scale = Utils.increaseScale(canvas.scale)
      }
      return [CanvasActions.zoom(scale, event.canvasPositionRounded)]
    } else {
      return []
    }
  } else if (event.event === 'WHEEL') {
    if (event.modifiers.ctrl) {
      const timeoutLength = canvas.scale === 1 ? 500 : 250
      if (Date.now() - lastPinchZoomedAt > timeoutLength) {
        lastPinchZoomedAt = Date.now()
        if (event.delta.y > 0) {
          return [
            CanvasActions.zoom(Utils.decreaseScale(canvas.scale), event.canvasPositionRounded),
          ]
        } else {
          return [
            CanvasActions.zoom(Utils.increaseScale(canvas.scale), event.canvasPositionRounded),
          ]
        }
      }
    } else {
      return [CanvasActions.scrollCanvas(event.delta as any as CanvasVector)]
    }
  } else if (
    isDragging(canvas.editorState) &&
    event.event === 'DRAG' &&
    canvasBounds != null &&
    canvas.mode.type === 'select'
  ) {
    const scrollBump = 5 * (1 / canvas.scale)
    let scrollX: number = 0
    let scrollY: number = 0
    const scaledCanvasBounds = Utils.scaleRect(canvasBounds, 1 / canvas.scale)
    const offsetCanvasPosition = Utils.offsetPoint(event.canvasPositionRounded, canvas.canvasOffset)
    if (offsetCanvasPosition.x < 0) {
      scrollX = -scrollBump
    } else if (offsetCanvasPosition.x > scaledCanvasBounds.width) {
      scrollX = scrollBump
    }
    if (offsetCanvasPosition.y < 0) {
      scrollY = -scrollBump
    } else if (offsetCanvasPosition.y > scaledCanvasBounds.height) {
      scrollY = scrollBump
    }
    if (scrollX !== 0 || scrollY !== 0) {
      const scrollDelta = { x: scrollX, y: scrollY } as CanvasVector
      additionalEvents.push(CanvasActions.scrollCanvas(scrollDelta))
    }
  }
  // Handle all other cases via the plugins.
  return additionalEvents
}

let interactionSessionTimerHandle: any = undefined
export function runLocalCanvasAction(
  dispatch: EditorDispatch,
  model: EditorState,
  derivedState: DerivedState,
  builtinDependencies: BuiltInDependencies,
  action: CanvasAction,
): EditorState {
  // TODO BB horrorshow performance
  switch (action.action) {
    case 'SCROLL_CANVAS': {
      const newCanvasOffset = Utils.offsetPoint(
        model.canvas.realCanvasOffset,
        Utils.negate(action.delta),
      )
      return {
        ...model,
        canvas: {
          ...model.canvas,
          realCanvasOffset: newCanvasOffset,
          roundedCanvasOffset: Utils.roundPointTo(newCanvasOffset, 0),
        },
      }
    }
    case 'POSITION_CANVAS':
      return {
        ...model,
        canvas: {
          ...model.canvas,
          realCanvasOffset: action.position,
          roundedCanvasOffset: Utils.roundPointTo(action.position, 0),
        },
      }
    case 'CLEAR_DRAG_STATE':
      return clearDragState(model, derivedState, action.applyChanges)
    case 'CREATE_DRAG_STATE':
      return createOrUpdateDragState(dispatch, model, action)
    case 'SET_SELECTION_CONTROLS_VISIBILITY':
      return update(model, {
        canvas: {
          selectionControlsVisible: { $set: action.selectionControlsVisible },
        },
      })
    case 'ZOOMUI':
      // Side effect here.
      const zoomChange = 1 * (action.zoomIn ? 1 : -1)
      if (webFrame != null) {
        webFrame.setZoomLevel(webFrame.getZoomLevel() + zoomChange)
      }
      return model
    case 'ZOOM': {
      const { focusPoint, scale } = action
      const previousScale = model.canvas.scale
      const newCanvasOffset = getCanvasOffset(
        model.canvas.realCanvasOffset,
        previousScale,
        scale,
        model.jsxMetadata,
        model.selectedViews,
        focusPoint,
        false,
      )

      return {
        ...model,
        canvas: {
          ...model.canvas,
          scale: scale,
          snappingThreshold: BaseSnappingThreshold / scale,
          realCanvasOffset: newCanvasOffset,
          roundedCanvasOffset: Utils.roundPointTo(newCanvasOffset, 0),
        },
      }
    }
    case 'CREATE_INTERACTION_SESSION':
      clearInterval(interactionSessionTimerHandle)
      if (action.interactionSession.interactionData.type === 'DRAG') {
        interactionSessionTimerHandle = setInterval(() => {
          dispatch([CanvasActions.updateDragInteractionData({ globalTime: Date.now() })])
        }, 200)
      }
      const metadata = model.canvas.interactionSession?.latestMetadata ?? model.jsxMetadata
      const allElementProps =
        model.canvas.interactionSession?.latestAllElementProps ?? model.allElementProps

      return {
        ...model,
        canvas: {
          ...model.canvas,
          interactionSession: {
            ...action.interactionSession,
            latestMetadata: metadata,
            latestAllElementProps: allElementProps,
          },
        },
      }
    case 'CLEAR_INTERACTION_SESSION':
      clearInterval(interactionSessionTimerHandle)
      return {
        ...model,
        canvas: {
          ...model.canvas,
          interactionSession: null, // TODO this should be only cleared in dispatch-strategies, and not here
          domWalkerInvalidateCount: model.canvas.domWalkerInvalidateCount + 1,
          controls: editorStateCanvasControls([], [], [], [], null, []),
        },
      }
    case 'UPDATE_INTERACTION_SESSION':
      if (model.canvas.interactionSession == null) {
        return model
      } else {
        return {
          ...model,
          canvas: {
            ...model.canvas,
            interactionSession: {
              ...model.canvas.interactionSession,
              ...action.interactionSessionUpdate,
            },
          },
        }
      }
    case 'UPDATE_DRAG_INTERACTION_DATA':
      if (
        model.canvas.interactionSession == null ||
        model.canvas.interactionSession.interactionData.type !== 'DRAG'
      ) {
        return model
      } else {
        return {
          ...model,
          canvas: {
            ...model.canvas,
            interactionSession: {
              ...model.canvas.interactionSession,
              interactionData: {
                ...model.canvas.interactionSession.interactionData,
                ...action.dragInteractionUpdate,
              },
            },
          },
        }
      }
    case 'SET_USERS_PREFERRED_STRATEGY': {
      if (model.canvas.interactionSession != null) {
        return {
          ...model,
          canvas: {
            ...model.canvas,
            interactionSession: {
              ...model.canvas.interactionSession,
              userPreferredStrategy: action.strategyId,
            },
          },
        }
      } else {
        return model
      }
    }
    default:
      const _exhaustiveCheck: never = action
      return model
  }
}

export interface ControlDependencies {
  dragState: DragState | null
  mode: Mode
  keysPressed: KeysPressed
  mouseButtonsPressed: MouseButtonsPressed
  scale: number
  snappingThreshold: number
  componentMetadata: ElementInstanceMetadataMap
  highlightedviews: Array<ElementPath>
  selectedViews: Array<ElementPath>
  topLevelHiddenInstances: Array<ElementPath>
  editorState: EditorState
  derivedState: DerivedState
}

export interface CollectControlsParams {
  editor: EditorState
  derived: DerivedState
}

export function collectControlsDependencies(
  dependencies: CollectControlsParams,
): ControlDependencies {
  const { editor, derived } = dependencies
  return {
    dragState: editor.canvas.dragState,
    mode: editor.mode,
    keysPressed: editor.keysPressed,
    mouseButtonsPressed: editor.mouseButtonsPressed,
    scale: editor.canvas.scale,
    snappingThreshold: editor.canvas.snappingThreshold,
    componentMetadata: editor.jsxMetadata,
    highlightedviews: editor.highlightedViews,
    selectedViews: editor.selectedViews,
    topLevelHiddenInstances: editor.hiddenInstances,
    editorState: editor,
    derivedState: derived,
  }
}

export function getNewCanvasControlsCursor(canvasCursor: CanvasCursor): CSSCursor | null {
  if (canvasCursor.fixed == null) {
    return Utils.propOr(null, 'cursor', last(canvasCursor.mouseOver)!)
  } else {
    return canvasCursor.fixed.cursor
  }
}

const applyScaleToControl =
  (scale: number) =>
  (control: ControlOrHigherOrderControl): ControlOrHigherOrderControl => {
    switch (control.type) {
      case 'text':
        return { ...control, props: control.scaleFn(control.props, scale) }
      case 'circle':
        return { ...control, props: control.scaleFn(control.props, scale) }
      case 'ellipse':
        return { ...control, props: control.scaleFn(control.props, scale) }
      case 'image':
        return { ...control, props: control.scaleFn(control.props, scale) }
      case 'path':
        return { ...control, props: control.scaleFn(control.props, scale) }
      case 'rect':
        return { ...control, props: control.scaleFn(control.props, scale) }
      case 'svgControl':
      case 'divControl':
        return {
          ...control,
          controls: (control.controls as any).map(applyScaleToControl(scale)), // HACK why do I need an any here
        }
      default:
        const _exhaustiveCheck: never = control
        throw `Invalid control type: ${JSON.stringify(control)}`
    }
  }

function controlFragmentContainingPoint(
  control: ControlOrHigherOrderControl,
  point: CanvasPoint,
): SvgFragmentControl | null {
  function reduction(
    working: SvgFragmentControl | null,
    c: ControlOrHigherOrderControl,
  ): SvgFragmentControl | null {
    if (working != null) {
      return working
    } else {
      return controlFragmentContainingPoint(c, point)
    }
  }

  switch (control.type) {
    case 'svgControl':
      return reverse(control.controls).reduce(reduction, null)
    case 'divControl':
      return reverse(control.controls).reduce(reduction, null)
    case 'image': {
      const containsPoint = Utils.rectContainsPoint(control.props as CanvasRectangle, point)
      return containsPoint ? control : null
    }
    case 'rect': {
      const { x, y, width, height, strokeWidth } = control.props
      const containsPoint = Utils.rectContainsPoint(
        {
          x: x - strokeWidth,
          y: y - strokeWidth,
          width: width + strokeWidth * 2,
          height: height + strokeWidth * 2,
        } as CanvasRectangle,
        point,
      )
      return containsPoint ? control : null
    }
    case 'circle': {
      const { cx, cy, r, strokeWidth } = control.props
      const containsPoint = Utils.circleContainsPoint({ cx: cx, cy: cy, r: r + strokeWidth }, point)
      return containsPoint ? control : null
    }
    case 'ellipse': {
      const { cx, cy, rx, ry, strokeWidth } = control.props
      const containsPoint = Utils.ellipseContainsPoint(
        { cx: cx, cy: cy, rx: rx + strokeWidth, ry: ry + strokeWidth },
        point,
      )
      return containsPoint ? control : null
    }
    case 'path':
    case 'text':
      return null
    default:
      const _exhaustiveCheck: never = control
      throw `Unable to check if point lies in invalid control ${control}`
  }
}

function topMostControlFragmentContainingPoint(
  controls: Array<ControlOrHigherOrderControl>,
  point: CanvasPoint,
): SvgFragmentControl | null {
  return reverse(controls).reduce((working: SvgFragmentControl | null, control) => {
    if (working != null) {
      return working
    } else {
      return controlFragmentContainingPoint(control, point)
    }
  }, null)
}

function createNodeConnectorsDiv(offset: CanvasPoint, scale: number) {
  return RU.create('div', {
    id: NodeConnectorsDivId,
    key: NodeConnectorsDivId,
    style: {
      position: 'absolute',
      left: 0,
      top: 0,
      zoom: scale >= 1 ? `${scale * 100}%` : 1,
      transform:
        (scale < 1 ? `scale(${scale})` : '') + ` translate3d(${offset.x}px, ${offset.y}px, 0)`,
    },
  })
}

interface EditorCanvasProps {
  model: CanvasModel
  editor: EditorState
  userState: UserState
  dispatch: EditorDispatch
}

export class EditorCanvas extends React.Component<EditorCanvasProps> {
  canvasWrapperRef: HTMLElement | null = null
  constructor(props: EditorCanvasProps) {
    super(props)
    this.setupWindowListeners()
  }

  screenToElementCoordinates(screenX: number, screenY: number): RawPoint {
    if (this.canvasWrapperRef != null) {
      const canvasWrapperRect = this.canvasWrapperRef.getBoundingClientRect()
      const canvasWrapperX = canvasWrapperRect.left
      const canvasWrapperY = canvasWrapperRect.top
      return {
        x: screenX - canvasWrapperX,
        y: screenY - canvasWrapperY,
      } as RawPoint
    } else {
      throw new Error('calling screenToElementCoordinates() before being mounted')
    }
  }

  suppressBrowserNavigation = (event: any) => {
    event.preventDefault()
  }

  componentDidMount() {
    if (this.canvasWrapperRef != null) {
      // Due to the introduction of this https://www.chromestatus.com/features/6662647093133312 combined with
      // React's lack of support for event handler options (https://github.com/facebook/react/issues/6436) we
      // have to add this event handler in a rather clunky way to enable us to call preventDefault() on it
      this.canvasWrapperRef.addEventListener('wheel', this.suppressBrowserNavigation, {
        passive: false,
      })
    }
  }

  componentWillUnmount() {
    if (this.canvasWrapperRef != null) {
      this.canvasWrapperRef.removeEventListener('wheel', this.suppressBrowserNavigation)
    }
    this.removeEventListeners()
  }

  getPositionFromCoordinates = (x: number, y: number): CanvasPositions => {
    const inverseOffset = Utils.negate(this.props.model.canvasOffset)
    const inverseScale = 1 / this.props.model.scale
    const canvasDivCoords = this.screenToElementCoordinates(x, y)
    const pagePosition = Utils.scaleVector(canvasDivCoords, inverseScale)
    const canvasPositionRaw = Utils.offsetPoint(pagePosition as any, inverseOffset)
    return {
      windowPosition: { x: x, y: y } as WindowPoint,
      canvasPositionRaw: canvasPositionRaw as CanvasPoint,
      canvasPositionRounded: Utils.roundPointToNearestHalf(canvasPositionRaw) as CanvasPoint,
    }
  }

  getPosition = (event: MouseEvent): CanvasPositions => {
    return this.getPositionFromCoordinates(event.clientX, event.clientY)
  }

  handleEvent(event: CanvasMouseEvent): void {
    if (
      event.event === 'MOVE' &&
      !this.props.editor.canvas.selectionControlsVisible &&
      this.isInsideCanvas(event.nativeEvent)
    ) {
      this.props.dispatch([CanvasActions.setSelectionControlsVisibility(true)], 'canvas')
    }

    if (
      (event.nativeEvent != null &&
        event.nativeEvent.srcElement != null &&
        (isTargetContextMenu(event.nativeEvent.target as any) ||
          isTargetInPopup(event.nativeEvent.srcElement as any, this.props.editor.openPopupId))) ||
      this.props.editor.modal !== null
    ) {
      return // This is a hack implementing a behavior when context menu blocks the UI
    }

    let canvasBounds: WindowRectangle | null
    if (this.canvasWrapperRef == null) {
      canvasBounds = null
    } else {
      const canvasBoundingRect = this.canvasWrapperRef.getBoundingClientRect()
      canvasBounds = {
        x: canvasBoundingRect.left,
        y: canvasBoundingRect.top,
        width: canvasBoundingRect.width,
        height: canvasBoundingRect.height,
      } as WindowRectangle
    }

    let actions: Array<EditorAction> = []
    // Focus the panel as something is happening in/on it.
    if (this.props.model.focusedPanel !== 'canvas' && event.event === 'MOUSE_DOWN') {
      actions.push(setFocus('canvas'))
    }

    actions.push(
      ...handleCanvasEvent(this.props.model, event, this.isInsideCanvas(event.nativeEvent)),
    )
    actions.push(...on(this.props.model, event, canvasBounds))

    const realActions = actions.filter((action) => action.action !== 'TRANSIENT_ACTIONS')
    const transientActions = actions.filter((action) => action.action === 'TRANSIENT_ACTIONS')

    if (realActions.length > 0) {
      this.props.dispatch(realActions, 'canvas')
    }

    if (transientActions.length > 0) {
      this.props.dispatch(transientActions, 'canvas')
    }
  }

  getModeSpecificCursor(): CSSCursor | null {
    if (this.props.editor.mode.type === 'insert') {
      return CSSCursor.Insert
    } else {
      return null
    }
  }

  render() {
    // we round the offset here, so all layers, the canvas, and controls use the same rounded value for positioning
    // instead of letting Chrome do it, because that results in funky artifacts (e.g. while scrolling, the layers don't "jump" pixels at the same time)

    const nodeConnectorsDiv = createNodeConnectorsDiv(
      this.props.model.canvasOffset,
      this.props.model.scale,
    )

    const modeOverrideCursor = this.getModeSpecificCursor()
    const dragStateCursor = null // FIXME: dragState == null ? null : dragState.cursor

    const cursor =
      modeOverrideCursor ??
      dragStateCursor ??
      cursorForKeysPressed(this.props.model.keysPressed, this.props.model.mouseButtonsPressed) ??
      cursorForHoveredControl(this.props.model.controls, CanvasMousePositionRaw) ??
      getNewCanvasControlsCursor(this.props.editor.cursorStack) ??
      getDefaultCursorForMode(this.props.editor.mode)

    const canvasIsLive = isLiveMode(this.props.editor.mode)

    const canvasControls = React.createElement(NewCanvasControls, {
      windowToCanvasPosition: this.getPosition,
      cursor,
    })

    const canvasLiveEditingStyle = canvasIsLive
      ? UtopiaStyles.canvas.live
      : UtopiaStyles.canvas.editing

    return RU.create(
      'div',
      {
        id: 'canvas-root',
        key: 'canvas-root',
        'data-testid': 'canvas-root',
        style: {
          ...canvasLiveEditingStyle,
          transition: 'all .2s linear',
          position: 'relative',
          overflow: 'hidden',
          height: '100%',
        },
        ref: (ref: HTMLElement | null) => {
          this.canvasWrapperRef = ref
        },

        onDragOver: (event) => {
          event.preventDefault()

          if (this.props.editor.canvas.interactionSession != null) {
            this.handleMouseMove(event.nativeEvent)
            return
          }
          const position = this.getPosition(event.nativeEvent)
          const interactionSessionAction = CanvasActions.createInteractionSession(
            createInteractionViaMouse(
              position.canvasPositionRounded,
              emptyModifiers,
              boundingArea(),
            ),
          )

          const newUID = generateUidWithExistingComponents(this.props.editor.projectContents)
          const newElementProps: Pick<JSXImageOptions, 'width' | 'height'> = {
            width: 1,
            height: 1,
          }
          const newElement = createJsxImage(newUID, newElementProps)

          const elementSize: Size = {
            width: newElementProps.width,
            height: newElementProps.height,
          }
          const insertAction = EditorActions.enableInsertModeForJSXElement(
            newElement,
            newUID,
            {},
            elementSize,
          )

          switch (this.props.editor.imageDragSessionState.type) {
            case 'DRAGGING_FROM_SIDEBAR':
              if (this.props.editor.imageDragSessionState.draggedImageProperties != null) {
                this.props.dispatch([
                  insertAction,
                  interactionSessionAction,
                  EditorActions.setFilebrowserDropTarget(null),
                ])
              } else {
                this.props.dispatch([
                  EditorActions.switchEditorMode(EditorModes.insertMode([])),
                  interactionSessionAction,
                  EditorActions.setFilebrowserDropTarget(null),
                ])
              }
              break
            case 'NOT_DRAGGING':
              if (event.dataTransfer.types.includes('Files')) {
                this.props.dispatch([
                  insertAction,
                  EditorActions.setImageDragSessionState(draggingFromFS()),
                  interactionSessionAction,
                  EditorActions.setFilebrowserDropTarget(null),
                ])
              }
              break
            case 'DRAGGING_FROM_FS':
            default:
              break
          }
        },

        onDragLeave: () => {
          this.props.dispatch([
            CanvasActions.clearInteractionSession(false),
            EditorActions.switchEditorMode(EditorModes.selectMode(null)),
          ])
        },

        onDrop: (event: React.DragEvent) => {
          if (
            this.props.editor.imageDragSessionState.type === 'DRAGGING_FROM_SIDEBAR' &&
            this.props.editor.imageDragSessionState.draggedImageProperties != null
          ) {
            const { width, height, src } =
              this.props.editor.imageDragSessionState.draggedImageProperties

            const imageParams: Partial<JSXImageOptions> = { width: width, height: height, src: src }
            const elementSize: Size = { width: width, height: height }
            const uid = generateUidWithExistingComponents(this.props.editor.projectContents)

            const newElement = createJsxImage(uid, imageParams)

            this.props.dispatch([
              EditorActions.enableInsertModeForJSXElement(newElement, uid, {}, elementSize),
              EditorActions.setImageDragSessionState(notDragging()),
              CanvasActions.clearInteractionSession(true),
              EditorActions.switchEditorMode(EditorModes.selectMode()),
            ])
            return
          }
          event.preventDefault()
          event.stopPropagation()

          const mousePosition = this.getPosition(event.nativeEvent)

          void DropHandlers.onDrop(
            event,
            () => {
              this.handleMouseMove(event.nativeEvent)
              this.handleMouseUp(event.nativeEvent)
            },
            {
              saveAssets: saveAssets,
              scale: this.props.model.scale,
              editor: () => this.props.editor,
              mousePosition: mousePosition,
              dispatch: this.props.dispatch,
              loginState: this.props.userState.loginState,
            },
          )

          return
        },

        onWheel: (event) => {
          if (!canvasIsLive || (canvasIsLive && event.metaKey)) {
            let deltaX = event.deltaX
            let deltaY = event.deltaY

            // Horizontally scroll like a Mac if not on one, as it appears that's done beyond the
            // reach of the browser itself.
            if (Math.abs(deltaX) === 0 && Math.abs(deltaY) !== 0 && event.shiftKey) {
              const temp = deltaX
              deltaX = deltaY
              deltaY = temp
            }

            if (didWeHandleWheelForThisFrame || !this.isInsideCanvas(event.nativeEvent)) {
              unhandledWheelDeltaX += deltaX
              unhandledWheelDeltaY += deltaY
              return
            }
            mouseWheelHandled()
            const canvasPositions = this.getPosition(event.nativeEvent)
            const delta = {
              x: (deltaX + unhandledWheelDeltaX) / 2 / this.props.model.scale,
              y: (deltaY + unhandledWheelDeltaY) / 2 / this.props.model.scale,
            } as WindowPoint
            unhandledWheelDeltaX = 0
            unhandledWheelDeltaY = 0
            event.stopPropagation()
            this.handleEvent({
              ...canvasPositions,
              event: 'WHEEL',
              modifiers: Modifier.modifiersForEvent(event),
              delta: delta,
              cursor: null,
              nativeEvent: event as any,
            })
          }
        },
      },
      nodeConnectorsDiv,
      React.createElement(CanvasComponentEntry, {}),
      canvasControls,
    )
  }

  isInsideCanvas(event: MouseEvent | WheelEvent): boolean {
    if (this.canvasWrapperRef == null) {
      return false
    } else {
      const boundingRect = this.canvasWrapperRef.getBoundingClientRect()
      const result =
        event.clientX >= boundingRect.left &&
        event.clientX <= boundingRect.right &&
        event.clientY >= boundingRect.top &&
        event.clientY <= boundingRect.bottom
      return result
    }
  }

  canvasSelected(): boolean {
    return this.canvasWrapperRef != null
  }

  getElementAspectRatioLocked(): boolean {
    return this.props.editor.selectedViews.every((target) => {
      const possibleElement = MetadataUtils.findElementByElementPath(
        this.props.editor.jsxMetadata,
        target,
      )
      const elementProps = this.props.editor.allElementProps[EP.toString(target)]
      if (possibleElement == null || elementProps == null) {
        return false
      } else {
        return isAspectRatioLockedNew(possibleElement, elementProps)
      }
    })
  }

  applyDragStateKeyboardEvent(event: KeyboardEvent, key: KeyCharacter, pressed: boolean): void {
    const dragState = this.props.editor.canvas.dragState
    const dispatch = this.props.dispatch
    function fireDragStateUpdate(updateFn: DragState): void {
      dispatch([CanvasActions.createDragState(updateFn)], 'canvas')
    }
    // TODO insert update functions for canvas interaction session
    if (dragState != null) {
      switch (dragState.type) {
        case 'MOVE_DRAG_STATE':
          switch (key) {
            case 'shift':
              fireDragStateUpdate(
                updateMoveDragState(
                  dragState,
                  undefined,
                  undefined,
                  undefined,
                  pressed,
                  undefined,
                  undefined,
                  undefined,
                  undefined,
                ),
              )
              break
            case 'cmd':
              fireDragStateUpdate(
                updateMoveDragState(
                  dragState,
                  undefined,
                  undefined,
                  !pressed,
                  undefined,
                  undefined,
                  pressed,
                  undefined,
                  undefined,
                ),
              )
              break
            case 'alt':
              let duplicateNewUIDs: Array<DuplicateNewUID> | null = null
              if (pressed) {
                duplicateNewUIDs = createDuplicationNewUIDsFromEditorState(this.props.editor)
              }
              fireDragStateUpdate(
                updateMoveDragState(
                  dragState,
                  undefined,
                  undefined,
                  undefined,
                  undefined,
                  pressed,
                  undefined,
                  duplicateNewUIDs,
                  undefined,
                ),
              )
              break
            default:
              break
          }
          break
        case 'RESIZE_DRAG_STATE': {
          const resizeOptions = this.props.editor.canvas.resizeOptions
          const dragPositions = getDragStatePositions(dragState, resizeOptions)
          const targetProperty =
            resizeOptions.propertyTargetOptions[resizeOptions.propertyTargetSelectedIndex]
          const propertyChange: ResizeDragStatePropertyChange | undefined =
            dragState.properties.find((prop) => {
              return prop.targetProperty === targetProperty
            })
          const keepAspectRatio =
            (key === 'shift' ? pressed : propertyChange?.keepAspectRatio) ||
            this.getElementAspectRatioLocked()
          const centerBasedResize =
            key === 'alt' ? pressed : propertyChange?.centerBasedResize ?? false
          const enableSnapping = key === 'cmd' ? !pressed : propertyChange?.enableSnapping ?? false
          const dragStart = dragPositions?.start ?? CanvasMousePositionRaw
          let exceededThreshold: boolean = dragPositions?.drag != null
          if (!exceededThreshold) {
            exceededThreshold =
              CanvasMousePositionRounded != null &&
              dragStart != null &&
              dragExceededThreshold(CanvasMousePositionRounded, dragStart)
          }

          if (dragStart != null && exceededThreshold) {
            switch (key) {
              case 'shift':
              case 'alt':
              case 'cmd':
                fireDragStateUpdate(
                  updateResizeDragState(
                    dragState,
                    dragStart,
                    dragPositions?.drag ?? null,
                    targetProperty,
                    enableSnapping,
                    centerBasedResize,
                    keepAspectRatio,
                  ),
                )
                break
              default:
                break
            }
            break
          }
          break
        }
        case 'INSERT_DRAG_STATE':
          break
        default:
          const _exhaustiveCheck: never = dragState
          break
      }
    }
  }

  handleKeyDown = (event: KeyboardEvent) => {
    if (!event.repeat) {
      this.applyDragStateKeyboardEvent(event, Keyboard.keyCharacterForCode(event.keyCode), true)
    }
  }
  handleKeyUp = (event: KeyboardEvent) => {
    if (!event.repeat) {
      this.applyDragStateKeyboardEvent(event, Keyboard.keyCharacterForCode(event.keyCode), false)
    }
  }

  handleMouseDown = (event: MouseEvent) => {
    // TODO RJB 2018 Create new events for right mouse button events
    if (this.canvasSelected() && this.isInsideCanvas(event)) {
      const canvasPositions = this.getPosition(event)

      if (this.props.model.focusedPanel !== 'canvas') {
        this.props.dispatch([setFocus('canvas')], 'everyone')
      }

      if (event.button === 0) {
        if (!this.props.model.keysPressed['z'] && !this.props.model.keysPressed['space']) {
          this.handleEvent({
            ...canvasPositions,
            event: 'MOUSE_DOWN',
            modifiers: Modifier.modifiersForEvent(event),
            cursor: null,
            nativeEvent: event,
          })
        }
      }
    }
  }

  handleMouseUp = (event: MouseEvent) => {
    if (this.canvasSelected()) {
      if (document.pointerLockElement != null) {
        document.exitPointerLock()
      }
      const canvasPositions = this.getPosition(event)
      if (isDragging(this.props.editor)) {
        this.handleEvent({
          ...canvasPositions,
          event: 'DRAG_END',
          modifiers: Modifier.modifiersForEvent(event),
          dragState: this.props.model.dragState,
          cursor: null,
          nativeEvent: event,
        })
      }

      this.handleEvent({
        ...canvasPositions,
        event: 'MOUSE_UP',
        modifiers: Modifier.modifiersForEvent(event),
        dragState: this.props.model.dragState,
        cursor: null,
        nativeEvent: event,
      })
    }
  }

  handleMouseMove = (event: MouseEvent) => {
    if (this.canvasSelected()) {
      const canvasPositions = this.getPosition(event)
      if (this.props.model.keysPressed['space'] || event.buttons === 4) {
        this.handleEvent({
          ...canvasPositions,
          event: 'MOVE',
          modifiers: Modifier.modifiersForEvent(event),
          cursor: null,
          nativeEvent: event,
          interactionSession: null,
        })
      } else {
        const dragState = this.props.model.dragState
        const resizeOptions = this.props.editor.canvas.resizeOptions
        const dragPositions = getDragStatePositions(dragState, resizeOptions)
        // Check and mark mouse move handling.
        if (didWeHandleMouseMoveForThisFrame) {
          return
        }
        mouseMoveHandled()
        const dragStarted = anyDragStarted(dragState)
        if (
          this.props.editor.canvas.interactionSession != null &&
          this.props.editor.canvas.interactionSession.interactionData.type === 'HOVER'
        ) {
          this.handleEvent({
            ...canvasPositions,
            event: 'MOVE',
            modifiers: Modifier.modifiersForEvent(event),
            cursor: null,
            nativeEvent: event,
            interactionSession: updateInteractionViaMouse(
              this.props.editor.canvas.interactionSession,
              'HOVER',
              canvasPositions.canvasPositionRounded,
              Modifier.modifiersForEvent(event),
              null,
            ),
          })
        }
        if (
          this.props.editor.canvas.interactionSession != null &&
          this.props.editor.canvas.interactionSession.interactionData.type === 'DRAG'
        ) {
          const dragStart = this.props.editor.canvas.interactionSession.interactionData.dragStart
          const newDrag = roundPointForScale(
            Utils.offsetPoint(canvasPositions.canvasPositionRounded, Utils.negate(dragStart)),
            this.props.model.scale,
          )

          if (document.pointerLockElement != null) {
            this.handleEvent({
              ...canvasPositions,
              event: 'MOVE',
              modifiers: Modifier.modifiersForEvent(event),
              cursor: null,
              nativeEvent: event,
              interactionSession: updateInteractionViaDragDelta(
                this.props.editor.canvas.interactionSession,
                Modifier.modifiersForEvent(event),
                null,
                canvasPoint({ x: event.movementX, y: event.movementY }),
              ),
            })
          } else {
            this.handleEvent({
              ...canvasPositions,
              event: 'MOVE',
              modifiers: Modifier.modifiersForEvent(event),
              cursor: null,
              nativeEvent: event,
              interactionSession: updateInteractionViaMouse(
                this.props.editor.canvas.interactionSession,
                'DRAG',
                newDrag,
                Modifier.modifiersForEvent(event),
                null,
              ),
            })
          }
        } else if (dragState == null || !dragStarted) {
          this.handleEvent({
            ...canvasPositions,
            event: 'MOVE',
            modifiers: Modifier.modifiersForEvent(event),
            cursor: null,
            nativeEvent: event,
            interactionSession: null,
          })
        } else {
          const newDrag = roundPointForScale(
            Utils.offsetPoint(
              canvasPositions.canvasPositionRounded,
              dragPositions == null ? zeroCanvasPoint : Utils.negate(dragPositions.start),
            ),
            this.props.model.scale,
          )
          let exceededThreshold: boolean = dragPositions?.drag != null
          if (!exceededThreshold && dragPositions != null) {
            exceededThreshold = dragExceededThreshold(
              canvasPositions.canvasPositionRounded,
              dragPositions.start,
            )
          }

          let newDragState: DragState | null = null
          switch (dragState.type) {
            case 'MOVE_DRAG_STATE': {
              const enableSnapping = !event.metaKey
              const constrainAxis = event.shiftKey
              const duplicate = event.altKey
              const reparent = event.metaKey
              newDragState = updateMoveDragState(
                dragState,
                exceededThreshold ? newDrag : undefined,
                exceededThreshold ? dragState.drag : undefined,
                enableSnapping,
                constrainAxis,
                duplicate,
                reparent,
                undefined,
                canvasPositions.canvasPositionRounded,
              )
              break
            }
            case 'RESIZE_DRAG_STATE': {
              const start: CanvasPoint = dragPositions?.start ?? canvasPositions.canvasPositionRaw
              const elementAspectRatioLocked = this.getElementAspectRatioLocked()
              const keepAspectRatio = event.shiftKey || elementAspectRatioLocked
              const centerBasedResize = event.altKey
              const enableSnapping = !event.metaKey

              if (!exceededThreshold) {
                exceededThreshold = dragExceededThreshold(
                  canvasPositions.canvasPositionRounded,
                  start,
                )
              }
              const targetProperty =
                resizeOptions.propertyTargetOptions[resizeOptions.propertyTargetSelectedIndex]
              newDragState = updateResizeDragState(
                dragState,
                start,
                exceededThreshold ? newDrag : null,
                targetProperty,
                enableSnapping,
                centerBasedResize,
                keepAspectRatio,
              )
              break
            }
            case 'INSERT_DRAG_STATE':
              break
            default:
              const _exhaustiveCheck: never = dragState
              break
          }
          this.handleEvent({
            ...canvasPositions,
            event: 'DRAG',
            modifiers: Modifier.modifiersForEvent(event),
            cursor: null, // FIXME: newDragState.cursor,
            dragState: newDragState,
            nativeEvent: event,
          })
        }
      }
    }
  }
  handleMouseLeave = (event: MouseEvent) => {
    if (this.canvasSelected()) {
      const canvasPositions = this.getPosition(event)
      this.handleEvent({
        ...canvasPositions,
        event: 'MOUSE_LEFT_WINDOW',
        modifiers: Modifier.modifiersForEvent(event),
        cursor: null,
        nativeEvent: event,
      })
    }
  }
  handleClick = (event: MouseEvent) => {
    if (event.button === 0 && this.canvasSelected() && this.isInsideCanvas(event)) {
      const canvasPositions = this.getPosition(event)

      this.handleEvent({
        ...canvasPositions,
        event: 'CLICK',
        modifiers: Modifier.modifiersForEvent(event),
        cursor: null,
        nativeEvent: event,
      })
    }
  }
  handleDoubleClick = (event: MouseEvent) => {
    if (event.button === 0 && this.canvasSelected() && this.isInsideCanvas(event)) {
      const canvasPositions = this.getPosition(event)

      this.handleEvent({
        ...canvasPositions,
        event: 'DOUBLE_CLICK',
        modifiers: Modifier.modifiersForEvent(event),
        cursor: null,
        nativeEvent: event,
      })
    }
  }
  handlePaste = (event: ClipboardEvent) => {
    const editor = this.props.editor
    const selectedViews = editor.selectedViews

    // Utopia handles all paste events for these panes first
    const paneFocusedWhereUtopiaHandlesPasteEvents =
      this.props.model.focusedPanel === 'canvas' ||
      this.props.model.focusedPanel === 'filebrowser' ||
      this.props.model.focusedPanel === 'navigator'

    // inputs in those panes won't accept paste events unless they are allowed here
    const editingNavigator = this.props.model.editorState.navigator.renamingTarget != null
    if (
      paneFocusedWhereUtopiaHandlesPasteEvents &&
      this.props.model.editorState.canvas.textEditor == null &&
      this.canvasSelected() &&
      !editingNavigator
    ) {
      event.stopPropagation()
      event.preventDefault()
      if (this.props.editor.keysPressed.alt) {
        // I'm leaving the below comment so that weird behaviour this doesn't get forgotten, but paste style is dead anyway
        // on macOS it seems like alt prevents the 'paste' event from being ever fired, so this is dead code here
        // needs testing if it's any help for other platforms
      } else {
        void parseClipboardData(event.clipboardData).then((result) => {
          const actions = getActionsForClipboardItems(
            editor.projectContents,
            editor.canvas.openFile?.filename ?? null,
            result.utopiaData,
            result.files,
            selectedViews,
            editor.pasteTargetsToIgnore,
            editor.jsxMetadata,
            this.props.model.scale,
          )
          this.props.dispatch(actions, 'everyone')
        })
      }
    }
  }

  setupWindowListeners() {
    window.addEventListener('keydown', this.handleKeyDown)
    window.addEventListener('keyup', this.handleKeyUp)
    window.addEventListener('mousedown', this.handleMouseDown)
    window.addEventListener('mouseup', this.handleMouseUp)
    window.addEventListener('mousemove', this.handleMouseMove, { capture: true }) // we use this event in the capture phase because size-box.ts calls stopPropagation() on mouseMove
    window.addEventListener('mouseleave', this.handleMouseLeave)
    window.addEventListener('click', this.handleClick)
    window.addEventListener('dblclick', this.handleDoubleClick)
    ;(window as any).addEventListener('paste', this.handlePaste)
  }

  removeEventListeners() {
    window.removeEventListener('keydown', this.handleKeyDown)
    window.removeEventListener('keyup', this.handleKeyUp)
    window.removeEventListener('mousedown', this.handleMouseDown)
    window.removeEventListener('mouseup', this.handleMouseUp)
    window.removeEventListener('mousemove', this.handleMouseMove, { capture: true })
    window.removeEventListener('mouseleave', this.handleMouseLeave)
    window.removeEventListener('click', this.handleClick)
    window.removeEventListener('dblclick', this.handleDoubleClick)
    ;(window as any).removeEventListener('paste', this.handlePaste)
  }
}

function isTargetContextMenu(target: HTMLElement): boolean {
  const className = target.className ?? ''
  return (
    (typeof className === 'string' && className.includes('react-contexify')) ||
    (target.parentElement != null && isTargetContextMenu(target.parentElement))
  )
}

function isTargetInPopup(target: HTMLElement, popupId: string | null): boolean {
  if (popupId == null) {
    return false
  } else {
    const popupElement = document.getElementById(popupId)
    return popupElement != null && popupElement.contains(target)
  }
}
