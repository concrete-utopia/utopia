import update from 'immutability-helper'
import React from 'react'
import { interactionInProgress } from '../components/canvas/canvas-strategies/canvas-strategies'
import { PROBABLY_ELECTRON, requireElectron } from '../common/env-vars'
import { isAspectRatioLockedNew } from '../components/aspect-ratio'
import CanvasActions from '../components/canvas/canvas-actions'
import { CanvasComponentEntry } from '../components/canvas/canvas-component-entry'
import {
  boundingArea,
  createInteractionViaMouse,
  isDragToPan,
  updateInteractionViaDragDelta,
  updateInteractionViaMouse,
} from '../components/canvas/canvas-strategies/interaction-state'
import type {
  CanvasAction,
  CanvasModel,
  CanvasMouseEvent,
  CanvasPositions,
  ControlOrHigherOrderControl,
  SvgFragmentControl,
} from '../components/canvas/canvas-types'
import { CSSCursor } from '../components/canvas/canvas-types'
import { getCanvasOffset } from '../components/canvas/canvas-utils'
import { NewCanvasControls } from '../components/canvas/controls/new-canvas-controls'
import { setFocus } from '../components/common/actions/index'
import type { EditorAction, EditorDispatch } from '../components/editor/action-types'
import * as EditorActions from '../components/editor/actions/action-creators'
import type { HandleInteractionSession } from '../components/editor/actions/meta-actions'
import { cancelInsertModeActions } from '../components/editor/actions/meta-actions'
import type { Mode } from '../components/editor/editor-modes'
import {
  EditorModes,
  isCommentMode,
  isFollowMode,
  isLiveMode,
} from '../components/editor/editor-modes'
import { saveAssets } from '../components/editor/server'
import type {
  CanvasCursor,
  DerivedState,
  EditorState,
  UserState,
} from '../components/editor/store/editor-state'
import {
  BaseSnappingThreshold,
  draggingFromFS,
  editorStateCanvasControls,
  emptyDragToMoveIndicatorFlags,
  isOpenFileUiJs,
  notDragging,
} from '../components/editor/store/editor-state'
import type { JSXImageOptions } from '../components/images'
import { createJsxImage } from '../components/images'
import {
  didWeHandleMouseMoveForThisFrame,
  didWeHandleWheelForThisFrame,
  mouseMoveHandled,
  mouseWheelHandled,
  resetMouseStatus,
} from '../components/mouse-move'
import type { BuiltInDependencies } from '../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../core/model/element-template-utils'
import { last, reverse } from '../core/shared/array-utils'
import * as EP from '../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../core/shared/element-template'
import type {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  CoordinateMarker,
  Point,
  RawPoint,
  Size,
  WindowPoint,
  WindowRectangle,
} from '../core/shared/math-utils'
import {
  canvasPoint,
  roundPointToNearestHalf,
  roundPointToNearestWhole,
  windowRectangle,
  zeroCanvasPoint,
  zeroCanvasRect,
} from '../core/shared/math-utils'
import type { ElementPath } from '../core/shared/project-file-types'
import { getActionsForClipboardItems, Clipboard } from '../utils/clipboard'
import {
  CanvasMousePositionRaw,
  CanvasMousePositionRounded,
  updateGlobalPositions,
} from '../utils/global-positions'
import type { KeysPressed } from '../utils/keyboard'
import Keyboard, { KeyCharacter } from '../utils/keyboard'
import { emptyModifiers, Modifier } from '../utils/modifiers'
import type { MouseButtonsPressed } from '../utils/mouse'
import RU from '../utils/react-utils'
import Utils from '../utils/utils'
import { UtopiaStyles, colorTheme } from '../uuiui'
import { DropHandlers } from './image-drop'
import { EditorCommon } from '../components/editor/editor-component-common'
import { CursorComponent } from '../components/canvas/controls/select-mode/cursor-component'
import { isFeatureEnabled } from '../utils/feature-switches'
import { getCanvasViewportCenter } from './paste-helpers'
import { DataPasteHandler, isPasteHandler } from '../utils/paste-handler'
import { ResizeObserver } from '../components/canvas/dom-walker'

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
  if (keysPressed['z'] && !keysPressed['cmd']) {
    // must omit `cmd` to avoid triggering on undo/redo
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
  if (keysPressed['alt']) {
    return CSSCursor.Duplicate
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
    case 'insert':
      return CSSCursor.Insert
    case 'live':
      return CSSCursor.BrowserAuto
    case 'comment':
      if (isCommentMode(mode)) {
        return CSSCursor.Comment
      }
      return CSSCursor.Select
    case 'select':
    case 'follow':
    case 'textEdit':
      return CSSCursor.Select
    default:
      const _exhaustiveCheck: never = mode
      throw `Unable to get default cursor for unsupported mode ${(mode as any).type}`
  }
}

function handleCanvasEvent(
  model: CanvasModel,
  event: CanvasMouseEvent,
  isInsideCanvas: boolean,
): Array<EditorAction> {
  if (event.event === 'WHEEL') {
    return []
  }

  let optionalInteractionSessionAction: Array<EditorAction> = []
  if ('interactionSession' in event && event.interactionSession != null) {
    optionalInteractionSessionAction = [
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

      optionalInteractionSessionAction = cancelInsertModeActions(shouldApplyChanges)
    } else if (event.event === 'MOUSE_DOWN') {
      if (model.editorState.canvas.interactionSession == null) {
        // This code path should absolutely not be live, because there should always be an
        // existing interaction session whilst in insert mode. However, since the path is
        // technically possible, we throw an error here so that we can quickly discover
        // if we've accidentally re-enabled it
        throw new Error(
          `It shouldn't be possible to be in insert mode without an active interactionSession`,
        )
      } else if (
        model.editorState.canvas.interactionSession.interactionData.type === 'DRAG' ||
        model.editorState.canvas.interactionSession.interactionData.type === 'HOVER'
      ) {
        optionalInteractionSessionAction = [
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
        break

      case 'MOUSE_UP':
        if (model.editorState.canvas.interactionSession?.interactionData.type === 'DRAG') {
          optionalInteractionSessionAction = [CanvasActions.clearInteractionSession(true)]
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
    optionalControlIdClearAction = [
      EditorActions.switchEditorMode(EditorModes.selectMode(null, false, 'none')),
    ]
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
    ...optionalInteractionSessionAction,
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

  if (isDragToPan(canvas.editorState.canvas.interactionSession, canvas.keysPressed['space'])) {
    const canPan =
      !isFollowMode(canvas.mode) && event.event === 'MOVE' && event.nativeEvent.buttons === 1
    if (canPan) {
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
    if (event.event === 'MOUSE_UP') {
      let scale: number
      if (canvas.keysPressed['alt']) {
        scale = Utils.decreaseScale(canvas.scale)
      } else {
        scale = Utils.increaseScale(canvas.scale)
      }
      return !isFollowMode(canvas.editorState.mode)
        ? [CanvasActions.zoom(scale, event.canvasPositionRounded)]
        : []
    } else {
      return []
    }
  } else if (event.event === 'WHEEL') {
    if (!isFollowMode(canvas.editorState.mode)) {
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
    } else {
      return []
    }
  }
  // Handle all other cases via the plugins.
  return additionalEvents
}

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
          roundedCanvasOffset: roundPointToNearestWhole(newCanvasOffset),
        },
      }
    }
    case 'POSITION_CANVAS':
      return {
        ...model,
        canvas: {
          ...model.canvas,
          realCanvasOffset: action.position,
          roundedCanvasOffset: roundPointToNearestWhole(action.position),
        },
      }
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
          roundedCanvasOffset: roundPointToNearestWhole(newCanvasOffset),
        },
      }
    }
    case 'CREATE_INTERACTION_SESSION':
      const metadata = model.canvas.interactionSession?.latestMetadata ?? model.jsxMetadata
      const allElementProps =
        model.canvas.interactionSession?.latestAllElementProps ?? model.allElementProps
      const variablesInScope =
        model.canvas.interactionSession?.latestVariablesInScope ?? model.variablesInScope
      const elementPathTree =
        model.canvas.interactionSession?.latestElementPathTree ?? model.elementPathTree

      return {
        ...model,
        canvas: {
          ...model.canvas,
          interactionSession: {
            ...action.interactionSession,
            latestMetadata: metadata,
            latestAllElementProps: allElementProps,
            latestElementPathTree: elementPathTree,
            latestVariablesInScope: variablesInScope,
          },
        },
      }
    case 'CLEAR_INTERACTION_SESSION':
      const interactionWasInProgress = interactionInProgress(model.canvas.interactionSession)
      return {
        ...model,
        canvas: {
          ...model.canvas,
          interactionSession: null, // TODO this should be only cleared in dispatch-strategies, and not here
          domWalkerInvalidateCount:
            model.canvas.domWalkerInvalidateCount + (interactionWasInProgress ? 1 : 0),
          controls: editorStateCanvasControls(
            [],
            [],
            [],
            [],
            null,
            [],
            emptyDragToMoveIndicatorFlags,
            null,
          ),
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
  derived: DerivedState
  userState: UserState
  builtinDependencies: BuiltInDependencies
  dispatch: EditorDispatch
  updateCanvasSize: (newValueOrUpdater: Size | ((oldValue: Size) => Size)) => void
}

export class EditorCanvas extends React.Component<EditorCanvasProps> {
  canvasWrapperRef: HTMLElement | null = null
  resizeObserver: ResizeObserver | null = null
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

  updateCanvasOffsetViaScroll = (event: React.UIEvent<HTMLElement, UIEvent>): void => {
    if (this.canvasWrapperRef != null) {
      const scrollDelta = {
        x: this.canvasWrapperRef.scrollLeft / this.props.model.scale,
        y: this.canvasWrapperRef.scrollTop / this.props.model.scale,
      } as CanvasVector

      // Reset wrapper scroll first
      this.canvasWrapperRef.scrollTo({ left: 0, top: 0 })

      // Update canvasOffset
      this.props.dispatch([CanvasActions.scrollCanvas(scrollDelta)], 'canvas')
    }
  }

  componentDidMount() {
    if (this.canvasWrapperRef != null) {
      // Due to the introduction of this https://www.chromestatus.com/features/6662647093133312 combined with
      // React's lack of support for event handler options (https://github.com/facebook/react/issues/6436) we
      // have to add this event handler in a rather clunky way to enable us to call preventDefault() on it
      // Balint 2024: I commented this out and it did not seem to break anything. I'm not sure we still need this,
      // and it can cause headaches: the listener callback here runs earlier than any handlers of the React synthetic events,
      // which means all the wheel events will be preventDefaulted in the descendants of EditorCanvas. Which means
      // it is not possible to scroll elements inside the canvas, except if you add an event listener manually with
      // a ref to stop propagation. In that case the event will not reach the listener here.
      this.canvasWrapperRef.addEventListener('wheel', this.suppressBrowserNavigation, {
        passive: false,
      })
      this.resizeObserver = new ResizeObserver((entries: Array<ResizeObserverEntry>) => {
        if (entries.length === 0) {
          return
        } else {
          const size = {
            width: entries[0].contentRect.width,
            height: entries[0].contentRect.height,
          }
          this.props.updateCanvasSize(size)
        }
      })
      this.resizeObserver!.observe(this.canvasWrapperRef)
      this.props.updateCanvasSize(this.canvasWrapperRef.getBoundingClientRect())
    }
  }

  componentWillUnmount() {
    if (this.canvasWrapperRef != null) {
      this.canvasWrapperRef.removeEventListener('wheel', this.suppressBrowserNavigation)
      if (this.resizeObserver != null) {
        this.resizeObserver.unobserve(this.canvasWrapperRef)
      }
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
      canvasPositionRounded: roundPointToNearestHalf(canvasPositionRaw),
    }
  }

  getPosition = (event: MouseEvent): CanvasPositions => {
    return this.getPositionFromCoordinates(event.clientX, event.clientY)
  }

  handleEvent(event: CanvasMouseEvent): Array<EditorAction> {
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
      return [] // This is a hack implementing a behavior when context menu blocks the UI
    }

    let canvasBounds: WindowRectangle | null
    if (this.canvasWrapperRef == null) {
      canvasBounds = null
    } else {
      const canvasBoundingRect = this.canvasWrapperRef.getBoundingClientRect()
      canvasBounds = windowRectangle({
        x: canvasBoundingRect.left,
        y: canvasBoundingRect.top,
        width: canvasBoundingRect.width,
        height: canvasBoundingRect.height,
      })
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

    return [...realActions, ...transientActions]
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

    const cursor =
      modeOverrideCursor ??
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
          backgroundColor: colorTheme.canvasBackground.value,
          transition: 'all .2s linear',
          position: 'relative',
          overflow: 'hidden',
          height: '100%',
        },
        ref: (ref: HTMLElement | null) => {
          this.canvasWrapperRef = ref
        },

        onDragOver: (event) => {
          if (this.props.editor.mode.type === 'live') {
            return
          }

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
              'zero-drag-not-permitted',
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
          if (this.props.editor.mode.type === 'live') {
            return
          }
          this.props.dispatch([
            CanvasActions.clearInteractionSession(false),
            EditorActions.switchEditorMode(EditorModes.selectMode(null, false, 'none')),
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
              EditorActions.switchEditorMode(EditorModes.selectMode(null, false, 'none')),
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
              this.props.dispatch(this.handleMouseUp(event.nativeEvent), 'everyone')
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
            this.props.dispatch(
              this.handleEvent({
                ...canvasPositions,
                event: 'WHEEL',
                modifiers: Modifier.modifiersForEvent(event),
                delta: delta,
                cursor: null,
                nativeEvent: event as any,
              }),
              'canvas',
            )
          }
        },
        onScroll: (event) => {
          this.updateCanvasOffsetViaScroll(event)
        },
      },
      nodeConnectorsDiv,
      React.createElement(CanvasComponentEntry, {}),
      canvasControls,
      React.createElement(CursorComponent, {}),
      <EditorCommon mouseDown={this.handleMouseDown} />,
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

  handleMouseDown = (event: MouseEvent) => {
    // TODO RJB 2018 Create new events for right mouse button events
    let actions: Array<EditorAction> = []
    if (this.canvasSelected() && this.isInsideCanvas(event)) {
      const canvasPositions = this.getPosition(event)

      if (this.props.model.focusedPanel !== 'canvas') {
        actions.push(setFocus('canvas'))
      }

      if (event.button === 0) {
        if (!this.props.model.keysPressed['z'] && !this.props.model.keysPressed['space']) {
          actions.push(
            ...this.handleEvent({
              ...canvasPositions,
              event: 'MOUSE_DOWN',
              modifiers: Modifier.modifiersForEvent(event),
              cursor: null,
              nativeEvent: event,
            }),
          )
        }
      }
    }
    return actions
  }

  handleMouseUp = (event: MouseEvent) => {
    let actions: Array<EditorAction> = []
    if (this.canvasSelected()) {
      if (document.pointerLockElement != null) {
        document.exitPointerLock()
      }
      const canvasPositions = this.getPosition(event)

      actions.push(
        ...this.handleEvent({
          ...canvasPositions,
          event: 'MOUSE_UP',
          modifiers: Modifier.modifiersForEvent(event),
          cursor: null,
          nativeEvent: event,
        }),
      )
    }
    return actions
  }

  handleMouseMove = (event: MouseEvent) => {
    let actions: Array<EditorAction> = []
    if (this.canvasSelected()) {
      const canvasPositions = this.getPosition(event)
      if (
        isDragToPan(
          this.props.editor.canvas.interactionSession,
          this.props.model.keysPressed['space'],
        ) ||
        event.buttons === 4
      ) {
        actions.push(
          ...this.handleEvent({
            ...canvasPositions,
            event: 'MOVE',
            modifiers: Modifier.modifiersForEvent(event),
            cursor: null,
            nativeEvent: event,
            interactionSession: null,
          }),
        )
      } else {
        // Check and mark mouse move handling.
        if (didWeHandleMouseMoveForThisFrame) {
          return
        }
        mouseMoveHandled()
        if (
          this.props.editor.canvas.interactionSession != null &&
          this.props.editor.canvas.interactionSession.interactionData.type === 'HOVER'
        ) {
          actions.push(
            ...this.handleEvent({
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
            }),
          )
        } else if (
          this.props.editor.canvas.interactionSession != null &&
          this.props.editor.canvas.interactionSession.interactionData.type === 'DRAG'
        ) {
          const dragStart = this.props.editor.canvas.interactionSession.interactionData.dragStart
          const newDrag = roundPointToNearestWhole(
            Utils.offsetPoint(canvasPositions.canvasPositionRounded, Utils.negate(dragStart)),
          )

          if (document.pointerLockElement != null) {
            actions.push(
              ...this.handleEvent({
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
              }),
            )
          } else {
            actions.push(
              ...this.handleEvent({
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
              }),
            )
          }
        } else {
          actions.push(
            ...this.handleEvent({
              ...canvasPositions,
              event: 'MOVE',
              modifiers: Modifier.modifiersForEvent(event),
              cursor: null,
              nativeEvent: event,
              interactionSession: null,
            }),
          )
        }
      }
    }
    this.props.dispatch(actions, 'everyone')
  }
  handleMouseLeave = (event: MouseEvent) => {
    if (this.canvasSelected()) {
      const canvasPositions = this.getPosition(event)
      this.props.dispatch(
        this.handleEvent({
          ...canvasPositions,
          event: 'MOUSE_LEFT_WINDOW',
          modifiers: Modifier.modifiersForEvent(event),
          cursor: null,
          nativeEvent: event,
        }),
        'everyone',
      )
    }
  }
  handleClick = (event: MouseEvent) => {
    if (event.button === 0 && this.canvasSelected() && this.isInsideCanvas(event)) {
      const canvasPositions = this.getPosition(event)

      this.props.dispatch(
        this.handleEvent({
          ...canvasPositions,
          event: 'CLICK',
          modifiers: Modifier.modifiersForEvent(event),
          cursor: null,
          nativeEvent: event,
        }),
        'everyone',
      )
    }
  }
  handleDoubleClick = (event: MouseEvent) => {
    if (event.button === 0 && this.canvasSelected() && this.isInsideCanvas(event)) {
      const canvasPositions = this.getPosition(event)

      this.props.dispatch(
        this.handleEvent({
          ...canvasPositions,
          event: 'DOUBLE_CLICK',
          modifiers: Modifier.modifiersForEvent(event),
          cursor: null,
          nativeEvent: event,
        }),
        'everyone',
      )
    }
  }
  handlePaste = (event: ClipboardEvent) => {
    const editor = this.props.editor
    const selectedViews = editor.selectedViews

    if (isPasteHandler(event.target)) {
      // components with data-pastehandler='true' have precedence
      return
    }

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
        const canvasWrapperRect = this.canvasWrapperRef?.getBoundingClientRect() ?? zeroCanvasRect
        const canvasViewportCenter = getCanvasViewportCenter(
          editor.canvas.roundedCanvasOffset,
          editor.canvas.scale,
          canvasWrapperRect,
        )
        void Clipboard.parseClipboardData(event.clipboardData).then((result) => {
          const actions = getActionsForClipboardItems(
            editor,
            canvasViewportCenter,
            result.utopiaData,
            result.files,
            this.props.model.scale,
          )

          if (actions.length > 0) {
            this.props.dispatch(actions, 'everyone')
          }
        })
      }
    }
  }

  handleWindowMouseUp = (event: any) => {
    this.props.dispatch(this.handleMouseUp(event))
  }

  setupWindowListeners() {
    window.addEventListener('mousemove', this.handleMouseMove, { capture: true }) // we use this event in the capture phase because size-box.ts calls stopPropagation() on mouseMove
    window.addEventListener('mouseleave', this.handleMouseLeave)
    window.addEventListener('mouseup', this.handleWindowMouseUp, { capture: true })
    window.addEventListener('click', this.handleClick)
    window.addEventListener('dblclick', this.handleDoubleClick)
    ;(window as any).addEventListener('paste', this.handlePaste)
  }

  removeEventListeners() {
    window.removeEventListener('mousemove', this.handleMouseMove, { capture: true })
    window.removeEventListener('mouseleave', this.handleMouseLeave)
    window.removeEventListener('mouseup', this.handleWindowMouseUp, { capture: true })
    window.removeEventListener('click', this.handleClick)
    window.removeEventListener('dblclick', this.handleDoubleClick)
    ;(window as any).removeEventListener('paste', this.handlePaste)
  }
}

function isTargetContextMenu(target: HTMLElement): boolean {
  const className = target.className ?? ''
  return (
    (typeof className === 'string' && className.includes('contexify')) ||
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
