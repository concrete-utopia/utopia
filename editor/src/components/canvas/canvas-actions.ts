import { CanvasPoint, CanvasVector } from '../../core/shared/math-utils'
import { CanvasAction, DragState } from './canvas-types'

const CanvasActions = {
  scrollCanvas: function(delta: CanvasVector): CanvasAction {
    return {
      action: 'SCROLL_CANVAS',
      delta: delta,
    }
  },
  createDragState: function(dragState: DragState): CanvasAction {
    return {
      action: 'CREATE_DRAG_STATE',
      dragState: dragState,
    }
  },
  setSelectionControlsVisibility: function(selectionControlsVisible: boolean): CanvasAction {
    return {
      action: 'SET_SELECTION_CONTROLS_VISIBILITY',
      selectionControlsVisible: selectionControlsVisible,
    }
  },
  zoom: function(scale: number, focusPoint: CanvasPoint | null = null): CanvasAction {
    return {
      action: 'ZOOM',
      scale: Math.max(0.03125, Math.min(64, scale)),
      focusPoint: focusPoint,
    }
  },
  zoomUI: function(zoomIn: boolean): CanvasAction {
    return {
      action: 'ZOOMUI',
      zoomIn: zoomIn,
    }
  },
  clearDragState: function(applyChanges: boolean): CanvasAction {
    return {
      action: 'CLEAR_DRAG_STATE',
      applyChanges: applyChanges,
    }
  },
}

export default CanvasActions
