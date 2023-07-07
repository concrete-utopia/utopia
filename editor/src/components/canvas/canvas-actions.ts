import type { CanvasPoint, CanvasVector } from '../../core/shared/math-utils'
import type { CanvasStrategyId } from './canvas-strategies/canvas-strategy-types'
import type {
  DragInteractionData,
  InteractionSession,
  InteractionSessionWithoutMetadata,
} from './canvas-strategies/interaction-state'
import type { CanvasAction } from './canvas-types'

const CanvasActions = {
  scrollCanvas: function (delta: CanvasVector): CanvasAction {
    return {
      action: 'SCROLL_CANVAS',
      delta: delta,
    }
  },
  positionCanvas: function (position: CanvasVector): CanvasAction {
    return {
      action: 'POSITION_CANVAS',
      position: position,
    }
  },
  createInteractionSession: function (session: InteractionSessionWithoutMetadata): CanvasAction {
    return {
      action: 'CREATE_INTERACTION_SESSION',
      interactionSession: session,
    }
  },
  clearInteractionSession: function (applyChanges: boolean): CanvasAction {
    return {
      action: 'CLEAR_INTERACTION_SESSION',
      applyChanges: applyChanges,
    }
  },
  updateInteractionSession: function (
    interactionSessionUpdate: Partial<InteractionSession>,
  ): CanvasAction {
    return {
      action: 'UPDATE_INTERACTION_SESSION',
      interactionSessionUpdate: interactionSessionUpdate,
    }
  },
  updateDragInteractionData: function (
    dragInteractionUpdate: Partial<DragInteractionData>,
  ): CanvasAction {
    return {
      action: 'UPDATE_DRAG_INTERACTION_DATA',
      dragInteractionUpdate: dragInteractionUpdate,
    }
  },
  setSelectionControlsVisibility: function (selectionControlsVisible: boolean): CanvasAction {
    return {
      action: 'SET_SELECTION_CONTROLS_VISIBILITY',
      selectionControlsVisible: selectionControlsVisible,
    }
  },
  zoom: function (scale: number, focusPoint: CanvasPoint | null = null): CanvasAction {
    return {
      action: 'ZOOM',
      scale: Math.max(0.03125, Math.min(64, scale)),
      focusPoint: focusPoint,
    }
  },
  zoomUI: function (zoomIn: boolean): CanvasAction {
    return {
      action: 'ZOOMUI',
      zoomIn: zoomIn,
    }
  },
  setUsersPreferredStrategy: function (strategyId: CanvasStrategyId): CanvasAction {
    return {
      action: 'SET_USERS_PREFERRED_STRATEGY',
      strategyId: strategyId,
    }
  },
}

export default CanvasActions
