import React from 'react'
import { EditorState, TransientCanvasState } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'

export type CanvasStrategy = (editorState: EditorState) => TransientCanvasState

export interface CanvasInteractionSession {
  selectedStrategy: CanvasStrategy
}

function moveDragAbsolutePositioned(editorState: EditorState): TransientCanvasState {
  // TODO only apply after a certain treshold

  return {
    highlightedViews: [],
    selectedViews: editorState.selectedViews,
    filesState: {},
    toastsToApply: [],
  }
}

export function pickCanvasStrategy(editorState: EditorState): CanvasStrategy | null {
  return moveDragAbsolutePositioned
}

export function applyCanvasStrategy(editorState: EditorState): TransientCanvasState | null {
  const strategy = editorState.canvas.dragSession?.selectedStrategy
  return strategy?.(editorState) ?? null
}
