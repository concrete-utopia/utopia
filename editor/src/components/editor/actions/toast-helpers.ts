import type { Spec } from 'immutability-helper'
import { uniqBy } from '../../../core/shared/array-utils'
import type { Notice } from '../../common/notice'
import { notice } from '../../common/notice'
import type { EditorState } from '../store/editor-state'

export function uniqToasts(notices: ReadonlyArray<Notice>): ReadonlyArray<Notice> {
  return uniqBy(notices, (l, r) => l.id === r.id)
}

export function removeToastFromState(editorState: EditorState, noticeID: string): EditorState {
  return {
    ...editorState,
    toasts: editorState.toasts.filter((toast) => toast.id !== noticeID),
  }
}

export function addToastToState(editorState: EditorState, noticeToAdd: Notice): EditorState {
  const withOldToastRemoved = removeToastFromState(editorState, noticeToAdd.id)
  return {
    ...withOldToastRemoved,
    toasts: uniqToasts([...withOldToastRemoved.toasts, noticeToAdd]),
  }
}

export function includeToast(toastText: string | null, editorState: EditorState): EditorState {
  return toastText == null ? editorState : addToastToState(editorState, notice(toastText))
}

export function includeToastPatch(
  toastText: string | null,
  editorState: EditorState,
): Spec<EditorState> {
  const updatedEditorState = includeToast(toastText, editorState)
  return {
    toasts: {
      $set: updatedEditorState.toasts,
    },
  }
}
