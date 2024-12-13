import type {
  ElementPath,
  StaticElementPath,
  Imports,
  ImageFile,
} from '../../core/shared/project-file-types'
import type { JSXElement } from '../../core/shared/element-template'
import type { CanvasPoint, LocalPoint, Size } from '../../core/shared/math-utils'

export const DefaultInsertSize: Size = { width: 100, height: 100 }

export type InsertionSubjectWrapper = 'conditional' | 'fragment'

export interface InsertionSubject {
  uid: string
  element: JSXElement
  defaultSize: Size
  importsToAdd: Imports
  parent: InsertionParent
  textEdit: boolean
  insertionSubjectWrapper: InsertionSubjectWrapper | null
}

export function insertionSubject(
  uid: string,
  element: JSXElement,
  size: Size | null,
  importsToAdd: Imports,
  parent: InsertionParent,
  textEdit: boolean,
  wrapInConditional: InsertionSubjectWrapper | null,
): InsertionSubject {
  return {
    uid: uid,
    element: element,
    defaultSize: size ?? DefaultInsertSize,
    importsToAdd: importsToAdd,
    parent: parent,
    textEdit: textEdit,
    insertionSubjectWrapper: wrapInConditional,
  }
}

export interface ImageInsertionSubject {
  file: ImageFile
  path: string
}

export function imageInsertionSubject(file: ImageFile, path: string): ImageInsertionSubject {
  return {
    file: file,
    path: path,
  }
}

export interface TargetedInsertionParent {
  target: ElementPath
  staticTarget: StaticElementPath
}

export function targetedInsertionParent(
  target: ElementPath,
  staticTarget: StaticElementPath,
): TargetedInsertionParent {
  return {
    target: target,
    staticTarget: staticTarget,
  }
}

export type InsertionParent = null | TargetedInsertionParent

export function insertionParent(
  target: ElementPath | null,
  staticTarget: StaticElementPath | null,
): InsertionParent {
  if (target == null || staticTarget == null) {
    return null
  } else {
    return {
      target: target,
      staticTarget: staticTarget,
    }
  }
}

export interface InsertMode {
  type: 'insert'
  subjects: Array<InsertionSubject>
}

export type IsDragging = 'dragging' | 'not-dragging'

export type CommentId = NewComment | ExistingComment

export interface NewComment {
  type: 'new'
  location: NewCommentLocation
}

export type NewCommentLocation = CanvasCommentLocation | SceneCommentLocation

export interface CanvasCommentLocation {
  type: 'canvas'
  position: CanvasPoint
}

export function canvasCommentLocation(canvasPoint: CanvasPoint): CanvasCommentLocation {
  return {
    type: 'canvas',
    position: canvasPoint,
  }
}

export interface SceneCommentLocation {
  type: 'scene'
  sceneId: string
  offset: LocalPoint
  position: CanvasPoint
}

export function sceneCommentLocation(
  sceneId: string,
  offset: LocalPoint,
  position: CanvasPoint,
): SceneCommentLocation {
  return {
    type: 'scene',
    sceneId: sceneId,
    offset: offset,
    position: position,
  }
}

export function newComment(location: NewCommentLocation): NewComment {
  return {
    type: 'new',
    location: location,
  }
}

export function isNewComment(comment: CommentId): comment is NewComment {
  return comment.type === 'new'
}

export interface ExistingComment {
  type: 'existing'
  threadId: string
}

export function existingComment(threadId: string): ExistingComment {
  return {
    type: 'existing',
    threadId: threadId,
  }
}

export function isExistingComment(comment: CommentId): comment is ExistingComment {
  return comment.type === 'existing'
}

export interface CommentMode {
  type: 'comment'
  comment: CommentId | null
  isDragging: IsDragging
}

export type SelectModeToolbarMode = 'none' | 'pseudo-insert' | 'panels'

export interface SelectMode {
  type: 'select'
  controlId: string | null
  area: boolean
  toolbarMode: SelectModeToolbarMode
}

export interface TextEditMode {
  type: 'textEdit'
  editedText: ElementPath
  cursorPosition: Coordinates | null
  elementState: TextEditableElementState
  selectOnFocus: 'select-all-on-focus' | 'no-text-selection'
}

export type TextEditableElementState = 'existing' | 'new'

export interface Coordinates {
  x: number
  y: number
}

export interface LiveCanvasMode {
  type: 'live'
  controlId: string | null
}

export interface FollowMode {
  type: 'follow'
  playerId: string // the ID of the followed player
  connectionId: number // the connection ID of the followed player
}

export type Mode =
  | InsertMode
  | SelectMode
  | LiveCanvasMode
  | TextEditMode
  | CommentMode
  | FollowMode

export type PersistedMode = SelectMode | LiveCanvasMode

export const EditorModes = {
  insertMode: function (subjects: Array<InsertionSubject>): InsertMode {
    return {
      type: 'insert',
      subjects: subjects,
    }
  },
  selectMode: function (
    controlId: string | null,
    area: boolean,
    toolbarMode: SelectModeToolbarMode,
  ): SelectMode {
    return {
      type: 'select',
      controlId: controlId,
      area,
      toolbarMode: toolbarMode,
    }
  },
  liveMode: function (controlId: string | null = null): LiveCanvasMode {
    return {
      type: 'live',
      controlId: controlId,
    }
  },
  textEditMode: function (
    editedText: ElementPath,
    cursorPosition: Coordinates | null,
    elementState: TextEditableElementState,
    selectOnFocus: 'select-all-on-focus' | 'no-text-selection',
  ): TextEditMode {
    return {
      type: 'textEdit',
      editedText: editedText,
      cursorPosition: cursorPosition,
      elementState: elementState,
      selectOnFocus: selectOnFocus,
    }
  },
  commentMode: function (comment: CommentId | null, isDragging: IsDragging): CommentMode {
    return {
      type: 'comment',
      comment: comment,
      isDragging: isDragging,
    }
  },
  followMode: function (playerId: string, connectionId: number): FollowMode {
    return {
      type: 'follow',
      playerId: playerId,
      connectionId: connectionId,
    }
  },
}

export function isInsertMode(value: Mode): value is InsertMode {
  return value.type === 'insert'
}
export function isSelectMode(value: Mode): value is SelectMode {
  return value.type === 'select'
}
export function isLiveMode(value: Mode): value is LiveCanvasMode {
  return value.type === 'live'
}
export function isTextEditMode(value: Mode): value is TextEditMode {
  return value.type === 'textEdit'
}
export function isSelectModeWithArea(value: Mode): boolean {
  return value.type === 'select' && value.area
}
export function isCommentMode(value: Mode): value is CommentMode {
  return value.type === 'comment'
}
export function isFollowMode(value: Mode): value is FollowMode {
  return value.type === 'follow'
}

export function convertModeToSavedMode(mode: Mode): PersistedMode {
  switch (mode.type) {
    case 'live':
      return EditorModes.liveMode()
    case 'select':
    case 'insert':
    case 'textEdit':
    case 'comment':
    case 'follow':
      return EditorModes.selectMode(null, false, 'none')
  }
}
