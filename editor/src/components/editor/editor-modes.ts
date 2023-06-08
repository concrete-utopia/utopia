import type {
  ElementPath,
  StaticElementPath,
  Imports,
  ImageFile,
} from '../../core/shared/project-file-types'
import type { JSXElement } from '../../core/shared/element-template'
import type { Size } from '../../core/shared/math-utils'

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

export interface SelectMode {
  type: 'select'
  controlId: string | null
  area: boolean
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

export type Mode = InsertMode | SelectMode | LiveCanvasMode | TextEditMode
export type PersistedMode = SelectMode | LiveCanvasMode

export const EditorModes = {
  insertMode: function (subjects: Array<InsertionSubject>): InsertMode {
    return {
      type: 'insert',
      subjects: subjects,
    }
  },
  selectMode: function (controlId: string | null = null, area: boolean = false): SelectMode {
    return {
      type: 'select',
      controlId: controlId,
      area,
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

export function convertModeToSavedMode(mode: Mode): PersistedMode {
  switch (mode.type) {
    case 'live':
      return EditorModes.liveMode()
    case 'select':
    case 'insert':
    case 'textEdit':
      return EditorModes.selectMode()
  }
}
