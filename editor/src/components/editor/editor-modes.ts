import type {
  ElementPath,
  id,
  StaticElementPath,
  Imports,
} from '../../core/shared/project-file-types'
import type { JSXElement, JSXElementName } from '../../core/shared/element-template'
import type { Size } from '../../core/shared/math-utils'

export interface ElementInsertionSubject {
  type: 'Element'
  uid: string
  element: JSXElement
  size: Size | null
  importsToAdd: Imports
  parent: InsertionParent
}

export interface SceneInsertionSubject {
  type: 'Scene'
}

export interface DragAndDropInsertionSubject {
  type: 'DragAndDrop'
  imageAssets: Array<string> | null
}

export function elementInsertionSubject(
  uid: string,
  element: JSXElement,
  size: Size | null,
  importsToAdd: Imports,
  parent: InsertionParent,
): ElementInsertionSubject {
  return {
    type: 'Element',
    uid: uid,
    element: element,
    size: size,
    importsToAdd: importsToAdd,
    parent: parent,
  }
}

export function SceneInsertionSubject(): SceneInsertionSubject {
  return {
    type: 'Scene',
  }
}

export function dragAndDropInsertionSubject(
  imageAssets: Array<string> | null,
): DragAndDropInsertionSubject {
  return {
    type: 'DragAndDrop',
    imageAssets: imageAssets,
  }
}

export type InsertionSubject =
  | ElementInsertionSubject
  | SceneInsertionSubject
  | DragAndDropInsertionSubject

export function insertionSubjectIsJSXElement(
  insertionSubject: InsertionSubject,
): insertionSubject is ElementInsertionSubject {
  return insertionSubject.type === 'Element'
}

export function insertionSubjectIsScene(
  insertionSubject: InsertionSubject,
): insertionSubject is SceneInsertionSubject {
  return insertionSubject.type === 'Scene'
}

export function insertionSubjectIsDragAndDrop(
  insertionSubject: InsertionSubject,
): insertionSubject is DragAndDropInsertionSubject {
  return insertionSubject.type === 'DragAndDrop'
}

type InsertionParent = null | {
  target: ElementPath
  staticTarget: StaticElementPath
}

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

export type InsertMode = {
  type: 'insert'
  subject: InsertionSubject
  insertionStarted: boolean
}

export type SelectMode = {
  type: 'select'
  controlId: string | null
}

export type SelectLiteMode = {
  type: 'select-lite'
}

export type LiveCanvasMode = {
  type: 'live'
  controlId: string | null
}

export type Mode = InsertMode | SelectMode | SelectLiteMode | LiveCanvasMode

export const EditorModes = {
  insertMode: function (insertionStarted: boolean, subject: InsertionSubject): InsertMode {
    return {
      type: 'insert',
      subject: subject,
      insertionStarted: insertionStarted,
    }
  },
  selectMode: function (controlId: string | null = null): SelectMode {
    return {
      type: 'select',
      controlId: controlId,
    }
  },
  selectLiteMode: function (): SelectLiteMode {
    return {
      type: 'select-lite',
    }
  },
  liveMode: function (controlId: string | null = null): LiveCanvasMode {
    return {
      type: 'live',
      controlId: controlId,
    }
  },
}

export function isInsertMode(value: Mode): value is InsertMode {
  return value.type === 'insert'
}
export function isSelectMode(value: Mode): value is SelectMode {
  return value.type === 'select'
}
export function isSelectLiteMode(value: Mode): value is SelectLiteMode {
  return value.type === 'select-lite'
}
export function isLiveMode(value: Mode): value is LiveCanvasMode {
  return value.type === 'live'
}
