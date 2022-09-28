import type {
  ElementPath,
  StaticElementPath,
  Imports,
  ImageFile,
} from '../../core/shared/project-file-types'
import type { JSXElement } from '../../core/shared/element-template'
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
  imageAssets: Array<ImageInsertionSubject>
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

export function sceneInsertionSubject(): SceneInsertionSubject {
  return {
    type: 'Scene',
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

export function dragAndDropInsertionSubject(
  assets: Array<ImageInsertionSubject>,
): DragAndDropInsertionSubject {
  return {
    type: 'DragAndDrop',
    imageAssets: assets,
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
  subject: InsertionSubject
  insertionStarted: boolean
}

export interface SelectMode {
  type: 'select'
  controlId: string | null
}

export interface LiveCanvasMode {
  type: 'live'
  controlId: string | null
}

export type Mode = InsertMode | SelectMode | LiveCanvasMode
export type PersistedMode = SelectMode | LiveCanvasMode

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
export function isLiveMode(value: Mode): value is LiveCanvasMode {
  return value.type === 'live'
}

export function convertModeToSavedMode(mode: Mode): PersistedMode {
  switch (mode.type) {
    case 'live':
      return EditorModes.liveMode()
    case 'select':
    case 'insert':
      return EditorModes.selectMode()
  }
}
