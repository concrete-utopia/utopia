import React from 'react'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import type { JSXElement } from '../../core/shared/element-template'
import { CanvasMousePositionRaw } from '../../utils/global-positions'
import { Modifier } from '../../utils/modifiers'
import CanvasActions from '../canvas/canvas-actions'
import {
  boundingArea,
  createHoverInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import type { WrapInElementWith } from './action-types'
import { enableInsertModeForJSXElement, wrapInElement } from './actions/action-creators'
import {
  defaultButtonElement,
  defaultDivElement,
  defaultImgElement,
  defaultSpanElement,
  defaultTransparentViewElement,
} from './defaults'
import type { InsertionSubject } from './editor-modes'
import { useDispatch } from './store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from './store/store-hook'
import type { InsertMenuItem } from '../canvas/ui/floating-insert-menu'
import { getActionsToApplyChange } from './convert-callbacks'
import { floatingInsertMenuStateInsert } from './store/editor-state'

function shouldSubjectBeWrappedWithConditional(
  subject: InsertionSubject,
  isWrapInConditionalInsertOptionSet: boolean,
): boolean {
  return subject.insertionSubjectWrapper === 'conditional' && isWrapInConditionalInsertOptionSet
}

export function useCheckInsertModeForElementType(
  elementName: string,
  insertOptions?: {
    textEdit?: boolean
    wrapInConditional?: boolean
  },
): boolean {
  return useEditorState(
    Substores.restOfEditor,
    (store) => {
      const mode = store.editor.mode
      const isTextEditInsertOptionSet = insertOptions?.textEdit ?? false
      const isWrapInConditionalInsertOptionSet = insertOptions?.wrapInConditional ?? false
      return (
        mode.type === 'insert' &&
        mode.subjects.some(
          (subject) =>
            subject.element.type === 'JSX_ELEMENT' &&
            subject.element.name.baseVariable === elementName &&
            subject.textEdit === isTextEditInsertOptionSet &&
            shouldSubjectBeWrappedWithConditional(subject, isWrapInConditionalInsertOptionSet),
        )
      )
    },
    'useCheckInsertModeForElementType mode',
  )
}

export function useEnterDrawToInsertForDiv(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultDivElement)
}

export function useEnterTextEditMode(): (event: React.MouseEvent<Element>) => void {
  const textInsertCallbackWithTextEditing = useEnterDrawToInsertForElement(defaultSpanElement)

  return React.useCallback(
    (event: React.MouseEvent<Element>): void => {
      textInsertCallbackWithTextEditing(event, { textEdit: true })
    },
    [textInsertCallbackWithTextEditing],
  )
}

export function useEnterDrawToInsertForImage(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultImgElement)
}

export function useEnterDrawToInsertForButton(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultButtonElement)
}

export function useEnterDrawToInsertForConditional(): (event: React.MouseEvent<Element>) => void {
  const conditionalInsertCallback = useEnterDrawToInsertForElement(defaultDivElement)

  return React.useCallback(
    (event: React.MouseEvent<Element>): void => {
      conditionalInsertCallback(event, { wrapInConditional: true })
    },
    [conditionalInsertCallback],
  )
}

function useEnterDrawToInsertForElement(elementFactory: (newUID: string) => JSXElement): (
  event: React.MouseEvent<Element>,
  insertOptions?: {
    textEdit?: boolean
    wrapInConditional?: boolean
  },
) => void {
  const dispatch = useDispatch()
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  return React.useCallback(
    (
      event: React.MouseEvent<Element>,
      insertOptions: {
        textEdit?: boolean
        wrapInConditional?: boolean
      } = {},
    ): void => {
      const modifiers = Modifier.modifiersForEvent(event)
      const newUID = generateUidWithExistingComponents(projectContentsRef.current)

      dispatch([
        enableInsertModeForJSXElement(elementFactory(newUID), newUID, {}, null, {
          textEdit: insertOptions?.textEdit,
          wrapInContainer: insertOptions.wrapInConditional === true ? 'conditional' : undefined,
        }),
        CanvasActions.createInteractionSession(
          createHoverInteractionViaMouse(
            CanvasMousePositionRaw!,
            modifiers,
            boundingArea(),
            'zero-drag-permitted',
          ),
        ),
      ])
    },
    [dispatch, projectContentsRef, elementFactory],
  )
}

export function useToInsert(): (convertTo: InsertMenuItem | null) => void {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const jsxMetadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  return React.useCallback(
    (convertToMenuItem: InsertMenuItem | null) => {
      if (convertToMenuItem != null) {
        // Pretend floating menu state as it may be forced by the UI.
        const floatingMenuState = floatingInsertMenuStateInsert(null, null)
        const convertTo = convertToMenuItem.value
        const actions = getActionsToApplyChange(
          projectContentsRef.current,
          jsxMetadataRef.current,
          elementPathTreeRef.current,
          selectedViewsRef.current,
          floatingMenuState,
          false,
          false,
          convertTo,
        )
        dispatch(actions, 'everyone')
      }
    },
    [dispatch, elementPathTreeRef, jsxMetadataRef, projectContentsRef, selectedViewsRef],
  )
}
