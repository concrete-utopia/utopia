import React from 'react'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { JSXElement } from '../../core/shared/element-template'
import { isFeatureEnabled } from '../../utils/feature-switches'
import { CanvasMousePositionRaw } from '../../utils/global-positions'
import { Modifier } from '../../utils/modifiers'
import CanvasActions from '../canvas/canvas-actions'
import {
  boundingArea,
  createHoverInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import { enableInsertModeForJSXElement, switchEditorMode } from './actions/action-creators'
import {
  defaultButtonElement,
  defaultDivElement,
  defaultImgElement,
  defaultSpanElement,
} from './defaults'
import { EditorModes } from './editor-modes'
import { useEditorState, useRefEditorState } from './store/store-hook'

export function useCheckInsertModeForElementType(elementName: string): boolean {
  return useEditorState((store) => {
    const mode = store.editor.mode
    return (
      mode.type === 'insert' &&
      mode.subjects.some(
        (subject) =>
          subject.element.type === 'JSX_ELEMENT' &&
          subject.element.name.baseVariable === elementName,
      )
    )
  }, 'useCheckInsertModeForElementType mode')
}

export function useEnterDrawToInsertForDiv(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultDivElement)
}

export function useEnterTextEditMode(): (event: React.MouseEvent<Element>) => void {
  const dispatch = useEditorState((store) => store.dispatch, 'useEnterTextEditMode dispatch')
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const textInsertCallback = useEnterDrawToInsertForElement(defaultSpanElement)

  return React.useCallback(
    (event: React.MouseEvent<Element>): void => {
      const firstTextEditableView = selectedViewsRef.current.find((v) =>
        MetadataUtils.targetTextEditable(metadataRef.current, v),
      )
      if (!isFeatureEnabled('Text editing')) {
        textInsertCallback(event, { textEdit: false })
      } else if (firstTextEditableView == null) {
        textInsertCallback(event, { textEdit: true })
      } else {
        dispatch([switchEditorMode(EditorModes.textEditMode(firstTextEditableView))])
      }
    },
    [dispatch, selectedViewsRef, metadataRef, textInsertCallback],
  )
}

export function useEnterDrawToInsertForImage(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultImgElement)
}

export function useEnterDrawToInsertForButton(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultButtonElement)
}

function useEnterDrawToInsertForElement(elementFactory: (newUID: string) => JSXElement): (
  event: React.MouseEvent<Element>,
  insertOptions?: {
    textEdit?: boolean
  },
) => void {
  const dispatch = useEditorState((store) => store.dispatch, 'enterDrawToInsertForDiv dispatch')
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  return React.useCallback(
    (
      event: React.MouseEvent<Element>,
      insertOptions: {
        textEdit?: boolean
      } = {},
    ): void => {
      const modifiers = Modifier.modifiersForEvent(event)
      const newUID = generateUidWithExistingComponents(projectContentsRef.current)

      dispatch([
        enableInsertModeForJSXElement(elementFactory(newUID), newUID, {}, null, insertOptions),
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
