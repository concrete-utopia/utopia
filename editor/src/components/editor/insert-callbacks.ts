import React from 'react'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { JSXElement } from '../../core/shared/element-template'
import { CanvasMousePositionRaw } from '../../utils/global-positions'
import { Modifier } from '../../utils/modifiers'
import CanvasActions from '../canvas/canvas-actions'
import {
  boundingArea,
  createHoverInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import { enableInsertModeForJSXElement } from './actions/action-creators'
import {
  defaultButtonElement,
  defaultDivElement,
  defaultImgElement,
  defaultSpanElement,
} from './defaults'
import { useEditorState, useRefEditorState } from './store/store-hook'

export function useEnterDrawToInsertForDiv(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultDivElement)
}

export function useEnterDrawToInsertForSpan(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultSpanElement)
}

export function useEnterDrawToInsertForImage(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultImgElement)
}

export function useEnterDrawToInsertForButton(): (event: React.MouseEvent<Element>) => void {
  return useEnterDrawToInsertForElement(defaultButtonElement)
}

function useEnterDrawToInsertForElement(
  elementFactory: (newUID: string) => JSXElement,
): (event: React.MouseEvent<Element>) => void {
  const dispatch = useEditorState((store) => store.dispatch, 'enterDrawToInsertForDiv dispatch')
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)

  return React.useCallback(
    (event: React.MouseEvent<Element>): void => {
      const modifiers = Modifier.modifiersForEvent(event)
      const newUID = generateUidWithExistingComponents(projectContentsRef.current)

      dispatch([
        enableInsertModeForJSXElement(elementFactory(newUID), newUID, {}, null),
        CanvasActions.createInteractionSession(
          createHoverInteractionViaMouse(CanvasMousePositionRaw!, modifiers, boundingArea()),
        ),
      ])
    },
    [dispatch, projectContentsRef, elementFactory],
  )
}
