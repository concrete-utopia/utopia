import React from 'react'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import type { WrapInElementWith } from './action-types'
import { wrapInElement } from './actions/action-creators'
import { defaultTransparentViewElement } from './defaults'
import type { InsertionSubject } from './editor-modes'
import { useDispatch } from './store/dispatch-context'
import { useRefEditorState } from './store/store-hook'

export function useWrapInElement(
  elementFactory: (newUID: string) => WrapInElementWith,
): (event: React.MouseEvent<Element>) => void {
  const dispatch = useDispatch()
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)

  return React.useCallback(() => {
    const newUID = generateUidWithExistingComponents(projectContentsRef.current)
    dispatch([wrapInElement(selectedViewsRef.current, elementFactory(newUID))], 'everyone')
  }, [projectContentsRef, dispatch, selectedViewsRef, elementFactory])
}

function makeWrapperDiv(newUID: string): WrapInElementWith {
  return {
    element: defaultTransparentViewElement(newUID),
    importsToAdd: emptyImports(),
  }
}

export function useWrapInDiv(): (event: React.MouseEvent<Element>) => void {
  return useWrapInElement(makeWrapperDiv)
}
