import React from 'react'
import { useDispatch } from './store/dispatch-context'
import { Substores, useEditorState } from './store/store-hook'
import { isRight } from '../../core/shared/either'
import { isJSXElement } from '../../core/shared/element-template'
import * as EP from '../../core/shared/element-path'
import { addCollapsedViews } from './actions/action-creators'

export function useDefaultCollapsedViews() {
  const dispatch = useDispatch()

  // keep track of the previously collapsed element paths, or they will be re-collapsed
  type PreviouslyCollapsed = { [path: string]: boolean }
  const previouslyCollapsed = React.useRef<PreviouslyCollapsed>({})

  const elementNamesByElementPath = useEditorState(
    Substores.metadata,
    (store) => {
      const elements = Object.values(store.editor.jsxMetadata)
      return elements.reduce((acc, element) => {
        if (isRight(element.element) && isJSXElement(element.element.value)) {
          acc[EP.toString(element.elementPath)] = element.element.value.name.baseVariable
        }
        return acc
      }, {} as { [key: string]: string })
    },
    'useDefaultCollapsed elementNamesByElementPath',
  )

  const collapsedPaths = useEditorState(
    Substores.navigator,
    (store): Set<string> => new Set(store.editor.navigator.collapsedViews.map(EP.toString)),
    'useDefaultCollapsed collapsedPaths',
  )

  React.useEffect(() => {
    const newCollapsedViews = Object.keys(elementNamesByElementPath).filter((path) => {
      return (
        // the collapsed views don't already contain the element
        !collapsedPaths.has(path) &&
        // the collapsed view wasn't already collapsed before
        previouslyCollapsed.current[path] == null &&
        // the element is a `head`
        elementNamesByElementPath[path] === 'head'
      )
    })

    if (newCollapsedViews.length > 0) {
      queueMicrotask(() => {
        // 1. add the new collapsed views
        dispatch([addCollapsedViews(newCollapsedViews.map(EP.fromString))])
        // 2. store the new added views to the previous ones
        previouslyCollapsed.current = newCollapsedViews.reduce(
          (acc, path) => ({ ...acc, [path]: true }),
          previouslyCollapsed.current,
        )
      })
    }
  }, [elementNamesByElementPath, collapsedPaths, dispatch, previouslyCollapsed])
}
