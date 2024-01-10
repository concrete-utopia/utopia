import React from 'react'
import type { EditorAction, EditorDispatch } from '../action-types'
import { releaseControlOverProject, snatchControlOverProject } from './collaborative-editing'
import { Substores, useEditorState } from './store-hook'
import { switchEditorMode, updateProjectServerState } from '../actions/action-creators'
import { EditorModes } from '../editor-modes'

interface CollaborationStateUpdaterProps {
  projectId: string | null
  dispatch: EditorDispatch
}

export const CollaborationStateUpdater = React.memo(
  (props: React.PropsWithChildren<CollaborationStateUpdaterProps>) => {
    const { projectId, dispatch, children } = props
    const isMyProject = useEditorState(
      Substores.projectServerState,
      (store) => store.projectServerState.isMyProject,
      'CollaborationStateUpdater isMyProject',
    )
    React.useEffect(() => {
      // If the document is hidden, that means the editor is in the background
      // or minimised, so release control. Otherwise if it has become unhidden
      // then snatch control of the project.
      function didFocus(): void {
        if (projectId != null) {
          // Only attempt to do any kind of snatching of control if the project is "mine".
          if (isMyProject === 'yes') {
            void snatchControlOverProject(projectId).then((controlResult) => {
              const newHolderOfTheBaton = controlResult ?? false
              let actions: Array<EditorAction> = [
                updateProjectServerState({ currentlyHolderOfTheBaton: newHolderOfTheBaton }),
              ]
              // Makes sense for the editing user to be in control and they probably want to be editing
              // when they regain control.
              if (newHolderOfTheBaton) {
                actions.push(switchEditorMode(EditorModes.selectMode(null, false, 'none')))
              }
              dispatch(actions)
            })
          }
        }
      }
      function didBlur(): void {
        if (projectId != null) {
          void releaseControlOverProject(projectId).then(() => {
            dispatch([updateProjectServerState({ currentlyHolderOfTheBaton: false })])
          })
        }
      }
      window.addEventListener('focus', didFocus)
      window.addEventListener('blur', didBlur)
      return () => {
        window.removeEventListener('focus', didFocus)
        window.removeEventListener('blur', didBlur)
      }
    }, [dispatch, projectId, isMyProject])
    return <>{children}</>
  },
)
