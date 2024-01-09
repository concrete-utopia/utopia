import React from 'react'
import type { EditorDispatch } from '../action-types'
import { releaseControlOverProject, snatchControlOverProject } from './collaborative-editing'
import { Substores, useEditorState } from './store-hook'
import { updateProjectServerState } from '../actions/action-creators'

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
      function handleVisibilityChange(): void {
        if (projectId != null) {
          // If the document is hidden, that means the editor is in the background
          // or minimised, so release control. Otherwise if it has become unhidden
          // then snatch control of the project.
          if (document.hidden) {
            void releaseControlOverProject(projectId).then(() => {
              dispatch([updateProjectServerState({ currentlyHolderOfTheBaton: false })])
            })
          } else {
            // Only attempt to do any kind of snatching of control if the project is "mine".
            if (isMyProject === 'yes') {
              void snatchControlOverProject(projectId).then((controlResult) => {
                dispatch([
                  updateProjectServerState({ currentlyHolderOfTheBaton: controlResult ?? false }),
                ])
              })
            }
          }
        }
      }
      document.addEventListener('visibilitychange', handleVisibilityChange)
      return () => {
        document.removeEventListener('visibilitychange', handleVisibilityChange)
      }
    }, [dispatch, projectId, isMyProject])
    return <>{children}</>
  },
)
