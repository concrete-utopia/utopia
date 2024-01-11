import React from 'react'
import type { EditorAction, EditorDispatch } from '../action-types'
import {
  claimControlOverProject,
  displayControlErrorToast,
  releaseControlOverProject,
  snatchControlOverProject,
} from './collaborative-editing'
import { Substores, useEditorState } from './store-hook'
import { switchEditorMode, updateProjectServerState } from '../actions/action-creators'
import { EditorModes } from '../editor-modes'
import type { ControlChangedRoomEvent } from '../../../../liveblocks.config'
import { useBroadcastEvent, useEventListener } from '../../../../liveblocks.config'

interface CollaborationStateUpdaterProps {
  projectId: string | null
  dispatch: EditorDispatch
}

const controlChangedEvent: ControlChangedRoomEvent = {
  type: 'CONTROL_CHANGED',
}

export const CollaborationStateUpdater = React.memo(
  (props: React.PropsWithChildren<CollaborationStateUpdaterProps>) => {
    const { projectId, dispatch, children } = props
    const isMyProject = useEditorState(
      Substores.projectServerState,
      (store) => store.projectServerState.isMyProject,
      'CollaborationStateUpdater isMyProject',
    )

    const handleControlUpdate = React.useCallback(
      (newHolderOfTheBaton: boolean) => {
        let actions: Array<EditorAction> = [
          updateProjectServerState({ currentlyHolderOfTheBaton: newHolderOfTheBaton }),
        ]
        // Makes sense for the editing user to be in control and they probably want to be editing
        // when they regain control.
        if (newHolderOfTheBaton) {
          actions.push(switchEditorMode(EditorModes.selectMode(null, false, 'none')))
        }
        dispatch(actions)
      },
      [dispatch],
    )

    const broadcast = useBroadcastEvent()

    // Handle events that appear to have come from the above broadcast call.
    useEventListener((data) => {
      if (data.event.type === 'CONTROL_CHANGED') {
        if (isMyProject === 'yes') {
          void claimControlOverProject(projectId)
            .then((controlResult) => {
              const newHolderOfTheBaton = controlResult ?? false
              handleControlUpdate(newHolderOfTheBaton ?? false)
            })
            .catch((error) => {
              console.error('Error when claiming control.', error)
              displayControlErrorToast(
                dispatch,
                'Error while attempting to claim control over this project.',
              )
            })
        }
      }
    })

    React.useEffect(() => {
      // If the window becomes focused then snatch control of the project.
      function didFocus(): void {
        if (projectId != null) {
          // Only attempt to do any kind of snatching of control if the project is "mine".
          if (isMyProject === 'yes') {
            void snatchControlOverProject(projectId)
              .then((controlResult) => {
                const newHolderOfTheBaton = controlResult ?? false
                handleControlUpdate(newHolderOfTheBaton)
                broadcast(controlChangedEvent)
              })
              .catch((error) => {
                console.error('Error when snatching control.', error)
                displayControlErrorToast(
                  dispatch,
                  'Error while attempting to gain control over this project.',
                )
              })
          }
        }
      }
      // If the window is blurred, that means the editor is no longer focused, so release control.
      function didBlur(): void {
        if (projectId != null) {
          void releaseControlOverProject(projectId)
            .then(() => {
              dispatch([updateProjectServerState({ currentlyHolderOfTheBaton: false })])
              broadcast(controlChangedEvent)
            })
            .catch((error) => {
              console.error('Error when releasing control.', error)
              displayControlErrorToast(
                dispatch,
                'Error while attempting to release control over this project.',
              )
            })
        }
      }
      window.addEventListener('focus', didFocus)
      window.addEventListener('blur', didBlur)
      return () => {
        window.removeEventListener('focus', didFocus)
        window.removeEventListener('blur', didBlur)
      }
    }, [dispatch, projectId, isMyProject, handleControlUpdate, broadcast])
    return <>{children}</>
  },
)
