import React from 'react'
import type { EditorAction, EditorDispatch } from '../action-types'
import { CollaborationEndpoints } from '../collaborative-endpoints'
import { useRefEditorState } from './store-hook'
import { switchEditorMode, updateProjectServerState } from '../actions/action-creators'
import { EditorModes } from '../editor-modes'
import type { ControlChangedRoomEvent } from '../../../../liveblocks.config'
import { useBroadcastEvent, useEventListener } from '../../../../liveblocks.config'

interface CollaborationStateUpdaterProps {
  projectId: string | null
  loggedIn: boolean
  dispatch: EditorDispatch
}

const controlChangedEvent: ControlChangedRoomEvent = {
  type: 'CONTROL_CHANGED',
}

export const CollaborationStateUpdater = React.memo(
  (props: React.PropsWithChildren<CollaborationStateUpdaterProps>) => {
    const { projectId, dispatch, loggedIn, children } = props
    const isMyProjectRef = useRefEditorState((store) => store.projectServerState.isMyProject)
    const currentlyHolderOfTheBatonRef = useRefEditorState(
      (store) => store.projectServerState.currentlyHolderOfTheBaton,
    )
    const [currentlyAttemptingToSnatch, setCurrentlyAttemptingToSnatch] = React.useState(false)

    const handleControlUpdate = React.useCallback(
      (newHolderOfTheBaton: boolean) => {
        let actions: Array<EditorAction> = [
          updateProjectServerState({ currentlyHolderOfTheBaton: newHolderOfTheBaton }),
        ]
        dispatch(actions)
      },
      [dispatch],
    )

    const broadcast = useBroadcastEvent()

    // Handle events that appear to have come from the above broadcast call.
    useEventListener((data) => {
      if (data.event.type === 'CONTROL_CHANGED') {
        if (loggedIn && isMyProjectRef.current === 'yes') {
          void CollaborationEndpoints.claimControlOverProject(projectId)
            .then((controlResult) => {
              const newHolderOfTheBaton = controlResult ?? false
              handleControlUpdate(newHolderOfTheBaton)
            })
            .catch((error) => {
              console.error('Error when claiming control.', error)
              CollaborationEndpoints.displayControlErrorToast(
                dispatch,
                'Error while attempting to claim control over this project.',
              )
            })
        }
      }
    })

    React.useEffect(() => {
      // If the window becomes focused then snatch control of the project.
      function attemptToSnatchControl(): void {
        if (projectId != null) {
          // Only attempt to do any kind of snatching of control if:
          // - The user is logged in.
          // - The project is "mine".
          // - This instance does not already hold control of the project.
          // - There isn't already an attempt to snatch control inflight.
          if (
            loggedIn &&
            isMyProjectRef.current === 'yes' &&
            !currentlyHolderOfTheBatonRef.current &&
            !currentlyAttemptingToSnatch
          ) {
            setCurrentlyAttemptingToSnatch(true)
            void CollaborationEndpoints.snatchControlOverProject(projectId)
              .then((controlResult) => {
                const newHolderOfTheBaton = controlResult ?? false
                handleControlUpdate(newHolderOfTheBaton)
                broadcast(controlChangedEvent)
              })
              .catch((error) => {
                console.error('Error when snatching control.', error)
                CollaborationEndpoints.displayControlErrorToast(
                  dispatch,
                  'Error while attempting to gain control over this project.',
                )
              })
              .finally(() => {
                setCurrentlyAttemptingToSnatch(false)
              })
          }
        }
      }
      window.addEventListener('focus', attemptToSnatchControl)
      window.addEventListener('click', attemptToSnatchControl, { capture: true })
      return () => {
        window.removeEventListener('focus', attemptToSnatchControl)
        window.removeEventListener('click', attemptToSnatchControl, { capture: true })
      }
    }, [
      dispatch,
      projectId,
      handleControlUpdate,
      broadcast,
      isMyProjectRef,
      currentlyHolderOfTheBatonRef,
      currentlyAttemptingToSnatch,
      loggedIn,
    ])
    return <>{children}</>
  },
)
