import * as React from 'react'
import { useDispatch } from './store/dispatch-context'
import { Substores, useSelectorWithCallback } from './store/store-hook'
import { removeToast, showToast } from './actions/action-creators'
import { notice } from '../common/notice'
import { allowedToEditProject } from './store/collaborative-editing'

const OwnershipToastID = 'project-ownership-toast'

interface OwnershipValues {
  allowedToEdit: boolean
  projectID: string | null
}

export function useDisplayOwnershipWarning(): void {
  const dispatch = useDispatch()
  const showOrHideWarning = React.useCallback(
    (ownershipValues: OwnershipValues) => {
      // Only once a project has been loaded will the project ID be populated.
      // Without this check this hook will only fire before the LOAD action
      // and then the toasts will be cleared before they ever really existed.
      if (ownershipValues.projectID != null) {
        if (ownershipValues.allowedToEdit) {
          // Remove the toast if we switch to a project that the user owns.
          globalThis.requestAnimationFrame(() => {
            dispatch([removeToast(OwnershipToastID)])
          })
        } else {
          // Add the toast if we switch to a project that the user does not own.
          globalThis.requestAnimationFrame(() => {
            dispatch([
              showToast(
                notice(
                  'Viewer Mode: Either you are not the owner of this project or you have this project open elsewhere. As a result it is read-only.',
                  'NOTICE',
                  true,
                  OwnershipToastID,
                ),
              ),
            ])
          })
        }
      }
    },
    [dispatch],
  )
  useSelectorWithCallback(
    Substores.fullStore,
    (store) => {
      return {
        allowedToEdit: allowedToEditProject(store.userState.loginState, store.projectServerState),
        projectID: store.editor.id,
      }
    },
    showOrHideWarning,
    'useDisplayOwnershipWarning useSelectorWithCallback',
  )
}
