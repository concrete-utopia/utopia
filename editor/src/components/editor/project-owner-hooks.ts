import * as React from 'react'
import { useDispatch } from './store/dispatch-context'
import { Substores, useSelectorWithCallback } from './store/store-hook'
import type { ProjectServerState } from './store/project-server-state'
import { removeToast, showToast } from './actions/action-creators'
import { notice } from '../common/notice'
import { assertNever } from '../../core/shared/utils'

const OwnershipToastID = 'project-ownership-toast'

interface OwnershipValues {
  projectOwnership: ProjectServerState['isMyProject']
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
        switch (ownershipValues.projectOwnership) {
          case 'yes':
            // Remove the toast if we switch to a project that the user owns.
            dispatch([removeToast(OwnershipToastID)])
            break
          case 'no':
            // Add the toast if we switch to a project that the user does not own.
            dispatch([
              showToast(
                notice(
                  'Viewer Mode: As you are not the owner of this project, it is read-only.',
                  'NOTICE',
                  true,
                  OwnershipToastID,
                ),
              ),
            ])
            break
          case 'unknown':
            // Do nothing.
            break
          default:
            assertNever(ownershipValues.projectOwnership)
        }
      }
    },
    [dispatch],
  )
  useSelectorWithCallback(
    Substores.fullStore,
    (store) => {
      return {
        projectOwnership: store.projectServerState.isMyProject,
        projectID: store.editor.id,
      }
    },
    showOrHideWarning,
    'useDisplayOwnershipWarning useSelectorWithCallback',
  )
}
