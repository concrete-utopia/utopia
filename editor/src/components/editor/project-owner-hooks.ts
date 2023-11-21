import * as React from 'react'
import { useDispatch } from './store/dispatch-context'
import { Substores, useSelectorWithCallback } from './store/store-hook'
import type { ProjectServerState } from './store/project-server-state'
import { removeToast, showToast } from './actions/action-creators'
import { notice } from '../common/notice'
import { assertNever } from '../../core/shared/utils'

const OwnershipToastID = 'project-ownership-toast'

export function useDisplayOwnershipWarning(): void {
  const dispatch = useDispatch()
  const showOrHideWarning = React.useCallback(
    (projectOwnership: ProjectServerState['isMyProject']) => {
      switch (projectOwnership) {
        case 'yes':
          dispatch([removeToast(OwnershipToastID)])
          break
        case 'no':
          dispatch([
            showToast(
              notice(
                'Viewer Mode: Project is read only in this session.',
                'INFO',
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
          assertNever(projectOwnership)
      }
    },
    [dispatch],
  )
  useSelectorWithCallback(
    Substores.projectServerState,
    (store) => store.projectServerState.isMyProject,
    showOrHideWarning,
    'useDisplayOwnershipWarning useSelectorWithCallback',
  )
}
