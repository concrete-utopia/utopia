import type { LoginState } from '../action-types'
import { isLoggedIn } from '../action-types'
import { checkIsProjectOwner } from './collaborative-editing'
import type { ProjectServerState } from './project-server-state'
import { Substores, useEditorState } from './store-hook'
import type { ProjectServerStateSubstate, UserStateSubstate } from './store-hook-substore-types'

export type Permissions = {
  edit: boolean // Edit the open project via the canvas, inspector, code editor
  comment: boolean // Add comments
}

export function usePermissions(): Permissions {
  return useEditorState(
    Substores.userStateAndProjectServerState,
    (store) => getPermissions(store),
    'usePermissions',
  )
}

export function getPermissions(store: ProjectServerStateSubstate & UserStateSubstate): Permissions {
  return {
    edit: hasEditPermissions(store.projectServerState, store.userState.loginState),
    comment: hasCommentPermission(store.userState.loginState),
  }
}

export function hasEditPermissions(
  projectServerState: ProjectServerState,
  loginState: LoginState,
): boolean {
  if (isLoggedIn(loginState)) {
    return projectServerState.currentlyHolderOfTheBaton
  } else {
    return checkIsProjectOwner(projectServerState)
  }
}

export function hasCommentPermission(loginState: LoginState): boolean {
  return isLoggedIn(loginState)
}
