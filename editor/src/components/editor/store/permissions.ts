import { useIsLoggedIn } from '../../../core/shared/multiplayer-hooks'
import { useIsMyProject } from './collaborative-editing'

export type Permissions = {
  edit: boolean // Edit the open project via the canvas, inspector, code editor
  comment: boolean // Add comments
}

export function usePermissions(): Permissions {
  return {
    edit: useIsMyProject(),
    comment: useIsLoggedIn(),
  }
}
