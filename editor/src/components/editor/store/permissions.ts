import { useIsMyProject } from './collaborative-editing'

export type Permissions = {
  // change stuff on the canvas, inspector, code editor
  edit: boolean
  // add comments
  comment: boolean
}

export function usePermissions(): Permissions {
  return {
    edit: useIsMyProject(),
    comment: true, // this is a placeholder
  }
}
