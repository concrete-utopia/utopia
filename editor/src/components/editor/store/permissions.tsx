import React from 'react'
import { assertNever } from '../../../core/shared/utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { useIsMyProject } from './collaborative-editing'

export type Role = 'viewer' | 'owner'

export function useMyRole(): Role | 'unknown' {
  const isMyProject = useIsMyProject()
  const isMultiplayer = isFeatureEnabled('Multiplayer')
  if (!isMultiplayer) {
    return 'unknown'
  }
  return isMyProject ? 'owner' : 'viewer'
}

export type Permissions = {
  // change stuff on the canvas, inspector, code editor
  edit: boolean
  // add comments
  comment: boolean
}

function getPermissionsForRole(role: Role): Permissions {
  switch (role) {
    case 'owner':
      return {
        edit: true,
        comment: true,
      }
    case 'viewer':
      return {
        edit: false,
        comment: false,
      }
    default:
      assertNever(role)
  }
}

export function usePermissions(): Permissions {
  const myRole = useMyRole()
  if (myRole === 'unknown') {
    return {
      edit: false,
      comment: false,
    }
  }
  return getPermissionsForRole(myRole)
}
