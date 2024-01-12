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

type InputAction = 'read' | 'write'

export type InputPermissions = Record<Role, InputAction>
