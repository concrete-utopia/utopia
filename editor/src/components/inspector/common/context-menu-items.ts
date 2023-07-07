import type { PropertyPath } from '../../../core/shared/project-file-types'
import type { ContextMenuItem } from '../../context-menu-items'
import type { OnUnsetValues } from './property-path-hooks'

export const addSetProperty = {
  name: 'Set with Visual script',
  enabled: false, // TODO missing feature
  action: (data: any) => {
    // TODO missing feature
  },
}

export function unsetPropertyMenuItem(
  propName: string,
  onUnset: () => void,
): ContextMenuItem<unknown> {
  return {
    name: `Unset ${propName}`,
    enabled: true,
    action: () => onUnset(),
  }
}

export function removeRow(onUnset: (...args: any) => void): ContextMenuItem<null> {
  return {
    name: `Remove Row`,
    enabled: true,
    action: () => onUnset(),
  }
}

interface UnsetValueData {
  onUnsetValue: (property: PropertyPath) => void
  property: PropertyPath
}

export const addOnUnsetValues = (
  prettyNames: Array<React.ReactText>,
  onUnsetValues: OnUnsetValues,
): ContextMenuItem<null> => {
  let name: string
  if (prettyNames.length === 1) {
    // Unset backgroundColor
    name = `Unset ${prettyNames[0]}`
  } else if (prettyNames.length === 2) {
    // Unset backgroundColor and backgroundSize
    name = `Unset ${prettyNames.join(' and ')}`
  } else {
    // Unset backgroundColor, backgroundImage, and backgroundSize
    const workingNames = [...prettyNames]
    workingNames[workingNames.length - 1] = `and ${workingNames[workingNames.length - 1]}`
    name = `Unset ${workingNames.join(', ')}`
  }
  return {
    name,
    enabled: true,
    action: () => onUnsetValues(),
  }
}

export const optionalAddOnUnsetValues = (
  addUnsetEntry: boolean,
  prettyNames: Array<React.ReactText>,
  onUnsetValues: OnUnsetValues,
): Array<ContextMenuItem<null>> => {
  if (!addUnsetEntry) {
    return []
  } else {
    return [addOnUnsetValues(prettyNames, onUnsetValues)]
  }
}
