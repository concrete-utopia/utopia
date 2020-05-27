import * as React from 'react'
import { colorTheme } from 'uuiui'
import { ControlStatus } from '../widgets/control-status'
import { CSSBackgroundLayer, CSSTransformItem, CSSUnknownArrayItem } from './css-utils'

const isControlledStyling = {
  backgroundColor: colorTheme.primary.shade(5).value,
  color: colorTheme.primary.value,
}

const isNotControlledStyling = {
  backgroundColor: undefined,
  color: undefined,
}

export function useGetSubsectionHeaderStyle(controlStatus: ControlStatus): React.CSSProperties {
  // TODO instead of this, make the inspector hook return the `PropertyStatus` too, and just use PropertyStatus.controlled
  const isControlled =
    controlStatus === 'controlled' ||
    controlStatus === 'controlled-nodegraph' ||
    controlStatus === 'multiselect-controlled' ||
    controlStatus === 'multiselect-identical-controlled-nodegraph' ||
    controlStatus === 'unoverwritable' ||
    controlStatus === 'multiselect-unoverwritable'

  return isControlled ? isControlledStyling : isNotControlledStyling
}

export type CSSArrayItem = CSSBackgroundLayer | CSSTransformItem | CSSUnknownArrayItem

export function getIndexedSpliceArrayItem<T extends CSSArrayItem>(index: number) {
  return function spliceArrayItem(_: any, oldValue: ReadonlyArray<T>): ReadonlyArray<T> {
    let newArrayItems = [...oldValue]
    newArrayItems.splice(index, 1)
    return newArrayItems
  }
}
