import * as PP from '../../../core/shared/property-path'
import { setProperty } from '../../canvas/commands/set-property-command'
import {
  Axis,
  hugContentsApplicableForContainer,
  nukeSizingPropsForAxisCommand,
  widthHeightFromAxis,
} from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'

export const hugContentsBasicStrategy = (axis: Axis): InspectorStrategy => ({
  name: 'Set to Hug',
  strategy: (metadata, elementPaths) => {
    const elements = elementPaths.filter((path) =>
      hugContentsApplicableForContainer(metadata, path),
    )

    if (elements.length === 0) {
      return null
    }

    return elements.flatMap((path) => [
      nukeSizingPropsForAxisCommand(axis, path),
      setProperty('always', path, PP.create(['style', widthHeightFromAxis(axis)]), 'min-content'),
    ])
  },
})
