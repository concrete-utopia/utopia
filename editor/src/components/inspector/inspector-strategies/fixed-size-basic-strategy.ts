import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import {
  groupErrorToastCommand,
  maybeGroupWithoutFixedSizeForFill,
} from '../../canvas/canvas-strategies/strategies/group-helpers'
import type { WhenToRun } from '../../canvas/commands/commands'
import { queueGroupTrueUp } from '../../canvas/commands/queue-group-true-up-command'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../../canvas/commands/set-css-length-command'
import type { CSSNumber } from '../common/css-utils'
import type { Axis } from '../inspector-common'
import { removeExtraPinsWhenSettingSize, widthHeightFromAxis } from '../inspector-common'
import { maybeInvalidGroupStates } from './inspector-strategies'
import type { InspectorStrategy } from './inspector-strategy'

export const fixedSizeBasicStrategy = (
  whenToRun: WhenToRun,
  axis: Axis,
  value: CSSNumber,
): InspectorStrategy => ({
  name: 'Set to Fixed',
  strategy: (metadata, elementPaths) => {
    if (elementPaths.length === 0) {
      return null
    }

    const maybeInvalidGroupState = maybeInvalidGroupStates(elementPaths, metadata, (path) => {
      const group = MetadataUtils.getJSXElementFromMetadata(metadata, EP.parentPath(path))
      return value.unit === '%' ? maybeGroupWithoutFixedSizeForFill(group) : null
    })
    if (maybeInvalidGroupState != null) {
      return [groupErrorToastCommand(maybeInvalidGroupState)]
    }

    return elementPaths.flatMap((path) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
      const parentFlexDirection =
        elementMetadata?.specialSizeMeasurements.parentFlexDirection ?? null

      return [
        ...removeExtraPinsWhenSettingSize(axis, elementMetadata),
        setCssLengthProperty(
          whenToRun,
          path,
          PP.create('style', widthHeightFromAxis(axis)),
          setExplicitCssValue(value),
          parentFlexDirection,
        ),
        queueGroupTrueUp(path),
      ]
    })
  },
})
