import * as PP from '../../../core/shared/property-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { WhenToRun } from '../../canvas/commands/commands'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../../canvas/commands/set-css-length-command'
import type { CSSNumber } from '../common/css-utils'
import type { Axis } from '../inspector-common'
import {
  predictElementSize,
  removeExtraPinsWhenSettingSize,
  widthHeightFromAxis,
} from '../inspector-common'
import type { InspectorStrategy } from './inspector-strategy'
import { pushIntendedBoundsAndUpdateGroups } from '../../canvas/commands/push-intended-bounds-and-update-groups-command'

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

    return elementPaths.flatMap((path) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
      const parentFlexDirection =
        elementMetadata?.specialSizeMeasurements.parentFlexDirection ?? null

      const propToChange = widthHeightFromAxis(axis)
      const predictedElementSize = predictElementSize(metadata, path, propToChange, value)

      return [
        ...removeExtraPinsWhenSettingSize(axis, elementMetadata),
        setCssLengthProperty(
          whenToRun,
          path,
          PP.create('style', widthHeightFromAxis(axis)),
          setExplicitCssValue(value),
          parentFlexDirection,
        ),
        pushIntendedBoundsAndUpdateGroups([{ target: path, frame: predictedElementSize }]),
      ]
    })
  },
})
