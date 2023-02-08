import * as PP from '../../../core/shared/property-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { WhenToRun } from '../../canvas/commands/commands'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../../canvas/commands/set-css-length-command'
import { CSSNumber } from '../common/css-utils'
import { Axis, widthHeightFromAxis } from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'

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

    return elementPaths.map((path) => {
      const parentFlexDirection =
        MetadataUtils.findElementByElementPath(metadata, path)?.specialSizeMeasurements
          .parentFlexDirection ?? null

      return setCssLengthProperty(
        whenToRun,
        path,
        PP.create('style', widthHeightFromAxis(axis)),
        setExplicitCssValue(value),
        parentFlexDirection,
      )
    })
  },
})
