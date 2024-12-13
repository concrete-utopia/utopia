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
  removeAlignJustifySelf,
  removeExtraPinsWhenSettingSize,
  widthHeightFromAxis,
} from '../inspector-common'
import type { InspectorStrategy } from './inspector-strategy'
import { queueTrueUpElement } from '../../canvas/commands/queue-true-up-command'
import {
  groupErrorToastCommand,
  maybeInvalidGroupState,
} from '../../canvas/canvas-strategies/strategies/group-helpers'
import { trueUpGroupElementChanged } from '../../../components/editor/store/editor-state'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'

export const fixedSizeBasicStrategy = (
  whenToRun: WhenToRun,
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  axis: Axis,
  value: CSSNumber,
): InspectorStrategy => ({
  name: 'Set to Fixed',
  strategy: () => {
    if (elementPaths.length === 0) {
      return null
    }

    const invalidGroupState = maybeInvalidGroupState(elementPaths, metadata, {
      onGroup: () => (value.unit === '%' ? 'group-has-percentage-pins' : null),
      onGroupChild: (path) => {
        return value.unit === '%' ? 'child-has-percentage-pins' : null
      },
    })
    if (invalidGroupState != null) {
      return [groupErrorToastCommand(invalidGroupState)]
    }

    return elementPaths.flatMap((path) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
      const parentFlexDirection =
        elementMetadata?.specialSizeMeasurements.parentFlexDirection ?? null

      return [
        ...removeExtraPinsWhenSettingSize(axis, elementMetadata),
        ...removeAlignJustifySelf(axis, elementMetadata),
        setCssLengthProperty(
          whenToRun,
          path,
          PP.create('style', widthHeightFromAxis(axis)),
          setExplicitCssValue(value),
          parentFlexDirection,
        ),
        queueTrueUpElement([trueUpGroupElementChanged(path)]),
      ]
    })
  },
})
