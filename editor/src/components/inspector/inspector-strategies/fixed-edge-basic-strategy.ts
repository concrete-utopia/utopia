import * as PP from '../../../core/shared/property-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { WhenToRun } from '../../canvas/commands/commands'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../../canvas/commands/set-css-length-command'
import type { CSSNumber } from '../common/css-utils'
import type { InspectorStrategy } from './inspector-strategy'
import { queueGroupTrueUp } from '../../canvas/commands/queue-group-true-up-command'
import {
  groupErrorToastCommand,
  maybeInvalidGroupState,
} from '../../canvas/canvas-strategies/strategies/group-helpers'
import { trueUpElementChanged } from '../../../components/editor/store/editor-state'
import type { LayoutEdgeProp } from '../../../core/layout/layout-helpers-new'

export const fixedEdgeBasicStrategy = (
  whenToRun: WhenToRun,
  edge: LayoutEdgeProp,
  value: CSSNumber,
): InspectorStrategy => ({
  name: 'Set edge to Fixed',
  strategy: (metadata, elementPaths) => {
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
        setCssLengthProperty(
          whenToRun,
          path,
          PP.create('style', edge),
          setExplicitCssValue(value),
          parentFlexDirection,
        ),
        queueGroupTrueUp([trueUpElementChanged(path)]),
      ]
    })
  },
})
