import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { boundingRectangleArray, isFiniteRectangle } from '../../../../core/shared/math-utils'
import * as PP from '../../../../core/shared/property-path'
import { cssNumber } from '../../../inspector/common/css-utils'
import { setCssLengthProperty } from '../../commands/set-css-length-command'
import { PostStrategyFixupStep } from '../canvas-strategies'
import { isSizedContainerWithAbsoluteChildren } from '../strategies/group-like-helpers'

export const groupSizingFixup: PostStrategyFixupStep = {
  name: 'Fix Group Sizes',
  fixup: (store) => {
    const metadata = store.jsxMetadata
    return Object.values(metadata)
      .filter((instance) =>
        isSizedContainerWithAbsoluteChildren(metadata, store.allElementProps, instance.elementPath),
      )
      .flatMap((instance) => {
        const aabb = boundingRectangleArray(
          mapDropNulls(
            (e) =>
              e.globalFrame != null && isFiniteRectangle(e.globalFrame) ? e.globalFrame : null,
            MetadataUtils.getChildrenUnordered(metadata, instance.elementPath),
          ),
        )
        if (aabb == null) {
          return []
        }
        return [
          setCssLengthProperty(
            'always',
            instance.elementPath,
            PP.create('style', 'top'),
            { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.y, null) },
            instance.specialSizeMeasurements.parentFlexDirection,
          ),
          setCssLengthProperty(
            'always',
            instance.elementPath,
            PP.create('style', 'left'),
            { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.x, null) },
            instance.specialSizeMeasurements.parentFlexDirection,
          ),
          setCssLengthProperty(
            'always',
            instance.elementPath,
            PP.create('style', 'width'),
            { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.width, null) },
            instance.specialSizeMeasurements.parentFlexDirection,
          ),
          setCssLengthProperty(
            'always',
            instance.elementPath,
            PP.create('style', 'height'),
            { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(aabb.height, null) },
            instance.specialSizeMeasurements.parentFlexDirection,
          ),
        ]
      })
  },
}
