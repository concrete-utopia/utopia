import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import {
  boundingRectangleArray,
  canvasRectangle,
  CanvasRectangle,
  isFiniteRectangle,
  rectanglesEqual,
} from '../../../../core/shared/math-utils'
import * as PP from '../../../../core/shared/property-path'
import { cssNumber } from '../../../inspector/common/css-utils'
import { CanvasCommand } from '../../commands/commands'
import { setCssLengthProperty } from '../../commands/set-css-length-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { PostStrategyFixupStep } from '../canvas-strategies'
import { isSizedContainerWithAbsoluteChildren } from '../strategies/group-like-helpers'

function setElementTopLeftWidthHeight(
  instance: ElementInstanceMetadata,
  aabb: CanvasRectangle,
): Array<CanvasCommand> {
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
}

export const groupSizingFixup: PostStrategyFixupStep = {
  name: 'Fix Group Sizes',
  fixup: (store) => {
    const metadata = store.jsxMetadata
    return Object.values(metadata)
      .filter((instance) =>
        isSizedContainerWithAbsoluteChildren(metadata, store.allElementProps, instance.elementPath),
      )
      .flatMap((instance) => {
        const children = MetadataUtils.getChildrenUnordered(metadata, instance.elementPath)
        const aabb = boundingRectangleArray(
          mapDropNulls(
            (e) =>
              e.globalFrame != null && isFiniteRectangle(e.globalFrame) ? e.globalFrame : null,
            children,
          ),
        )

        if (aabb == null) {
          return []
        }

        if (
          instance.globalFrame != null &&
          isFiniteRectangle(instance.globalFrame) &&
          rectanglesEqual(instance.globalFrame, aabb)
        ) {
          return []
        }

        return [
          ...setElementTopLeftWidthHeight(instance, aabb),
          ...children.flatMap((child) =>
            child.globalFrame != null && isFiniteRectangle(child.globalFrame)
              ? setElementTopLeftWidthHeight(
                  child,
                  canvasRectangle({
                    x: child.globalFrame.x - aabb.x,
                    y: child.globalFrame.y - aabb.y,
                    width: child.globalFrame.width,
                    height: child.globalFrame.height,
                  }),
                )
              : [],
          ),
          setElementsToRerenderCommand('rerender-all-elements'),
        ]
      })
  },
}
