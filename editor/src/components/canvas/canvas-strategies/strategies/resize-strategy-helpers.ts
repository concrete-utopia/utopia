import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../../core/layout/layout-helpers-new'
import { isRight, right } from '../../../../core/shared/either'
import type { JSXElement } from '../../../../core/shared/element-template'
import type { CanvasRectangle, SimpleRectangle } from '../../../../core/shared/math-utils'
import {
  canvasRectangleToLocalRectangle,
  rectangleDifference,
  roundTo,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { styleStringInArray } from '../../../../utils/common-constants'
import type { FlexDirection } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import type { EdgePosition } from '../../canvas-types'
import type {
  AdjustCssLengthProperties,
  LengthPropertyToAdjust,
} from '../../commands/adjust-css-length-command'
import {
  adjustCssLengthProperties,
  lengthPropertyToAdjust,
} from '../../commands/adjust-css-length-command'
import type { SetCssLengthProperty } from '../../commands/set-css-length-command'
import {
  setCssLengthProperty,
  setValueKeepingOriginalUnit,
} from '../../commands/set-css-length-command'
import type { AbsolutePin } from './resize-helpers'
import { ensureAtLeastTwoPinsForEdgePosition, onlyEnsureOffsetPinsExist } from './resize-helpers'

export type EnsureFramePointsExist =
  | 'ensure-two-frame-points-per-dimension-exists'
  | 'only-offset-frame-points-are-needed'

export function createResizeCommandsFromFrame(
  element: JSXElement,
  selectedElement: ElementPath,
  newFrame: CanvasRectangle,
  originalFrame: CanvasRectangle,
  elementParentBounds: CanvasRectangle | null,
  elementParentFlexDirection: FlexDirection | null,
  edgePosition: EdgePosition,
  ensureFramePointsExist: EnsureFramePointsExist,
): (AdjustCssLengthProperties | SetCssLengthProperty)[] {
  const pins: Array<AbsolutePin> =
    ensureFramePointsExist === 'ensure-two-frame-points-per-dimension-exists'
      ? ensureAtLeastTwoPinsForEdgePosition(right(element.props), edgePosition)
      : onlyEnsureOffsetPinsExist(right(element.props), edgePosition)

  let propertiesToAdjust: Array<LengthPropertyToAdjust> = []
  let commands: Array<AdjustCssLengthProperties | SetCssLengthProperty> = []

  for (const pin of pins) {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )
    const value = getLayoutProperty(pin, right(element.props), styleStringInArray)
    const rectangleDiff = rectangleDifference(originalFrame, newFrame)
    const delta = allPinsFromFrame(rectangleDiff)[pin]
    const roundedDelta = roundTo(delta, 0)
    const pinDirection = pin === 'right' || pin === 'bottom' ? -1 : 1
    if (roundedDelta !== 0) {
      if (isRight(value) && value.value != null) {
        propertiesToAdjust.push(
          lengthPropertyToAdjust(
            stylePropPathMappingFn(pin, styleStringInArray),
            roundedDelta * pinDirection,
            horizontal ? elementParentBounds?.width : elementParentBounds?.height,
            'create-if-not-existing',
          ),
        )
      } else {
        // If this element has a parent, we need to take that parent's bounds into account
        const frameToUse =
          elementParentBounds == null
            ? newFrame
            : canvasRectangleToLocalRectangle(newFrame, elementParentBounds)
        const valueToSet = allPinsFromFrame(frameToUse)[pin]
        commands.push(
          setCssLengthProperty(
            'always',
            selectedElement,
            stylePropPathMappingFn(pin, styleStringInArray),
            setValueKeepingOriginalUnit(
              roundTo(valueToSet, 0),
              horizontal ? elementParentBounds?.width : elementParentBounds?.height,
            ),
            elementParentFlexDirection,
          ),
        )
      }
    }
  }

  commands.push(
    adjustCssLengthProperties(
      'always',
      selectedElement,
      elementParentFlexDirection,
      propertiesToAdjust,
    ),
  )
  return commands
}

function allPinsFromFrame(frame: SimpleRectangle): { [key: string]: number } {
  return {
    left: frame.x,
    top: frame.y,
    width: frame.width,
    height: frame.height,
    right: frame.x + frame.width,
    bottom: frame.y + frame.height,
  }
}
