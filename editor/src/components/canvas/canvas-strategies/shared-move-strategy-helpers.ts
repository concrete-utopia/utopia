import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import { JSXElement } from '../../../core/shared/element-template'
import { CanvasRectangle, CanvasVector } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import {
  getElementFromProjectContents,
  withUnderlyingTarget,
} from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import {
  adjustCssLengthProperty,
  AdjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { InteractionCanvasState } from './canvas-strategy-types'
import { StrategyState } from './interaction-state'

export function getMoveCommandsForSelectedElement(
  selectedElement: ElementPath,
  drag: CanvasVector,
  canvasState: InteractionCanvasState,
  sessionState: StrategyState,
) {
  const element: JSXElement | null = getElementFromProjectContents(
    selectedElement,
    canvasState.projectContents,
    canvasState.openFile,
  )
  const elementParentBounds =
    MetadataUtils.findElementByElementPath(
      sessionState.startingMetadata, // TODO should this be using the current metadata?
      selectedElement,
    )?.specialSizeMeasurements.immediateParentBounds ?? null

  if (element == null) {
    return []
  }

  return createMoveCommandsForElement(element, selectedElement, drag, elementParentBounds)
}

function createMoveCommandsForElement(
  element: JSXElement,
  selectedElement: ElementPath,
  drag: CanvasVector,
  elementParentBounds: CanvasRectangle | null,
): AdjustCssLengthProperty[] {
  return mapDropNulls(
    (pin) => {
      const horizontal = isHorizontalPoint(
        // TODO avoid using the loaded FramePoint enum
        framePointForPinnedProp(pin),
      )
      const negative = pin === 'right' || pin === 'bottom'
      const value = getLayoutProperty(pin, right(element.props), ['style'])
      if (isRight(value) && value.value != null) {
        // TODO what to do about missing properties?
        return adjustCssLengthProperty(
          'permanent',
          selectedElement,
          stylePropPathMappingFn(pin, ['style']),
          (horizontal ? drag.x : drag.y) * (negative ? -1 : 1),
          horizontal ? elementParentBounds?.width : elementParentBounds?.height,
          true,
        )
      } else {
        return null
      }
    },
    ['top', 'bottom', 'left', 'right'] as const,
  )
}
