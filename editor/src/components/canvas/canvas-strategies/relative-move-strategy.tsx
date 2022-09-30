import { ParsedCSSProperties } from 'src/components/inspector/common/css-utils'
import { ElementPath } from 'src/core/shared/project-file-types'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { MetadataUtils, PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'
import { isRight, right } from '../../../core/shared/either'
import { ElementInstanceMetadata, isJSXElement } from '../../../core/shared/element-template'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { CanvasCommand } from '../commands/commands'
import { setCssLengthProperty } from '../commands/set-css-length-command'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'

export const relativeMoveStrategy: CanvasStrategy = {
  id: 'RELATIVE_MOVE',

  name: () => 'Move (relative)',

  isApplicable: (canvasState, _interactionState, metadata) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length === 0) {
      return false
    }

    const filteredSelectedElements = getDragTargets(selectedElements)
    const last = filteredSelectedElements[filteredSelectedElements.length - 1]
    const meta = MetadataUtils.findElementByElementPath(metadata, last)
    if (!meta) {
      return false
    }

    const { position } = meta.specialSizeMeasurements
    if (position !== 'relative') {
      return false
    }

    const offsets = getStyleOffsets(meta)
    if (!offsets) {
      return false
    }

    return (
      offsets.left !== null ||
      offsets.top !== null ||
      offsets.right !== null ||
      offsets.bottom !== null
    )
  },

  controlsToRender: [],

  fitness: (_canvasState, interactionState, _sessionState) => {
    // it fits if:
    // - it's dragging, and
    // - bounding area is defined, and
    // - CMD is pressed
    const { interactionData, activeControl } = interactionState
    if (!(interactionData.type === 'DRAG' && interactionData.drag !== null)) {
      return 0
    }
    if (activeControl.type !== 'BOUNDING_AREA') {
      return 0
    }
    if (!interactionData.modifiers.cmd) {
      return 0
    }
    return 10 // this is silly, it should be less random
  },

  apply: (canvasState, interactionState, sessionState) => {
    const { interactionData } = interactionState
    if (!(interactionData.type === 'DRAG' && interactionData.drag !== null)) {
      return emptyStrategyApplicationResult
    }

    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length === 0) {
      return emptyStrategyApplicationResult
    }

    const filteredSelectedElements = getDragTargets(selectedElements)
    const last = filteredSelectedElements[filteredSelectedElements.length - 1]
    const meta = MetadataUtils.findElementByElementPath(sessionState.startingMetadata, last)
    if (!meta) {
      return emptyStrategyApplicationResult
    }
    const offsets = getStyleOffsets(meta)
    if (!offsets) {
      return emptyStrategyApplicationResult
    }

    const { drag } = interactionData

    const commands: Array<CanvasCommand> = []
    commands.push(applyStyle('left', (offsets.left || 0) + drag.x, last))
    commands.push(applyStyle('top', (offsets.top || 0) + drag.y, last))
    commands.push(applyStyle('right', (offsets.right || 0) - drag.x, last))
    commands.push(applyStyle('bottom', (offsets.bottom || 0) - drag.y, last))
    return strategyApplicationResult(commands)
  },
}

const applyStyle = (
  name: keyof ParsedCSSProperties,
  val: number,
  path: ElementPath,
): CanvasCommand => {
  return setCssLengthProperty(
    'always',
    path,
    stylePropPathMappingFn(name, ['style']),
    val,
    undefined,
  )
}

const getStyleOffsets = (meta: ElementInstanceMetadata) => {
  const getOffsetPropValue = (
    name: 'left' | 'top' | 'right' | 'bottom',
    attrs: PropsOrJSXAttributes,
  ): number | null => {
    const prop = getLayoutProperty(name, attrs, ['style'])
    if (!isRight(prop)) {
      return null
    }
    if (!prop.value) {
      return null
    }
    return prop.value.value
  }

  if (!isRight(meta.element)) {
    return null
  }
  const { value } = meta.element
  if (!isJSXElement(value)) {
    return null
  }

  const attrs = right(value.props)

  return {
    left: getOffsetPropValue('left', attrs),
    top: getOffsetPropValue('top', attrs),
    right: getOffsetPropValue('right', attrs),
    bottom: getOffsetPropValue('bottom', attrs),
  }
}
