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

  name: () => 'Move (Relative)',

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

    // should we also support absolute elements, for which we'll do something like `CONVERT_TO_RELATIVE`?
    return meta.specialSizeMeasurements.position === 'relative'
  },

  controlsToRender: [],

  fitness: (canvasState, interactionState, _sessionState) => {
    const { interactionData, activeControl } = interactionState
    if (!(interactionData.type === 'DRAG' && interactionData.drag != null)) {
      return 0
    }
    if (activeControl.type !== 'BOUNDING_AREA') {
      return 0
    }

    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length === 0) {
      return 0
    }
    const filteredSelectedElements = getDragTargets(selectedElements)
    const last = filteredSelectedElements[filteredSelectedElements.length - 1]
    const meta = MetadataUtils.findElementByElementPath(interactionState.latestMetadata, last)
    if (!meta) {
      return 0
    }
    const offsets = getStyleOffsets(meta)
    if (!offsets) {
      return 0
    }

    const hasOffsets =
      offsets.left != null || offsets.top != null || offsets.right != null || offsets.bottom != null

    return hasOffsets
      ? 4 // +1 than reorder flow
      : 1 // there should be a more structured way to define priorities (:
  },

  apply: (canvasState, interactionState, sessionState) => {
    const { interactionData } = interactionState
    if (!(interactionData.type === 'DRAG' && interactionData.drag != null)) {
      return emptyStrategyApplicationResult
    }

    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length === 0) {
      return emptyStrategyApplicationResult
    }

    const filteredSelectedElements = getDragTargets(selectedElements)
    const last = filteredSelectedElements[filteredSelectedElements.length - 1]
    const meta = MetadataUtils.findElementByElementPath(sessionState.startingMetadata, last)
    if (meta == null) {
      return emptyStrategyApplicationResult
    }
    const offsets = getStyleOffsets(meta)
    if (!offsets) {
      return emptyStrategyApplicationResult
    }

    const { drag } = interactionData

    const commands: Array<CanvasCommand> = []

    // should set a direction only if it's either defined in the original offsets or if the counterpart (horiz/vert) is not defined
    // vertical
    commands.push(
      ...applyStyle({
        name: 'top',
        initial: offsets.top,
        delta: drag.y,
        keep: offsets.bottom == null,
        path: last,
      }),
      ...applyStyle({
        name: 'bottom',
        initial: offsets.bottom,
        delta: -drag.y,
        path: last,
      }),
    )
    // horizontal
    commands.push(
      ...applyStyle({
        name: 'left',
        initial: offsets.left,
        delta: drag.x,
        keep: offsets.right == null,
        path: last,
      }),
      ...applyStyle({
        name: 'right',
        initial: offsets.right,
        delta: -drag.x,
        path: last,
      }),
    )

    return strategyApplicationResult(commands)
  },
}

const applyStyle = (params: {
  name: 'top' | 'left' | 'bottom' | 'right'
  initial: number | null
  delta: number
  keep?: boolean // if true, this item will be preferred
  path: ElementPath
}): CanvasCommand[] => {
  const { name, initial, delta, keep, path } = params

  const skip = initial == null && !keep
  if (skip) {
    return []
  }

  const value = (initial || 0) + delta
  return [
    setCssLengthProperty('always', path, stylePropPathMappingFn(name, ['style']), value, undefined),
  ]
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
    top: getOffsetPropValue('top', attrs),
    left: getOffsetPropValue('left', attrs),
    bottom: getOffsetPropValue('bottom', attrs),
    right: getOffsetPropValue('right', attrs),
  }
}
