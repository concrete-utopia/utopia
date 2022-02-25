import { safeIndex } from '../../../core/shared/array-utils'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { forceNotNull } from '../../../core/shared/optional-utils'
import {
  AdjustNumberCondition,
  adjustNumberInequalityCondition,
  AdjustNumberInequalityCondition,
  adjustNumberProperty,
} from '../commands/commands'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { MinMaxDimensionControls } from '../controls/select-mode/min-max-dimension-controls'
import { PropertyPath } from '../../../core/shared/project-file-types'
import { EdgePosition } from '../canvas-types'
import { CanvasStrategy } from './canvas-strategy-types'

function isHorizontalResize(position: EdgePosition): boolean {
  return position.y === 0.5
}

function isVerticalResize(position: EdgePosition): boolean {
  return position.x === 0.5
}

function isHorizontalOrVerticalResize(position: EdgePosition): boolean {
  return isHorizontalResize(position) || isVerticalResize(position)
}

export const adjustMinMaxDimensionStrategy: CanvasStrategy = {
  name: 'Adjust Min And Max Dimension',
  strategyGroups: new Set(),
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length === 1) {
      const selectedView = canvasState.selectedElements[0]
      const selectedMetadata = MetadataUtils.findElementByElementPath(metadata, selectedView)
      return selectedMetadata?.specialSizeMeasurements.parentLayoutSystem === 'flow'
    }
    return false
  },
  controlsToRender: [
    { control: MinMaxDimensionControls, key: 'min-max-dimension-controls', show: 'always-visible' },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    // Currently only edge resizes.
    return adjustMinMaxDimensionStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'RESIZE_HANDLE' &&
      isHorizontalOrVerticalResize(interactionState.activeControl.edgePosition)
      ? 1
      : 0
  },
  apply: (canvasState, interactionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'RESIZE_HANDLE' &&
      isHorizontalOrVerticalResize(interactionState.activeControl.edgePosition)
    ) {
      const edgePosition = interactionState.activeControl.edgePosition
      // Only looks at the first selected element.
      const targetedElement = forceNotNull(
        'Could not get first element.',
        safeIndex(canvasState.selectedElements, 0),
      )

      let dimensionChange: number = 0
      let additionalPropPath: PropertyPath | null = null
      let dimensionPropPath: PropertyPath
      let boundsPropPath: PropertyPath
      let adjustmentInequality: AdjustNumberInequalityCondition | null = null
      if (isHorizontalResize(edgePosition)) {
        dimensionChange = interactionState.interactionData.drag?.x ?? 0
        if (edgePosition.x === 0) {
          dimensionPropPath = stylePropPathMappingFn('left', ['style'])
          additionalPropPath = stylePropPathMappingFn('width', ['style'])
        } else {
          dimensionPropPath = stylePropPathMappingFn('width', ['style'])
        }
        if (dimensionChange >= 0) {
          boundsPropPath = stylePropPathMappingFn('maxWidth', ['style'])
          adjustmentInequality = adjustNumberInequalityCondition(dimensionPropPath, 'less-than')
        } else {
          boundsPropPath = stylePropPathMappingFn('minWidth', ['style'])
          adjustmentInequality = adjustNumberInequalityCondition(dimensionPropPath, 'greater-than')
        }
      } else {
        dimensionChange = interactionState.interactionData.drag?.y ?? 0
        if (edgePosition.y === 0) {
          dimensionPropPath = stylePropPathMappingFn('top', ['style'])
          additionalPropPath = stylePropPathMappingFn('height', ['style'])
        } else {
          dimensionPropPath = stylePropPathMappingFn('height', ['style'])
        }
        if (dimensionChange >= 0) {
          boundsPropPath = stylePropPathMappingFn('maxHeight', ['style'])
          adjustmentInequality = adjustNumberInequalityCondition(dimensionPropPath, 'less-than')
        } else {
          boundsPropPath = stylePropPathMappingFn('minHeight', ['style'])
          adjustmentInequality = adjustNumberInequalityCondition(dimensionPropPath, 'greater-than')
        }
      }
      const adjustDimensionProperty = adjustNumberProperty(
        'permanent',
        targetedElement,
        dimensionPropPath,
        dimensionChange,
        true,
      )
      const additionalDimensionProperty =
        additionalPropPath == null
          ? []
          : [
              adjustNumberProperty(
                'permanent',
                targetedElement,
                additionalPropPath,
                -dimensionChange,
                true,
              ),
            ]
      // Needs to somehow check against dimension prop.
      const adjustBoundProperty = adjustNumberProperty(
        'permanent',
        targetedElement,
        boundsPropPath,
        adjustmentInequality,
        false,
      )
      return [adjustDimensionProperty, ...additionalDimensionProperty, adjustBoundProperty]
    }

    // Fallback for when the checks above are not satisfied.
    return []
  },
}
