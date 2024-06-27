import { fromArrayIndex, fromField } from '../../../../core/shared/optics/optic-creators'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import * as PP from '../../../../core/shared/property-path'
import { setProperty } from '../../commands/set-property-command'
import { GridControls } from '../../controls/grid-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import type { CSSNumber } from '../../../../components/inspector/common/css-utils'
import { printArrayCSSNumber } from '../../../../components/inspector/common/css-utils'
import { modify } from '../../../../core/shared/optics/optic-utilities'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'

export const resizeGridStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const selectedElement = selectedElements[0]
  const parentPath = EP.parentPath(selectedElement)
  const ok = MetadataUtils.isGridLayoutedContainer(
    MetadataUtils.findElementByElementPath(canvasState.startingMetadata, parentPath),
  )
  if (!ok) {
    return null
  }

  return {
    id: 'resize-grid-strategy',
    name: 'Resize Grid',
    descriptiveLabel: 'Resize Grid',
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: [
      {
        control: GridControls,
        props: {},
        key: `grid-controls-${EP.toString(selectedElement)}`,
        show: 'always-visible',
      },
    ],
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_AXIS_HANDLE', 1),
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_AXIS_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const control = interactionSession.activeControl
      const drag = interactionSession.interactionData.drag
      const dragAmount = control.axis === 'column' ? drag.x : drag.y
      const parentSpecialSizeMeasurements =
        canvasState.startingMetadata[EP.toString(parentPath)].specialSizeMeasurements
      const originalValues =
        control.axis === 'column'
          ? parentSpecialSizeMeasurements.containerGridProperties.gridTemplateColumns
          : parentSpecialSizeMeasurements.containerGridProperties.gridTemplateRows

      if (originalValues == null || originalValues.type !== 'DIMENSIONS') {
        return emptyStrategyApplicationResult
      }
      const originalDimensions = originalValues.dimensions
      const updateOptic = fromArrayIndex<CSSNumber>(control.columnOrRow).compose(fromField('value'))
      const newSetting = modify(updateOptic, (current) => current + dragAmount, originalDimensions)
      const propertyValueAsString = printArrayCSSNumber(newSetting)

      const commands = [
        setProperty(
          'always',
          parentPath,
          PP.create(
            'style',
            control.axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows',
          ),
          propertyValueAsString,
        ),
        setElementsToRerenderCommand([parentPath]),
      ]

      return strategyApplicationResult(commands)
    },
  }
}
