import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import {
  isFiniteRectangle,
  offsetPoint,
  rectContainsPointInclusive,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { create } from '../../../../core/shared/property-path'
import type { CanvasCommand } from '../../commands/commands'
import { deleteProperties } from '../../commands/delete-properties-command'
import { rearrangeChildren } from '../../commands/rearrange-children-command'
import { GridControls } from '../../controls/grid-controls'
import { recurseIntoChildrenOfMapOrFragment } from '../../gap-utils'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  emptyStrategyApplicationResult,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { setGridProps } from './grid-helpers'

export const rearrangeGridSwapStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (
    selectedElements.length !== 1 ||
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.interactionData.drag == null ||
    interactionSession.activeControl.type !== 'GRID_CELL_HANDLE' ||
    interactionSession.interactionData.modifiers.alt
  ) {
    return null
  }

  const selectedElement = selectedElements[0]
  const ok = MetadataUtils.isGridLayoutedContainer(
    MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      EP.parentPath(selectedElement),
    ),
  )
  if (!ok) {
    return null
  }

  const children = recurseIntoChildrenOfMapOrFragment(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    canvasState.startingElementPathTree,
    EP.parentPath(selectedElement),
  )

  return {
    id: 'rearrange-grid-swap-strategy',
    name: 'Rearrange Grid (Swap)',
    descriptiveLabel: 'Rearrange Grid (Swap)',
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
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_CELL_HANDLE', 1),
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.interactionData.drag == null ||
        interactionSession.activeControl.type !== 'GRID_CELL_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const pointOnCanvas = offsetPoint(
        interactionSession.interactionData.dragStart,
        interactionSession.interactionData.drag,
      )

      const pointerOverChild = children.find(
        (c) =>
          c.globalFrame != null &&
          isFiniteRectangle(c.globalFrame) &&
          rectContainsPointInclusive(c.globalFrame, pointOnCanvas),
      )

      let commands: CanvasCommand[] = []

      if (
        pointerOverChild != null &&
        EP.toUid(pointerOverChild.elementPath) !== interactionSession.activeControl.id
      ) {
        commands.push(
          ...swapChildrenCommands({
            grabbedElementUid: interactionSession.activeControl.id,
            swapToElementUid: EP.toUid(pointerOverChild.elementPath),
            children: children,
            parentPath: EP.parentPath(selectedElement),
          }),
        )
      }

      if (commands == null) {
        return emptyStrategyApplicationResult
      }

      return strategyApplicationResult(commands)
    },
  }
}

const GridPositioningProps: Array<keyof React.CSSProperties> = [
  'gridColumn',
  'gridRow',
  'gridColumnStart',
  'gridColumnEnd',
  'gridRowStart',
  'gridRowEnd',
]

function swapChildrenCommands({
  grabbedElementUid,
  swapToElementUid,
  children,
  parentPath,
}: {
  grabbedElementUid: string
  swapToElementUid: string
  children: ElementInstanceMetadata[]
  parentPath: ElementPath
}): CanvasCommand[] {
  const grabbedElement = children.find((c) => EP.toUid(c.elementPath) === grabbedElementUid)
  const swapToElement = children.find((c) => EP.toUid(c.elementPath) === swapToElementUid)

  if (grabbedElement == null || swapToElement == null) {
    return []
  }

  const rearrangedChildren = children
    .map((c) => {
      if (EP.pathsEqual(c.elementPath, grabbedElement.elementPath)) {
        return swapToElement.elementPath
      }
      if (EP.pathsEqual(c.elementPath, swapToElement.elementPath)) {
        return grabbedElement.elementPath
      }
      return c.elementPath
    })
    .map((path) => EP.dynamicPathToStaticPath(path))

  return [
    rearrangeChildren('always', parentPath, rearrangedChildren),
    deleteProperties(
      'always',
      swapToElement.elementPath,
      GridPositioningProps.map((p) => create('style', p)),
    ),
    deleteProperties(
      'always',
      grabbedElement.elementPath,
      GridPositioningProps.map((p) => create('style', p)),
    ),
    ...setGridProps(
      grabbedElement.elementPath,
      swapToElement.specialSizeMeasurements.elementGridProperties,
    ),
    ...setGridProps(
      swapToElement.elementPath,
      grabbedElement.specialSizeMeasurements.elementGridProperties,
    ),
  ]
}
