import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { stripNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  GridElementProperties,
  GridPosition,
} from '../../../../core/shared/element-template'
import {
  isFiniteRectangle,
  offsetPoint,
  rectContainsPointInclusive,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { create } from '../../../../core/shared/property-path'
import type { CanvasCommand } from '../../commands/commands'
import { deleteProperties } from '../../commands/delete-properties-command'
import { rearrangeChildren } from '../../commands/rearrange-children-command'
import { setProperty } from '../../commands/set-property-command'
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

  if (!MetadataUtils.isGridCell(canvasState.startingMetadata, selectedElement)) {
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

function gridPositionToValue(p: GridPosition | null): string | number | null {
  if (p == null) {
    return null
  }
  if (p === 'auto') {
    return 'auto'
  }

  return p.numericalPosition
}

function setGridProps(elementPath: ElementPath, gridProps: GridElementProperties): CanvasCommand[] {
  return stripNulls([
    optionalMap(
      (s) => setProperty('always', elementPath, create('style', 'gridColumnStart'), s),
      gridPositionToValue(gridProps.gridColumnStart),
    ),
    optionalMap(
      (s) => setProperty('always', elementPath, create('style', 'gridColumnEnd'), s),
      gridPositionToValue(gridProps.gridColumnEnd),
    ),
    optionalMap(
      (s) => setProperty('always', elementPath, create('style', 'gridRowStart'), s),
      gridPositionToValue(gridProps.gridRowStart),
    ),
    optionalMap(
      (s) => setProperty('always', elementPath, create('style', 'gridRowEnd'), s),
      gridPositionToValue(gridProps.gridRowEnd),
    ),
  ])
}

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
