import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type {
  ElementInstanceMetadataMap,
  GridElementProperties,
  GridPosition,
} from '../../../../core/shared/element-template'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import {
  offsetPoint,
  rectContainsPoint,
  windowRectangle,
  type WindowPoint,
} from '../../../../core/shared/math-utils'
import { create } from '../../../../core/shared/property-path'
import type { CanvasCommand } from '../../commands/commands'
import { setProperty } from '../../commands/set-property-command'
import type { GridCellCoordinates } from '../../controls/grid-controls'
import { gridCellCoordinates } from '../../controls/grid-controls'
import { canvasPointToWindowPoint } from '../../dom-lookup'
import type { DragInteractionData } from '../interaction-state'
import { stripNulls } from '../../../../core/shared/array-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'

export function getGridCellUnderMouse(mousePoint: WindowPoint) {
  return getGridCellAtPoint(mousePoint, false)
}

function getGridCellUnderMouseRecursive(mousePoint: WindowPoint) {
  return getGridCellAtPoint(mousePoint, true)
}

function getGridCellAtPoint(
  windowPoint: WindowPoint,
  duplicating: boolean,
): { id: string; coordinates: GridCellCoordinates } | null {
  function maybeRecursivelyFindCellAtPoint(elements: Element[]): Element | null {
    // If this used during duplication, the canvas controls will be in the way and we need to traverse the children too.
    for (const element of elements) {
      if (element.id.startsWith('gridcell-')) {
        const rect = element.getBoundingClientRect()
        if (rectContainsPoint(windowRectangle(rect), windowPoint)) {
          return element
        }
      }

      if (duplicating) {
        const child = maybeRecursivelyFindCellAtPoint(Array.from(element.children))
        if (child != null) {
          return child
        }
      }
    }

    return null
  }

  const cellUnderMouse = maybeRecursivelyFindCellAtPoint(
    document.elementsFromPoint(windowPoint.x, windowPoint.y),
  )
  if (cellUnderMouse == null) {
    return null
  }

  const row = cellUnderMouse.getAttribute('data-grid-row')
  const column = cellUnderMouse.getAttribute('data-grid-column')
  return {
    id: cellUnderMouse.id,
    coordinates: gridCellCoordinates(
      row == null ? 0 : parseInt(row),
      column == null ? 0 : parseInt(column),
    ),
  }
}

export function runGridRearrangeMove(
  targetElement: ElementPath,
  selectedElement: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  interactionData: DragInteractionData,
  canvasScale: number,
  canvasOffset: CanvasVector,
  targetGridCell: GridCellCoordinates | null,
  duplicating: boolean,
): { commands: CanvasCommand[]; targetGridCell: GridCellCoordinates | null } {
  let commands: CanvasCommand[] = []

  if (interactionData.drag == null) {
    return { commands: [], targetGridCell: null }
  }

  const mouseWindowPoint = canvasPointToWindowPoint(
    offsetPoint(interactionData.dragStart, interactionData.drag),
    canvasScale,
    canvasOffset,
  )

  let newTargetGridCell = targetGridCell ?? null
  const cellUnderMouse = duplicating
    ? getGridCellUnderMouseRecursive(mouseWindowPoint)
    : getGridCellUnderMouse(mouseWindowPoint)
  if (cellUnderMouse != null) {
    newTargetGridCell = cellUnderMouse.coordinates
  }

  if (newTargetGridCell == null || newTargetGridCell.row < 1 || newTargetGridCell.column < 1) {
    return { commands: [], targetGridCell: null }
  }

  const originalElementMetadata = MetadataUtils.findElementByElementPath(
    jsxMetadata,
    selectedElement,
  )
  if (originalElementMetadata == null) {
    return { commands: [], targetGridCell: null }
  }

  function getGridProperty(field: keyof GridElementProperties, fallback: number) {
    const propValue = originalElementMetadata?.specialSizeMeasurements.elementGridProperties[field]
    return propValue == null || propValue === 'auto' ? 0 : propValue.numericalPosition ?? fallback
  }

  const gridColumnStart = getGridProperty('gridColumnStart', 0)
  const gridColumnEnd = getGridProperty('gridColumnEnd', 1)
  const gridRowStart = getGridProperty('gridRowStart', 0)
  const gridRowEnd = getGridProperty('gridRowEnd', 1)

  commands.push(
    setProperty(
      'always',
      targetElement,
      create('style', 'gridColumnStart'),
      newTargetGridCell.column,
    ),
    setProperty(
      'always',
      targetElement,
      create('style', 'gridColumnEnd'),
      Math.max(
        newTargetGridCell.column,
        newTargetGridCell.column + (gridColumnEnd - gridColumnStart),
      ),
    ),
    setProperty('always', targetElement, create('style', 'gridRowStart'), newTargetGridCell.row),
    setProperty(
      'always',
      targetElement,
      create('style', 'gridRowEnd'),
      Math.max(newTargetGridCell.row, newTargetGridCell.row + (gridRowEnd - gridRowStart)),
    ),
  )

  return {
    commands: commands,
    targetGridCell: newTargetGridCell,
  }
}

export function gridPositionToValue(p: GridPosition | null | undefined): string | number | null {
  if (p == null) {
    return null
  }
  if (p === 'auto') {
    return 'auto'
  }

  return p.numericalPosition
}

export function setGridProps(
  elementPath: ElementPath,
  gridProps: Partial<GridElementProperties>,
): CanvasCommand[] {
  return stripNulls([
    optionalMap(
      (s) => setProperty('always', elementPath, create('style', 'gridColumnStart'), s),
      gridPositionToValue(gridProps?.gridColumnStart),
    ),
    optionalMap(
      (s) => setProperty('always', elementPath, create('style', 'gridColumnEnd'), s),
      gridPositionToValue(gridProps?.gridColumnEnd),
    ),
    optionalMap(
      (s) => setProperty('always', elementPath, create('style', 'gridRowStart'), s),
      gridPositionToValue(gridProps?.gridRowStart),
    ),
    optionalMap(
      (s) => setProperty('always', elementPath, create('style', 'gridRowEnd'), s),
      gridPositionToValue(gridProps?.gridRowEnd),
    ),
  ])
}
