import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { stripNulls } from '../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { canvasRectangle, CanvasRectangle, CanvasVector } from '../../core/shared/math-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import { CSSCursor } from './canvas-types'
import { gapControlBounds } from './padding-utils'

export type SimpleFlexDirection = 'row' | 'column' | 'row-reverse' | 'column-reverse'

export interface PathWithBounds {
  bounds: CanvasRectangle
  path: ElementPath
}

export function simpleFlexDirectionFromString(raw: string): SimpleFlexDirection | null {
  switch (raw) {
    case 'row':
      return 'row'
    case 'column':
      return 'column'
    case 'row-reverse':
      return 'row-reverse'
    case 'column-reverse':
      return 'column-reverse'
    default:
      return null
  }
}

export function updateGapValue(
  orientation: SimpleFlexDirection,
  originalValue: number,
  delta: CanvasVector,
): number {
  switch (orientation) {
    case 'row':
      return originalValue + delta.x * 2
    case 'row-reverse':
      return originalValue - delta.x * 2
    case 'column':
      return originalValue + delta.y * 2
    case 'column-reverse':
      return originalValue - delta.y * 2
    default:
      assertNever(orientation)
  }
}

export function cursorFromFlexDirection(direction: SimpleFlexDirection): CSSCursor {
  switch (direction) {
    case 'column':
    case 'column-reverse':
      return CSSCursor.RowResize
    case 'row':
    case 'row-reverse':
      return CSSCursor.ColResize
    default:
      assertNever(direction)
  }
}

export function gapControlBounds(
  parentBounds: CanvasRectangle,
  bounds: CanvasRectangle,
  flexDirection: SimpleFlexDirection,
  gap: number,
): CanvasRectangle {
  if (flexDirection === 'row' || flexDirection === 'row-reverse') {
    return canvasRectangle({
      x: bounds.x + bounds.width,
      y: parentBounds.y,
      width: gap,
      height: parentBounds.height,
    })
  }
  if (flexDirection === 'column' || flexDirection === 'column-reverse') {
    return canvasRectangle({
      x: parentBounds.x,
      y: bounds.y + bounds.height,
      width: parentBounds.width,
      height: gap,
    })
  }

  assertNever(flexDirection)
}

function paddingControlContainerBoundsFromChildBounds(
  parentBounds: CanvasRectangle,
  children: Array<PathWithBounds>,
  gap: number,
  flexDirection: SimpleFlexDirection,
): Array<PathWithBounds> {
  return children.map(({ bounds, path }) => ({
    path: path,
    bounds: gapControlBounds(parentBounds, bounds, flexDirection, gap),
  }))
}

export function gapControlBoundsFromMetadata(
  elementMetadata: ElementInstanceMetadataMap,
  selectedElement: ElementPath,
  gap: number,
  flexDirection: SimpleFlexDirection,
): Array<PathWithBounds> {
  const parentBounds = MetadataUtils.getFrameInCanvasCoords(selectedElement, elementMetadata)
  if (parentBounds == null) {
    return []
  }

  const children = MetadataUtils.getChildrenPaths(elementMetadata, selectedElement)
  const childCanvasBounds = stripNulls(
    children
      .map((childPath) =>
        optionalMap(
          (frame) => ({ path: childPath, bounds: frame }),
          MetadataUtils.getFrameInCanvasCoords(childPath, elementMetadata),
        ),
      )
      .slice(0, -1),
  )

  return paddingControlContainerBoundsFromChildBounds(
    parentBounds,
    childCanvasBounds,
    gap,
    flexDirection,
  )
}
