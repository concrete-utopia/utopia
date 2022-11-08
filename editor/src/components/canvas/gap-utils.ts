import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { stripNulls } from '../../core/shared/array-utils'
import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { defaultEither, isLeft, right } from '../../core/shared/either'
import { ElementInstanceMetadataMap, isJSXElement } from '../../core/shared/element-template'
import { canvasRectangle, CanvasRectangle, CanvasVector } from '../../core/shared/math-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import { CSSCursor } from './canvas-types'
import { CSSNumberWithRenderedValue } from './controls/select-mode/controls-common'
import { cssNumber, CSSNumber } from '../inspector/common/css-utils'

export type SimpleFlexDirectionForGap = 'row' | 'column' | 'row-reverse' | 'column-reverse'

export interface PathWithBounds {
  bounds: CanvasRectangle
  path: ElementPath
}

export function simpleFlexDirectionFromString(raw: string): SimpleFlexDirectionForGap | null {
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

export function dragDeltaForOrientation(
  orientation: SimpleFlexDirectionForGap,
  delta: CanvasVector,
): number {
  switch (orientation) {
    case 'row':
      return delta.x
    case 'row-reverse':
      return -delta.x
    case 'column':
      return delta.y
    case 'column-reverse':
      return -delta.y
    default:
      assertNever(orientation)
  }
}

export function cursorFromFlexDirection(direction: SimpleFlexDirectionForGap): CSSCursor {
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
  flexDirection: SimpleFlexDirectionForGap,
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
  flexDirection: SimpleFlexDirectionForGap,
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
  flexDirection: SimpleFlexDirectionForGap,
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

export interface FlexGapData {
  value: CSSNumberWithRenderedValue
  direction: SimpleFlexDirectionForGap
}

export function maybeFlexGapFromElement(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): FlexGapData | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (
    element == null ||
    element.specialSizeMeasurements.display !== 'flex' ||
    isLeft(element.element) ||
    !isJSXElement(element.element.value)
  ) {
    return null
  }

  const children = MetadataUtils.getChildren(metadata, elementPath)
  if (children.length < 2) {
    return null
  }

  const flexGap = children[0].specialSizeMeasurements.parentFlexGap
  if (flexGap === 0) {
    return null
  }

  const gapFromProps: CSSNumber | undefined =
    defaultEither(
      undefined,
      getLayoutProperty('gap', right(element.element.value.props), ['style']),
    ) ?? cssNumber(flexGap, null)

  const flexDirection =
    optionalMap(
      simpleFlexDirectionFromString,
      children[0].specialSizeMeasurements.parentFlexDirection,
    ) ?? 'row'

  return { value: { renderedValuePx: flexGap, value: gapFromProps }, direction: flexDirection }
}
