import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { stripNulls } from '../../core/shared/array-utils'
import { isLeft } from '../../core/shared/either'
import { ElementInstanceMetadataMap, isJSXElement } from '../../core/shared/element-template'
import { canvasRectangle, CanvasRectangle, CanvasVector } from '../../core/shared/math-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import { CSSCursor } from './canvas-types'
import {
  CSSNumberWithRenderedValue,
  getPropertyFromStyle,
} from './controls/select-mode/controls-common'
import { CSSNumber, FlexDirection } from '../inspector/common/css-utils'
import { AllElementProps } from '../editor/store/editor-state'

export interface PathWithBounds {
  bounds: CanvasRectangle
  path: ElementPath
}

export function dragDeltaForOrientation(orientation: FlexDirection, delta: CanvasVector): number {
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

export function cursorFromFlexDirection(direction: FlexDirection): CSSCursor {
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
  flexDirection: FlexDirection,
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
  flexDirection: FlexDirection,
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
  flexDirection: FlexDirection,
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
  source: 'props' | 'code'
  value: CSSNumberWithRenderedValue
  direction: FlexDirection
}

export function maybeFlexGapFromElement(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
  allElementProps: AllElementProps,
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
  const flexDirection = children[0].specialSizeMeasurements.parentFlexDirection ?? 'row'

  const gapFromProps: CSSNumber | null = getPropertyFromStyle(allElementProps, elementPath, 'gap')

  const spiedGap = getPropertyFromStyle(allElementProps, elementPath, 'gap')

  if (gapFromProps != null) {
    return {
      source: 'props',
      value: { renderedValuePx: flexGap, value: gapFromProps },
      direction: flexDirection,
    }
  }

  if (spiedGap != null) {
    return {
      source: 'code',
      value: { renderedValuePx: flexGap, value: spiedGap },
      direction: flexDirection,
    }
  }

  return null
}
