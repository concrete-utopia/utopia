import { MetadataUtils } from '../../core/model/element-metadata-utils'
import {
  createArrayWithLength,
  matrixGetter,
  reverse,
  stripNulls,
} from '../../core/shared/array-utils'
import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { defaultEither, isLeft, mapEither, right } from '../../core/shared/either'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../core/shared/element-template'
import { isJSXElement } from '../../core/shared/element-template'
import type { CanvasRectangle, CanvasVector, Size } from '../../core/shared/math-utils'
import { canvasRectangle, isInfinityRectangle } from '../../core/shared/math-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import { assertNever } from '../../core/shared/utils'
import type { StyleInfo } from './canvas-types'
import { CSSCursor } from './canvas-types'
import type { CSSNumberWithRenderedValue } from './controls/select-mode/controls-common'
import type { CSSNumber, FlexDirection } from '../inspector/common/css-utils'
import type { Sides } from 'utopia-api/core'
import { sides } from 'utopia-api/core'
import { styleStringInArray } from '../../utils/common-constants'
import {
  getElementPathTreeChildren,
  getSubTree,
  type ElementPathTrees,
} from '../../core/shared/element-path-tree'
import { isReversedFlexDirection } from '../../core/model/flex-utils'
import * as EP from '../../core/shared/element-path'
import { treatElementAsFragmentLike } from './canvas-strategies/strategies/fragment-like-helpers'
import type { AllElementProps } from '../editor/store/editor-state'
import type { GridData } from './controls/grid-controls-for-strategies'
import { getNullableAutoOrTemplateBaseString } from './controls/grid-controls-for-strategies'

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
      return CSSCursor.GapNS
    case 'row':
    case 'row-reverse':
      return CSSCursor.GapEW
    default:
      assertNever(direction)
  }
}

export function cursorFromAxis(axis: Axis): CSSCursor {
  switch (axis) {
    case 'column':
      return CSSCursor.GapEW
    case 'row':
      return CSSCursor.GapNS
    default:
      assertNever(axis)
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

function inset(sidess: Sides, rect: CanvasRectangle): CanvasRectangle {
  const { left, top, bottom, r } = {
    left: sidess.left ?? 0,
    top: sidess.top ?? 0,
    bottom: sidess.bottom ?? 0,
    r: sidess.right ?? 0,
  }
  return canvasRectangle({
    x: rect.x + left,
    y: rect.y + top,
    width: rect.width - (left + r),
    height: rect.height - (bottom + top),
  })
}

export function gapControlBoundsFromMetadata(
  elementMetadata: ElementInstanceMetadataMap,
  parentPath: ElementPath,
  children: ElementPath[],
  gap: number,
  flexDirection: FlexDirection,
): Array<PathWithBounds> {
  const elementPadding =
    MetadataUtils.findElementByElementPath(elementMetadata, parentPath)?.specialSizeMeasurements
      .padding ?? sides(0, 0, 0, 0)
  const parentFrame = MetadataUtils.getFrameInCanvasCoords(parentPath, elementMetadata)
  if (parentFrame == null || isInfinityRectangle(parentFrame)) {
    return []
  }

  const parentBounds = inset(elementPadding, parentFrame)

  // Needs to handle reversed content as that will be flipped in the visual order, which changes
  // what elements will be either side of the gaps.
  const possiblyReversedChildren = isReversedFlexDirection(flexDirection)
    ? reverse(children)
    : children

  const childCanvasBounds = stripNulls(
    possiblyReversedChildren
      .map((childPath) => {
        const childFrame = MetadataUtils.getFrameInCanvasCoords(childPath, elementMetadata)
        if (childFrame == null || isInfinityRectangle(childFrame)) {
          return null
        } else {
          return { path: childPath, bounds: childFrame }
        }
      })
      .slice(0, -1),
  )

  return paddingControlContainerBoundsFromChildBounds(
    parentBounds,
    childCanvasBounds,
    gap,
    flexDirection,
  )
}

export interface GridGapData {
  row: CSSNumberWithRenderedValue
  column: CSSNumberWithRenderedValue
}

export function maybeGridGapData(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): GridGapData | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (
    element == null ||
    element.specialSizeMeasurements.display !== 'grid' ||
    isLeft(element.element) ||
    !isJSXElement(element.element.value)
  ) {
    return null
  }

  const rowGap = element.specialSizeMeasurements.rowGap ?? element.specialSizeMeasurements.gap ?? 0
  const rowGapFromProps: CSSNumber | undefined = defaultEither(
    undefined,
    getLayoutProperty('rowGap', right(element.element.value.props), styleStringInArray),
  )

  const columnGap =
    element.specialSizeMeasurements.columnGap ?? element.specialSizeMeasurements.gap ?? 0
  const columnGapFromProps: CSSNumber | undefined = defaultEither(
    undefined,
    getLayoutProperty('columnGap', right(element.element.value.props), styleStringInArray),
  )

  return {
    row: { renderedValuePx: rowGap, value: rowGapFromProps ?? null },
    column: { renderedValuePx: columnGap, value: columnGapFromProps ?? null },
  }
}

export interface FlexGapData {
  value: CSSNumberWithRenderedValue
  direction: FlexDirection
}

export function maybeFlexGapData(
  info: StyleInfo | null,
  element: ElementInstanceMetadata | null,
): FlexGapData | null {
  if (
    element == null ||
    element.specialSizeMeasurements.display !== 'flex' ||
    isLeft(element.element) ||
    !isJSXElement(element.element.value) ||
    info == null
  ) {
    return null
  }

  if (element.specialSizeMeasurements.justifyContent?.startsWith('space')) {
    return null
  }

  const gap = element.specialSizeMeasurements.gap ?? 0
  const gapFromReader = info.gap?.value

  const flexDirection = element.specialSizeMeasurements.flexDirection ?? 'row'

  return {
    value: {
      renderedValuePx: gap,
      value: gapFromReader ?? null,
    },
    direction: flexDirection,
  }
}

export function recurseIntoChildrenOfMapOrFragment(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  target: ElementPath,
): Array<ElementInstanceMetadata> {
  const subTree = getSubTree(pathTrees, target)
  if (subTree == null) {
    return []
  }

  return getElementPathTreeChildren(subTree).flatMap((element) => {
    const elementPath = element.path
    if (EP.isRootElementOfInstance(elementPath)) {
      return []
    }

    const isMap = MetadataUtils.isJSXMapExpression(elementPath, metadata)
    const isFragmentLike = treatElementAsFragmentLike(
      metadata,
      allElementProps,
      pathTrees,
      elementPath,
      'sizeless-div-not-considered-fragment-like',
    )

    if (isFragmentLike || isMap) {
      return recurseIntoChildrenOfMapOrFragment(metadata, allElementProps, pathTrees, elementPath)
    }
    const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
    if (instance == null) {
      return []
    }

    return [instance]
  })
}

export type Axis = 'row' | 'column'
