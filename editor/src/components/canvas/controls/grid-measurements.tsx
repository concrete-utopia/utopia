import type { Property } from 'csstype'
import type { CSSProperties } from 'react'
import type { GridIdentifier } from '../../editor/store/editor-state'
import type {
  BorderWidths,
  GridAutoOrTemplateBase,
  GridElementProperties,
} from '../../../core/shared/element-template'
import type { CanvasRectangle } from '../../../core/shared/math-utils'
import type { Sides } from 'utopia-api/core'
import { sides } from 'utopia-api/core'
import type { ElementPath } from 'utopia-shared/src/types'
import { getFromElement } from '../direct-dom-lookups'
import { CanvasContainerID } from '../canvas-types'
import {
  applicativeSidesPxTransform,
  getGridContainerProperties,
  getGridElementProperties,
} from '../dom-walker'
import { isStaticGridRepeat, parseCSSLength } from '../../inspector/common/css-utils'
import { assertNever } from '../../../core/shared/utils'
import { applicative4Either, defaultEither, isRight, mapEither } from '../../../core/shared/either'
import { domRectToScaledCanvasRectangle, getRoundingFn } from '../../../core/shared/dom-utils'
import Utils from '../../../utils/utils'
import React from 'react'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { addChangeCallback, removeChangeCallback } from '../observers'
import { useMonitorChangesToElements } from '../../editor/store/store-monitor'
import { useKeepReferenceEqualityIfPossible } from '../../../utils/react-performance'

export type GridElementMeasurementHelperData = {
  frame: CanvasRectangle
  computedStyling: CSSProperties
  gridElementProperties: GridElementProperties
}

export type ElementOrParent = 'parent' | 'element'

export type GridMeasurementHelperData = {
  frame: CanvasRectangle
  rows: number
  columns: number
  cells: number
  computedStyling: CSSProperties
  gridTemplateColumns: GridAutoOrTemplateBase | null
  gridTemplateRows: GridAutoOrTemplateBase | null
  gridTemplateColumnsFromProps: GridAutoOrTemplateBase | null
  gridTemplateRowsFromProps: GridAutoOrTemplateBase | null
  border: BorderWidths
  gap: number | null
  rowGap: number | null
  columnGap: number | null
  padding: Sides
  element: HTMLElement
}

export type GridData = GridMeasurementHelperData & {
  identifier: GridIdentifier
}

function getPositionFromComputedStyling(position: string): Property.Position | undefined {
  switch (position) {
    case '-moz-initial':
    case 'inherit':
    case 'initial':
    case 'revert':
    case 'unset':
    case '-webkit-sticky':
    case 'absolute':
    case 'fixed':
    case 'relative':
    case 'static':
    case 'sticky':
      return position
    default:
      return undefined
  }
}

function getGridElementStylingSubset(styling: CSSStyleDeclaration): CSSProperties {
  // Fields chosen to not overlap with any others, so as to not make React complain.
  return {
    position: getPositionFromComputedStyling(styling.position),
    gridColumnStart: styling.gridColumnStart,
    gridColumnEnd: styling.gridColumnEnd,
    gridRowStart: styling.gridRowStart,
    gridRowEnd: styling.gridRowEnd,
    borderRadius: styling.borderRadius,
  }
}

function getGlobalFrame(
  canvasRootContainer: HTMLElement,
  targetElement: HTMLElement,
  scale: number,
): CanvasRectangle {
  const boundingRectangle = targetElement.getBoundingClientRect()
  const elementRect = domRectToScaledCanvasRectangle(
    boundingRectangle,
    1 / scale,
    getRoundingFn('nearest-half'),
  )
  const parentRect = domRectToScaledCanvasRectangle(
    canvasRootContainer.getBoundingClientRect(),
    1 / scale,
    getRoundingFn('nearest-half'),
  )
  return Utils.offsetRect(elementRect, Utils.negate(parentRect))
}

export function gridElementMeasurementHelperDataFromElement(
  scale: number,
): (element: HTMLElement) => GridElementMeasurementHelperData | undefined {
  return (element) => {
    const canvasRootContainer = document.getElementById(CanvasContainerID)
    if (canvasRootContainer == null) {
      return undefined
    }

    const computedStyle = getComputedStyle(element)
    const computedStyling: CSSProperties = getGridElementStylingSubset(computedStyle)

    const frame = getGlobalFrame(canvasRootContainer, element, scale)

    const parentElementStyle =
      element.parentElement == null ? null : getComputedStyle(element.parentElement)
    const gridContainerProperties = getGridContainerProperties(parentElementStyle)
    const gridElementProperties = getGridElementProperties(gridContainerProperties, element.style)

    return {
      frame: frame,
      computedStyling: computedStyling,
      gridElementProperties: gridElementProperties,
    }
  }
}

export function getGridElementMeasurementHelperData(
  elementPath: ElementPath,
  scale: number,
): GridElementMeasurementHelperData | undefined {
  return getFromElement(elementPath, gridElementMeasurementHelperDataFromElement(scale), 'element')
}

export function getGridMeasurementHelperData(
  elementPath: ElementPath,
  scale: number,
  source: ElementOrParent,
): GridMeasurementHelperData | undefined {
  return getFromElement(elementPath, gridMeasurementHelperDataFromElement(scale), source)
}

function getGridStylingSubset(styling: CSSStyleDeclaration): CSSProperties {
  // Fields chosen to not overlap with any others, so as to not make React complain.
  return {
    gridAutoFlow: styling.gridAutoFlow,
    gridAutoColumns: styling.gridAutoColumns,
    gridAutoRows: styling.gridAutoRows,
    gridTemplateColumns: styling.gridTemplateColumns,
    gridTemplateRows: styling.gridTemplateRows,
    gridColumn: styling.gridColumn,
    gridRow: styling.gridRow,
    gap: styling.gap,
    rowGap: styling.rowGap,
    columnGap: styling.columnGap,
    justifyContent: styling.justifyContent,
    alignContent: styling.alignContent,
    padding: styling.padding,
    paddingTop: styling.paddingTop,
    paddingLeft: styling.paddingLeft,
    paddingBottom: styling.paddingBottom,
    paddingRight: styling.paddingRight,
    borderTop: styling.borderTopWidth,
    borderLeft: styling.borderLeftWidth,
    borderBottom: styling.borderBottomWidth,
    borderRight: styling.borderRightWidth,
  }
}

function getCellsCount(template: GridAutoOrTemplateBase | null): number {
  if (template == null) {
    return 0
  }

  switch (template.type) {
    case 'DIMENSIONS':
      return template.dimensions.reduce((acc, cur) => {
        return acc + (isStaticGridRepeat(cur) ? cur.times : 1)
      }, 0)
    case 'FALLBACK':
      return 0
    default:
      assertNever(template)
  }
}

export function gridMeasurementHelperDataFromElement(
  scale: number,
): (element: HTMLElement) => GridMeasurementHelperData | undefined {
  return (element) => {
    const canvasRootContainer = document.getElementById(CanvasContainerID)
    if (canvasRootContainer == null) {
      return undefined
    }

    const computedStyle = getComputedStyle(element)

    const computedStyling: CSSProperties = getGridStylingSubset(computedStyle)

    const containerGridProperties = getGridContainerProperties(computedStyle)
    const containerGridPropertiesFromProps = getGridContainerProperties(element.style)

    const columns = getCellsCount(containerGridProperties.gridTemplateColumns)
    const rows = getCellsCount(containerGridProperties.gridTemplateRows)
    const borderTopWidth = parseCSSLength(computedStyle.borderTopWidth)
    const borderRightWidth = parseCSSLength(computedStyle.borderRightWidth)
    const borderBottomWidth = parseCSSLength(computedStyle.borderBottomWidth)
    const borderLeftWidth = parseCSSLength(computedStyle.borderLeftWidth)
    const border: BorderWidths = {
      top: isRight(borderTopWidth) ? borderTopWidth.value.value : 0,
      right: isRight(borderRightWidth) ? borderRightWidth.value.value : 0,
      bottom: isRight(borderBottomWidth) ? borderBottomWidth.value.value : 0,
      left: isRight(borderLeftWidth) ? borderLeftWidth.value.value : 0,
    }
    const padding = defaultEither(
      sides(undefined, undefined, undefined, undefined),
      applicative4Either(
        applicativeSidesPxTransform,
        parseCSSLength(computedStyle.paddingTop),
        parseCSSLength(computedStyle.paddingRight),
        parseCSSLength(computedStyle.paddingBottom),
        parseCSSLength(computedStyle.paddingLeft),
      ),
    )
    const gap = defaultEither(
      null,
      mapEither((n) => n.value, parseCSSLength(computedStyle.gap)),
    )

    const rowGap = defaultEither(
      null,
      mapEither((n) => n.value, parseCSSLength(computedStyle.rowGap)),
    )

    const columnGap = defaultEither(
      null,
      mapEither((n) => n.value, parseCSSLength(computedStyle.columnGap)),
    )

    const frame = getGlobalFrame(canvasRootContainer, element, scale)

    return {
      frame: frame,
      gridTemplateColumns: containerGridProperties.gridTemplateColumns,
      gridTemplateRows: containerGridProperties.gridTemplateRows,
      gridTemplateColumnsFromProps: containerGridPropertiesFromProps.gridTemplateColumns,
      gridTemplateRowsFromProps: containerGridPropertiesFromProps.gridTemplateRows,
      border: border,
      padding: padding,
      gap: gap,
      rowGap: rowGap,
      columnGap: columnGap,
      rows: rows,
      columns: columns,
      cells: columns * rows,
      computedStyling: computedStyling,
      element: element,
    }
  }
}

export function useObserversToWatch(elementPathOrPaths: Array<ElementPath> | ElementPath): number {
  // Used to trigger extra renders.
  const [counter, setCounter] = React.useState(0)
  const bumpCounter = React.useCallback(() => {
    setCounter((value) => value + 1)
  }, [])

  // Need to use the mount count for the callback trigger.
  const mountCount = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.mountCount,
    'useObserversToWatch mountCount',
  )

  React.useEffect(() => {
    // Add the change callback(s) for the element path or paths.
    if (Array.isArray(elementPathOrPaths)) {
      for (const elementPath of elementPathOrPaths) {
        addChangeCallback(mountCount, elementPath, bumpCounter)
      }
    } else {
      addChangeCallback(mountCount, elementPathOrPaths, bumpCounter)
    }

    return function cleanup() {
      if (Array.isArray(elementPathOrPaths)) {
        for (const elementPath of elementPathOrPaths) {
          removeChangeCallback(elementPath, bumpCounter)
        }
      } else {
        removeChangeCallback(elementPathOrPaths, bumpCounter)
      }
    }
  }, [mountCount, elementPathOrPaths, bumpCounter])

  return counter
}

export function useGridMeasurementHelperData(
  elementPath: ElementPath,
  source: ElementOrParent,
): GridMeasurementHelperData | undefined {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'useGridMeasurementHelperData scale',
  )

  useMonitorChangesToElements([elementPath])

  useObserversToWatch(elementPath)

  return useKeepReferenceEqualityIfPossible(
    getGridMeasurementHelperData(elementPath, scale, source),
  )
}

export function useGridElementMeasurementHelperData(
  elementPath: ElementPath,
): GridElementMeasurementHelperData | undefined {
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'useGridMeasurementHelperData scale',
  )

  useMonitorChangesToElements([elementPath])

  useObserversToWatch(elementPath)

  return useKeepReferenceEqualityIfPossible(getGridElementMeasurementHelperData(elementPath, scale))
}
