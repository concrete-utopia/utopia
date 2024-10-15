import type { CSSProperties } from 'react'
import React from 'react'
import { createArrayWithLength, interleaveArray } from '../../../../core/shared/array-utils'
import type { GridAutoOrTemplateBase } from '../../../../core/shared/element-template'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import {
  printGridAutoOrTemplateBase,
  stringifyGridDimension,
} from '../../../inspector/common/css-utils'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import type { GridData } from '../grid-controls-for-strategies'
import { getNullableAutoOrTemplateBaseString, useGridData } from '../grid-controls-for-strategies'
import { getStyleMatchingTargetGrid } from '../grid-controls-helpers'
import type { GridGapControlProps } from './grid-gap-control-component'
import { NO_OP } from '../../../../core/shared/utils'

export const GridGapControlComponent2 = React.memo<GridGapControlProps>((props) => {
  const { selectedElement, updatedGapValueRow, updatedGapValueColumn } = props

  const grid = useGridData([selectedElement]).at(0)

  if (grid == null) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      <GridPaddingOutlineForDimension grid={grid} dimension={'rows'} />
      <GridPaddingOutlineForDimension grid={grid} dimension={'columns'} />
    </CanvasOffsetWrapper>
  )
})

const GridPaddingOutlineForDimension = (props: {
  grid: GridData
  dimension: 'rows' | 'columns'
}) => {
  const { grid, dimension } = props

  let style: CSSProperties = {
    ...getStyleMatchingTargetGrid(grid),
    gap: undefined,
    rowGap: undefined,
    columnGap: undefined,
    gridTemplateRows:
      dimension === 'rows'
        ? tweakTrackListByInsertingGap(grid.gridTemplateRows, grid.rowGap ?? grid.gap)
        : '1fr',
    gridTemplateColumns:
      dimension === 'columns'
        ? tweakTrackListByInsertingGap(grid.gridTemplateColumns, grid.columnGap ?? grid.gap)
        : '1fr',
  }

  const length = 2 * (dimension === 'rows' ? grid.rows : grid.columns) - 1

  return (
    <div style={style}>
      {createArrayWithLength(length, (index) => {
        return (
          <GridRowHighlight
            key={index}
            hide={index === 0 || index === length - 1 || index % 2 === 0} // we only want to show the divs that fall in where the gaps are in the original grid
            gapId={`${dimension}-${index}`}
            onGapHover={NO_OP}
            onHandleHoverEndInner={NO_OP}
            axis={dimension === 'rows' ? 'row' : 'column'}
            template={getNullableAutoOrTemplateBaseString(
              dimension === 'rows' ? grid.gridTemplateColumns : grid.gridTemplateRows,
            )}
            numberOfHandles={dimension === 'rows' ? grid.columns : grid.rows}
            gap={dimension === 'rows' ? grid.rowGap ?? grid.gap : grid.columnGap ?? grid.gap}
          />
        )
      })}
    </div>
  )
}

const GridRowHighlight = (props: {
  gapId: string
  onGapHover: () => void
  onHandleHoverEndInner: () => void
  hide: boolean
  axis: 'row' | 'column'
  template: string | undefined
  numberOfHandles: number
  gap: number | null
}) => {
  const { gapId, onGapHover, onHandleHoverEndInner, hide, axis, template, gap, numberOfHandles } =
    props

  const colorTheme = useColorTheme()
  const canvasScale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridRowHighlight canvasScale',
  )

  const lineWidth = 1 / canvasScale

  return (
    <div
      key={gapId}
      onMouseEnter={onGapHover}
      onMouseLeave={onHandleHoverEndInner}
      data-testid={`gap-control-segment-${gapId}`}
      style={{
        display: 'grid',
        position: 'relative',
        pointerEvents: 'all',
        boxShadow: `inset 0 0 0 ${lineWidth}px ${colorTheme.brandNeonOrange.value}`,
        opacity: hide ? 0 : 1,

        alignItems: 'center',
        justifyContent: 'center',
        placeItems: 'center',

        gap: gap ?? 0,
        gridTemplateColumns: axis === 'row' ? template : '1fr',
        gridTemplateRows: axis === 'column' ? template : '1fr',
      }}
    >
      {createArrayWithLength(numberOfHandles, (i) => (
        <div>{'<>'}</div>
      ))}
    </div>
  )
}

function tweakTrackListByInsertingGap(
  trackList: GridAutoOrTemplateBase | null,
  gap: number | null,
): string | undefined {
  if (trackList == null) {
    return undefined
  }

  if (trackList.type === 'FALLBACK') {
    throw new Error('Cannot insert gap into fallback')
  }

  if (gap == null) {
    return printGridAutoOrTemplateBase(trackList)
  }

  return interleaveArray(trackList.dimensions.map(stringifyGridDimension), `${gap}px`).join(' ')
}
