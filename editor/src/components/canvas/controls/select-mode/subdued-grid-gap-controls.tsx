import React from 'react'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import type { Axis } from '../../gap-utils'
import { useGridData } from '../grid-controls-for-strategies'
import { unitlessCSSNumberWithRenderedValue } from './controls-common'
import { NO_OP } from '../../../../core/shared/utils'
import * as EP from '../../../../core/shared/element-path'
import { GridPaddingOutlineForDimension } from './grid-gap-control-component'

export interface SubduedGridGapControlProps {
  hoveredOrFocused: 'hovered' | 'focused'
  axis: Axis | 'both'
}

export const SubduedGridGapControl = React.memo<SubduedGridGapControlProps>((props) => {
  const { hoveredOrFocused, axis } = props
  const colorTheme = useColorTheme()
  const targets = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'SubduedGridGapControl selectedViews',
  )

  const gridRowColumnInfo = useGridData(targets)

  if (gridRowColumnInfo.length === 0) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      {gridRowColumnInfo.map((gridData) => {
        return (
          <React.Fragment key={`grid-gap-${EP.toString(gridData.elementPath)}`}>
            <GridPaddingOutlineForDimension
              grid={gridData}
              dimension={'rows'}
              onMouseDown={NO_OP}
              beingDragged={true}
              onMouseOver={NO_OP}
              zIndexPriority={false}
              gridGap={unitlessCSSNumberWithRenderedValue(gridData.rowGap ?? 0)}
              elementHovered={hoveredOrFocused === 'hovered' && axis === 'row'}
              draggedOutlineColor={colorTheme.brandNeonPink}
            />
            <GridPaddingOutlineForDimension
              grid={gridData}
              dimension={'columns'}
              onMouseDown={NO_OP}
              beingDragged={true}
              onMouseOver={NO_OP}
              zIndexPriority={false}
              gridGap={unitlessCSSNumberWithRenderedValue(gridData.columnGap ?? 0)}
              elementHovered={hoveredOrFocused === 'hovered' && axis === 'column'}
              draggedOutlineColor={colorTheme.brandNeonPink}
            />
          </React.Fragment>
        )
      })}
    </CanvasOffsetWrapper>
  )
})
