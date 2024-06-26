import React from 'react'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isFiniteRectangle } from '../../../core/shared/math-utils'
import { isRight } from '../../../core/shared/either'
import { isJSXElement } from '../../../core/shared/element-template'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'

export const GridControls = controlForStrategyMemoized(() => {
  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    '',
  )
  const grids = useEditorState(
    Substores.metadataAndPropertyControlsInfo,
    (store) => {
      return mapDropNulls((view) => {
        const element = MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, view)
        const parent = MetadataUtils.findElementByElementPath(
          store.editor.jsxMetadata,
          EP.parentPath(view),
        )

        const target = MetadataUtils.isGridLayoutedContainer(element)
          ? element
          : MetadataUtils.isGridLayoutedContainer(parent)
          ? parent
          : null

        if (
          target == null ||
          target.globalFrame == null ||
          !isFiniteRectangle(target.globalFrame)
        ) {
          return null
        }

        let rows = 0
        let columns = 0
        let gridTemplateColumns: any = undefined
        let gridTemplateRows: any = undefined
        let gap: any = 0
        let padding: any = 0
        // TODO well this whole logic piece is garbage, we should have these in the SpecialSizeMeasurements
        if (isRight(target.element) && isJSXElement(target.element.value)) {
          for (const prop of target.element.value.props) {
            if (
              prop.type === 'JSX_ATTRIBUTES_ENTRY' &&
              prop.key === 'style' &&
              prop.value.type === 'ATTRIBUTE_NESTED_OBJECT'
            ) {
              for (const entry of prop.value.content) {
                if (
                  entry.type === 'PROPERTY_ASSIGNMENT' &&
                  entry.value.type === 'ATTRIBUTE_VALUE'
                ) {
                  if (typeof entry.value.value === 'string') {
                    // TODO do something about it…
                    const count = entry.value.value.trim().split(/\s+/).length
                    if (entry.key === 'gridTemplateColumns') {
                      columns = count
                      gridTemplateColumns = entry.value.value
                    }
                    if (entry.key === 'gridTemplateRows') {
                      rows = count
                      gridTemplateRows = entry.value.value
                    }
                  }
                  if (entry.key === 'gap' || entry.key === 'gridGap') {
                    gap = entry.value.value
                  }
                  if (entry.key === 'padding') {
                    padding = entry.value.value
                  }
                }
              }
            }
          }
        }
        return {
          elementPath: target.elementPath,
          frame: target.globalFrame,
          gridTemplateColumns: gridTemplateColumns,
          gridTemplateRows: gridTemplateRows,
          gap: gap,
          padding: padding,
          cells: rows * columns,
        }
      }, store.editor.selectedViews)
    },
    'GridControls selectedGrids',
  )

  const jsxMetadata = useEditorState(Substores.metadata, (store) => store.editor.jsxMetadata, '')

  const cells = React.useMemo(() => {
    return grids.flatMap((grid) => {
      const children = MetadataUtils.getChildrenUnordered(jsxMetadata, grid.elementPath)
      return mapDropNulls((cell) => {
        if (cell == null || cell.globalFrame == null || !isFiniteRectangle(cell.globalFrame)) {
          return null
        }
        return { elementPath: cell.elementPath, globalFrame: cell.globalFrame }
      }, children)
    })
  }, [grids, jsxMetadata])

  if (grids.length === 0) {
    return null
  }

  return (
    <CanvasOffsetWrapper>
      {/* grid lines */}
      {grids.map((grid, index) => {
        const placeholders = Array.from(Array(grid.cells).keys())
        return (
          <div
            key={`grid-${index}`}
            style={{
              pointerEvents: 'none',
              position: 'absolute',
              top: grid.frame.y,
              left: grid.frame.x,
              width: grid.frame.width,
              height: grid.frame.height,
              backgroundColor: '#ff00ff0a',
              display: 'grid',
              gridTemplateColumns: grid.gridTemplateColumns,
              gridTemplateRows: grid.gridTemplateRows,
              gap: grid.gap,
              padding: grid.padding,
            }}
          >
            {placeholders.map((cell) => {
              return (
                <div
                  key={`grid-${index}-cell-${cell}`}
                  style={{
                    border: '1px solid #ff00ff66',
                  }}
                />
              )
            })}
          </div>
        )
      })}
      {/* cell targets */}
      {cells.map((cell) => {
        const isSelected = selectedViews.some((view) => EP.pathsEqual(cell.elementPath, view))
        return (
          <div
            key={`grid-cell-${EP.toString(cell.elementPath)}`}
            style={{
              position: 'absolute',
              top: cell.globalFrame.y,
              left: cell.globalFrame.x,
              width: cell.globalFrame.width,
              height: cell.globalFrame.height,
              backgroundColor: '#09f',
              opacity: !isSelected ? 0.3 : 0,
            }}
          />
        )
      })}
    </CanvasOffsetWrapper>
  )
})
