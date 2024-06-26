import React from 'react'
import { CanvasOffsetWrapper } from './canvas-offset-wrapper'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isFiniteRectangle, windowPoint } from '../../../core/shared/math-utils'
import { controlForStrategyMemoized } from '../canvas-strategies/canvas-strategy-types'
import { useDispatch } from '../../editor/store/dispatch-context'
import { createInteractionViaMouse, gridCellHandle } from '../canvas-strategies/interaction-state'
import CanvasActions from '../canvas-actions'
import { Modifier } from '../../../utils/modifiers'
import { windowToCanvasCoordinates } from '../dom-lookup'
import { motion } from 'framer-motion'

export const GridControls = controlForStrategyMemoized(() => {
  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'GridControls selectedViews',
  )

  const isActivelyDraggingCell = useEditorState(
    Substores.canvas,
    (store) =>
      store.editor.canvas.interactionSession != null &&
      store.editor.canvas.interactionSession.activeControl.type === 'GRID_CELL_HANDLE',
    '',
  )

  const jsxMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'GridControls jsxMetadata',
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

        const targetGridContainer = MetadataUtils.isGridLayoutedContainer(element)
          ? element
          : MetadataUtils.isGridLayoutedContainer(parent)
          ? parent
          : null

        if (
          targetGridContainer == null ||
          targetGridContainer.globalFrame == null ||
          !isFiniteRectangle(targetGridContainer.globalFrame)
        ) {
          return null
        }

        const gap = targetGridContainer.specialSizeMeasurements.gap
        const padding = targetGridContainer.specialSizeMeasurements.padding
        const gridTemplateColumns =
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateColumns
        const gridTemplateRows =
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateRows

        function getSillyCellsCount(template: string | null) {
          return template == null ? 0 : template.trim().split(/\s+/).length
        }
        const columns = getSillyCellsCount(
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateColumns,
        )
        const rows = getSillyCellsCount(
          targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateRows,
        )

        return {
          elementPath: targetGridContainer.elementPath,
          frame: targetGridContainer.globalFrame,
          gridTemplateColumns: gridTemplateColumns,
          gridTemplateRows: gridTemplateRows,
          gap: gap,
          padding: padding,
          cells: rows * columns,
        }
      }, store.editor.selectedViews)
    },
    'GridControls grids',
  )

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

  const dispatch = useDispatch()

  const canvasOffsetRef = useRefEditorState((store) => store.editor.canvas.roundedCanvasOffset)
  const scaleRef = useRefEditorState((store) => store.editor.canvas.scale)

  const startInteractionWithUid = React.useCallback(
    (uid: string) => (event: React.MouseEvent) => {
      const start = windowToCanvasCoordinates(
        scaleRef.current,
        canvasOffsetRef.current,
        windowPoint({ x: event.nativeEvent.x, y: event.nativeEvent.y }),
      )

      dispatch([
        CanvasActions.createInteractionSession(
          createInteractionViaMouse(
            start.canvasPositionRounded,
            Modifier.modifiersForEvent(event),
            gridCellHandle(uid),
            'zero-drag-not-permitted',
          ),
        ),
      ])
    },
    [canvasOffsetRef, dispatch, scaleRef],
  )

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
              //   backgroundColor: '#ff00ff09',
              display: 'grid',
              gridTemplateColumns: grid.gridTemplateColumns ?? undefined,
              gridTemplateRows: grid.gridTemplateRows ?? undefined,
              gap: grid.gap ?? 0,
              padding:
                grid.padding == null
                  ? 0
                  : `${grid.padding.top}px ${grid.padding.right}px ${grid.padding.bottom}px ${grid.padding.left}px`,
            }}
          >
            {placeholders.map((cell) => {
              return (
                <div
                  key={`grid-${index}-cell-${cell}`}
                  style={{
                    border: '1px solid #ff00ff66',
                    background: '#ff00ff06',
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
          <motion.div
            initial={{
              scale: 1.2,
            }}
            animate={{
              scale: 1.3,
              transition: {
                type: 'tween',
                repeatType: 'mirror',
                repeat: Infinity,
                duration: 0.5,
              },
            }}
            onMouseDown={startInteractionWithUid(EP.toUid(cell.elementPath))}
            key={`grid-cell-${EP.toString(cell.elementPath)}`}
            style={{
              position: 'absolute',
              top: cell.globalFrame.y,
              left: cell.globalFrame.x,
              width: cell.globalFrame.width,
              height: cell.globalFrame.height,
              backgroundColor: '#f0f',
              opacity: !isActivelyDraggingCell || isSelected ? 0 : 0.2,
            }}
          />
        )
      })}
    </CanvasOffsetWrapper>
  )
})
