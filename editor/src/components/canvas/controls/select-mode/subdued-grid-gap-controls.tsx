import React from 'react'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import type { Axis } from '../../gap-utils'
import { gridGapControlBoundsFromMetadata, maybeGridGapData } from '../../gap-utils'
import type { ElementPath } from 'utopia-shared/src/types'
import { useGridData } from '../grid-controls'
import { fallbackEmptyValue } from './controls-common'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import type { CSSNumber } from '../../../../components/inspector/common/css-utils'

export interface SubduedGridGapControlProps {
  hoveredOrFocused: 'hovered' | 'focused'
  axis: Axis | 'both'
}

export const SubduedGridGapControl = React.memo<SubduedGridGapControlProps>((props) => {
  const { hoveredOrFocused, axis } = props
  const targets = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'SubduedGridGapControl selectedViews',
  )

  const metadata = useRefEditorState((store) => store.editor.jsxMetadata)
  const selectedElement = targets.at(0)

  const gridRowColumnInfo = useGridData(targets)
  const selectedGrid = gridRowColumnInfo.at(0)

  const filteredGaps = React.useMemo(() => {
    if (selectedElement == null || selectedGrid == null) {
      return []
    }
    const gridGap = maybeGridGapData(metadata.current, selectedElement)
    if (gridGap == null) {
      return []
    }

    const gridGapRow = gridGap.row
    const gridGapColumn = gridGap.column

    const controlBounds = gridGapControlBoundsFromMetadata(selectedGrid, {
      row: fallbackEmptyValue(gridGapRow),
      column: fallbackEmptyValue(gridGapColumn),
    })
    return controlBounds.gaps.filter((gap) => gap.axis === axis || axis === 'both')
  }, [axis, metadata, selectedElement, selectedGrid])

  if (filteredGaps.length === 0 || selectedElement == null) {
    return null
  }

  return (
    <>
      {filteredGaps.map((gap) => (
        <GridGapControl
          key={gap.gapId}
          targets={targets}
          selectedElement={selectedElement}
          hoveredOrFocused={hoveredOrFocused}
          gap={gap}
        />
      ))}
    </>
  )
})

function GridGapControl({
  targets,
  selectedElement,
  hoveredOrFocused,
  gap,
}: {
  targets: Array<ElementPath>
  selectedElement: ElementPath
  hoveredOrFocused: 'hovered' | 'focused'
  gap: {
    bounds: CanvasRectangle
    gapId: string
    gap: CSSNumber
    axis: Axis
  }
}) {
  const metadata = useRefEditorState((store) => store.editor.jsxMetadata)
  const scale = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scale,
    'GridGapControl scale',
  )
  const gridRowColumnInfo = useGridData([selectedElement])

  const sideRef = useBoundingBox([selectedElement], (ref, parentBoundingBox) => {
    const gridGap = maybeGridGapData(metadata.current, selectedElement)
    const selectedGrid = gridRowColumnInfo.at(0)
    if (gridGap == null || selectedGrid == null) {
      return
    }

    const controlBounds = gridGapControlBoundsFromMetadata(selectedGrid, {
      row: fallbackEmptyValue(gridGap.row),
      column: fallbackEmptyValue(gridGap.column),
    })

    const bound = controlBounds.gaps.find((updatedGap) => updatedGap.gapId === gap.gapId)
    if (bound == null) {
      return
    }

    ref.current.style.display = 'block'
    ref.current.style.left = `${bound.bounds.x + parentBoundingBox.x}px`
    ref.current.style.top = `${bound.bounds.y + parentBoundingBox.y}px`
    ref.current.style.height = numberToPxValue(bound.bounds.height)
    ref.current.style.width = numberToPxValue(bound.bounds.width)
  })

  const color = useColorTheme().brandNeonPink.value

  const solidOrDashed = hoveredOrFocused === 'focused' ? 'solid' : 'dashed'

  return (
    <CanvasOffsetWrapper>
      <div
        ref={sideRef}
        style={{
          position: 'absolute',
          border: `1px ${solidOrDashed} ${color}`,
        }}
        data-testid={getSubduedGridGaplTestID(hoveredOrFocused)}
      />
    </CanvasOffsetWrapper>
  )
}

export function getSubduedGridGaplTestID(hoveredOrFocused: 'hovered' | 'focused'): string {
  return `SubduedGridGapControl-${hoveredOrFocused}`
}

const numberToPxValue = (n: number) => n + 'px'
