import React from 'react'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import type { Axis } from '../../gap-utils'
import {
  gridGapControlBoundsFromMetadata,
  maybeGridGapData,
  recurseIntoChildrenOfMapOrFragment,
} from '../../gap-utils'
import type { ElementPath } from 'utopia-shared/src/types'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import { useGridData } from '../grid-controls'
import { fallbackEmptyValue } from './controls-common'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import type { CSSNumber } from '../../../../components/inspector/common/css-utils'

export interface SubduedGridGapControlProps {
  hoveredOrFocused: 'hovered' | 'focused'
}

export const SubduedGridGapControl = React.memo<SubduedGridGapControlProps>((props) => {
  const { hoveredOrFocused } = props
  const targets = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'SubduedGridGapControl selectedViews',
  )
  const metadata = useRefEditorState((store) => store.editor.jsxMetadata)
  const elementPathTrees = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementProps = useRefEditorState((store) => store.editor.allElementProps)
  const selectedElement = targets[0]

  const children = recurseIntoChildrenOfMapOrFragment(
    metadata.current,
    allElementProps.current,
    elementPathTrees.current,
    targets[0],
  )

  const gridRowColumnInfo = useGridData([selectedElement])

  const gridGap = maybeGridGapData(metadata.current, selectedElement)
  if (gridGap == null) {
    return null
  }

  const gridGapRow = gridGap.row
  const gridGapColumn = gridGap.column

  const controlBounds = gridGapControlBoundsFromMetadata(
    selectedElement,
    gridRowColumnInfo[0],
    {
      row: fallbackEmptyValue(gridGapRow),
      column: fallbackEmptyValue(gridGapColumn),
    },
    1,
  )

  return (
    <>
      {controlBounds.gaps.map((gap, i) => (
        <GridGapControl
          key={i}
          targets={targets}
          selectedElement={selectedElement}
          hoveredOrFocused={hoveredOrFocused}
          gap={gap}
          flexChildren={children}
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
  flexChildren,
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
  flexChildren: Array<ElementInstanceMetadata>
}) {
  const metadata = useRefEditorState((store) => store.editor.jsxMetadata)
  const gridRowColumnInfo = useGridData([selectedElement])

  const sideRef = useBoundingBox([selectedElement], (ref) => {
    const gridGap = maybeGridGapData(metadata.current, selectedElement)
    if (gridGap == null) {
      return
    }

    const controlBounds = gridGapControlBoundsFromMetadata(
      selectedElement,
      gridRowColumnInfo[0],
      {
        row: fallbackEmptyValue(gridGap.row),
        column: fallbackEmptyValue(gridGap.column),
      },
      1,
    )

    const bound = controlBounds.gaps.find((updatedGap) => updatedGap.gapId === gap.gapId)
    if (bound == null) {
      return
    }

    ref.current.style.display = 'block'
    ref.current.style.left = `${bound.bounds.x}px`
    ref.current.style.top = `${bound.bounds.y}px`
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
