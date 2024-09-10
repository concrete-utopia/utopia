import React from 'react'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import type { EdgePiece } from '../../canvas-types'
import { paddingPropForEdge, simplePaddingFromMetadata } from '../../padding-utils'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

export interface SubduedPaddingControlProps {
  side: EdgePiece
  hoveredOrFocused: 'hovered' | 'focused'
}

export const SubduedPaddingControl = React.memo<SubduedPaddingControlProps>((props) => {
  const { side, hoveredOrFocused } = props
  const targets = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'SubduedPaddingControl selectedViews',
  )

  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'SubduedPaddingControl projectContents',
  )

  const elementMetadata = useRefEditorState((store) => store.editor.jsxMetadata)

  const isHorizontalPadding = side === 'left' || side === 'right'
  const isVerticalPadding = !isHorizontalPadding
  const paddingKey = paddingPropForEdge(side)

  // TODO Multiselect
  const sideRef = useBoundingBox(targets, (ref, boundingBox) => {
    const padding = simplePaddingFromMetadata(projectContents, elementMetadata.current, targets[0])
    const paddingValue = padding[paddingKey]?.renderedValuePx ?? 0

    const { x, y, width, height } = boundingBox
    const left = side === 'right' ? x + width - paddingValue : x
    const top = side === 'bottom' ? y + height - paddingValue : y

    ref.current.style.display = 'block'
    ref.current.style.left = `${left}px`
    ref.current.style.top = `${top}px`
    ref.current.style.height = isVerticalPadding
      ? numberToPxValue(paddingValue)
      : numberToPxValue(boundingBox.height)
    ref.current.style.width = isHorizontalPadding
      ? numberToPxValue(paddingValue)
      : numberToPxValue(boundingBox.width)
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
        data-testid={getSubduedPaddingControlTestID(side, hoveredOrFocused)}
      />
    </CanvasOffsetWrapper>
  )
})

export function getSubduedPaddingControlTestID(
  side: EdgePiece,
  hoveredOrFocused: 'hovered' | 'focused',
): string {
  return `SubduedPaddingControl-${side}-${hoveredOrFocused}`
}

const numberToPxValue = (n: number) => n + 'px'
