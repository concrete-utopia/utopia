import React from 'react'
import { CanvasVector } from '../../../../core/shared/math-utils'
import { EditorStorePatched } from '../../../editor/store/editor-state'
import { useEditorState } from '../../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { EdgePiece } from '../../canvas-types'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'

interface PaddingResizeStepIndicatorProps {
  currentPaddingValue: number
  activeEdge: EdgePiece
  dragDelta: CanvasVector
}

export const PaddingResizeStepIndicator =
  controlForStrategyMemoized<PaddingResizeStepIndicatorProps>((props) => {
    const { currentPaddingValue, activeEdge, dragDelta } = props
    const selectedElements = useEditorState(
      (store: EditorStorePatched) => store.editor.selectedViews,
      'selectedElementsSelector selectedElements',
    )

    const controlRef = useBoundingBox(selectedElements, (ref, boundingBox) => {
      if (isZeroSizedElement(boundingBox)) {
        ref.current.style.display = 'none'
      } else {
        ref.current.style.display = 'block'
        ref.current.style.left = boundingBox.x + 'px'
        ref.current.style.top = boundingBox.y + 'px'
        ref.current.style.width = boundingBox.width + 'px'
        ref.current.style.height = boundingBox.height + 'px'
      }
    })

    return (
      <CanvasOffsetWrapper>
        <div ref={controlRef}>hello</div>
      </CanvasOffsetWrapper>
    )
  })
