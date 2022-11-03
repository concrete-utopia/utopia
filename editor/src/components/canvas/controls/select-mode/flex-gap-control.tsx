import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { useRefEditorState } from '../../../editor/store/store-hook'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'

interface FlexGapControlProps {
  selectedElement: ElementPath
}

export const FlexGapControlTestId = 'FlexGapControlTestId'

export const FlexGapControl = controlForStrategyMemoized<FlexGapControlProps>((props) => {
  const { selectedElement } = props

  const elementMetadata = useRefEditorState((store) => store.editor.jsxMetadata)

  const children = MetadataUtils.getChildrenPaths(elementMetadata.current, selectedElement)
  const childFrames = MetadataUtils.findElementsByElementPath(
    elementMetadata.current,
    children,
  ).map((c) => c.globalFrame)

  const controlRef = useBoundingBox([selectedElement], (ref, boundingBox) => {
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
      <div
        data-testid={FlexGapControlTestId}
        ref={controlRef}
        style={{
          position: 'absolute',
        }}
      >
        {/** TODO */}
      </div>
    </CanvasOffsetWrapper>
  )
})
