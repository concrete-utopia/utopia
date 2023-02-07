import React from 'react'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { useColorTheme } from '../../../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'

interface SubduedBorderRadiusControlProps {
  hoveredOrFocused: 'hovered' | 'focused'
}

export const SubduedBorderRadiusControlTestId = (state: 'hovered' | 'focused'): string =>
  `SubduedBorderRadiusControl-${state}`

export const SubduedBorderRadiusControl = React.memo<SubduedBorderRadiusControlProps>((props) => {
  const { hoveredOrFocused } = props
  const targets = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'SubduedBorderRadiusControl  selectedViews',
  )
  const elementMetadata = useRefEditorState((store) => store.editor.jsxMetadata)

  // TODO Multiselect
  const sideRef = useBoundingBox(targets, (ref, boundingBox) => {
    const borderRadius = MetadataUtils.findElementByElementPath(elementMetadata.current, targets[0])
      ?.specialSizeMeasurements.borderRadius

    const { x, y, width, height } = boundingBox

    ref.current.style.display = 'block'
    ref.current.style.left = `${x}px`
    ref.current.style.top = `${y}px`
    ref.current.style.height = `${height}px`
    ref.current.style.width = `${width}px`
    ref.current.style.borderTopLeftRadius = `${borderRadius?.top ?? 0}px`
    ref.current.style.borderTopRightRadius = `${borderRadius?.right ?? 0}px`
    ref.current.style.borderBottomLeftRadius = `${borderRadius?.bottom ?? 0}px`
    ref.current.style.borderBottomRightRadius = `${borderRadius?.left ?? 0}px`
  })

  const solidOrDashed = hoveredOrFocused === 'focused' ? 'solid' : 'dashed'

  return (
    <CanvasOffsetWrapper>
      <div
        ref={sideRef}
        style={{
          position: 'absolute',
          border: `1px ${solidOrDashed} blue`,
        }}
        data-testid={SubduedBorderRadiusControlTestId(hoveredOrFocused)}
      />
    </CanvasOffsetWrapper>
  )
})
