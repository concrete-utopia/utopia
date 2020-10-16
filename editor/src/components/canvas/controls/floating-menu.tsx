import * as React from 'react'
import { useRecoilState } from 'recoil'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { eitherToMaybe, isRight, right } from '../../../core/shared/either'
import { isJSXElement } from '../../../core/shared/element-template'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import { colorTheme } from '../../../uuiui'
import { ControlProps } from './new-canvas-controls'
import { layoutHoveredState } from '../../../core/shared/inspector-recoil'

export const FloatingMenu = (props: ControlProps) => {
  const [, setLayoutHovered] = useRecoilState(layoutHoveredState)
  if (!isFeatureEnabled('Floating Menu Warning') || props.selectedViews.length !== 1) {
    return null
  }

  const element = MetadataUtils.getElementByTemplatePathMaybe(
    props.componentMetadata,
    props.selectedViews[0],
  )
  if (element != null && isRight(element.element) && isJSXElement(element.element.value)) {
    const attributes = element.element.value.props
    const left = eitherToMaybe(getLayoutProperty('PinnedLeft', right(attributes)))
    const top = eitherToMaybe(getLayoutProperty('PinnedTop', right(attributes)))
    const styleRight = eitherToMaybe(getLayoutProperty('PinnedRight', right(attributes)))
    const bottom = eitherToMaybe(getLayoutProperty('PinnedBottom', right(attributes)))
    const centerX = eitherToMaybe(getLayoutProperty('PinnedCenterX', right(attributes)))
    const centerY = eitherToMaybe(getLayoutProperty('PinnedCenterY', right(attributes)))

    const elementHasPositionProps =
      left != null ||
      top != null ||
      styleRight != null ||
      bottom != null ||
      centerX != null ||
      centerY != null

    const layoutType = element?.specialSizeMeasurements.position
    const frame = MetadataUtils.getFrameInCanvasCoords(
      element.templatePath,
      props.componentMetadata,
    )

    if (
      elementHasPositionProps &&
      (layoutType == null || layoutType === 'static') &&
      frame != null
    ) {
      // show warning triangle
      return (
        <div
          style={{
            borderRadius: 5,
            boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor.o(50).value} 0px 0px ${
              1 / props.scale
            }px, ${colorTheme.canvasControlsSizeBoxShadowColor.o(21).value} 0px ${
              1 / props.scale
            }px ${2 / props.scale}px ${1 / props.scale}px`,
            height: 25,
            backgroundColor: '#f5f5f5',
            position: 'absolute',
            left: frame.x + props.canvasOffset.x,
            top: frame.y + props.canvasOffset.y - 25 - 8,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'flex-start',
          }}
          onMouseOver={() => {
            setLayoutHovered(true)
          }}
          onMouseOut={() => {
            setLayoutHovered(false)
          }}
        >
          <span
            style={{
              padding: '0 5px',
              color: colorTheme.primary.value,
            }}
          >
            ⚠️ Missing position from style.
          </span>
        </div>
      )
    } else {
      return null
    }
  } else {
    return null
  }
}
