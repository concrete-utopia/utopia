import React from 'react'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { controlForStrategyMemoized } from '../../canvas-strategies/canvas-strategy-types'
import { useBoundingBox } from '../bounding-box-hooks'
import { CanvasOffsetWrapper } from '../canvas-offset-wrapper'
import { isZeroSizedElement } from '../outline-utils'
import { useHoverWithDelay } from './controls-common'

type ShowHideControlProps = React.PropsWithChildren<{ selectedElements: ElementPath[] }>

export const ShowHideControl = controlForStrategyMemoized<ShowHideControlProps>((props) => {
  const [hoverHidden, setHoverHidden] = React.useState<boolean>(true)
  const [hoverStart, hoverEnd] = useHoverWithDelay(200, (h) => {
    setHoverHidden(!h)
  })

  const controlRef = useBoundingBox(props.selectedElements, (ref, boundingBox) => {
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
        ref={controlRef}
        style={{ position: 'absolute', visibility: hoverHidden ? 'hidden' : 'visible' }}
        onMouseEnter={(e) => {
          hoverStart(e)
        }}
        onMouseLeave={hoverEnd}
      >
        {props.children}
      </div>
    </CanvasOffsetWrapper>
  )
})
