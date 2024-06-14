import React from 'react'
import { windowPoint } from '../../core/shared/math-utils'
import type { ElementOutsideVisibleAreaIndicator } from '../canvas/controls/elements-outside-visible-area-hooks'
import {
  getIndicatorAngleToTarget,
  useElementsOutsideVisibleArea,
} from '../canvas/controls/elements-outside-visible-area-hooks'
import { Icn } from '../../uuiui'

export const ToolbarIndicatorElementsOutsideVisibleAreaId =
  'indicator-elements-outside-visible-area'

export const ElementsOutsideVisibleAreaIndicator = React.memo(() => {
  const target = useElementsOutsideVisibleArea()

  return <IndicatorArrow target={target} />
})

ElementsOutsideVisibleAreaIndicator.displayName = 'ElementsOutsideVisibleAreaIndicator'

const IndicatorArrow = React.memo(
  ({ target }: { target: ElementOutsideVisibleAreaIndicator | null }) => {
    const [mouse, setMouse] = React.useState(windowPoint({ x: 0, y: 0 }))

    const angle = React.useMemo(() => {
      if (target == null) {
        return null
      }
      return getIndicatorAngleToTarget(windowPoint({ x: mouse.x, y: mouse.y }), target.position)
    }, [mouse, target])

    React.useEffect(() => {
      function storeMousePosition(e: MouseEvent) {
        setMouse(windowPoint({ x: e.clientX, y: e.clientY }))
      }
      window.addEventListener('mousemove', storeMousePosition)
      window.addEventListener('wheel', storeMousePosition)
      return function () {
        window.removeEventListener('mousemove', storeMousePosition)
        window.removeEventListener('wheel', storeMousePosition)
      }
    }, [])

    if (target == null) {
      return null
    }

    return (
      <div
        id={ToolbarIndicatorElementsOutsideVisibleAreaId}
        data-testid={ToolbarIndicatorElementsOutsideVisibleAreaId}
        style={{
          transform: `rotate(${angle}rad)`,
          position: 'fixed',
          left: mouse.x + 3,
          top: mouse.y + 14,
        }}
      >
        <Icn category='semantic' type='arrow-out-of-bounds' width={12} height={12} color='white' />
      </div>
    )
  },
)

IndicatorArrow.displayName = 'IndicatorArrow'
