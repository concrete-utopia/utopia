import React from 'react'
import { windowPoint } from '../../core/shared/math-utils'
import type { ElementOutsideVisibleAreaIndicator } from '../canvas/controls/elements-outside-visible-area-hooks'
import {
  getIndicatorAngleToTarget,
  useElementsOutsideVisibleArea,
} from '../canvas/controls/elements-outside-visible-area-hooks'
import { useColorTheme } from '../../uuiui'

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
      return getIndicatorAngleToTarget(
        windowPoint({ x: mouse.x + 20, y: mouse.y }),
        target.position,
      )
    }, [mouse, target])

    React.useEffect(() => {
      function handle(e: MouseEvent) {
        setMouse(windowPoint({ x: e.clientX, y: e.clientY }))
      }
      window.addEventListener('mousemove', handle)
      window.addEventListener('wheel', handle)
      return function () {
        window.removeEventListener('mousemove', handle)
        window.removeEventListener('wheel', handle)
      }
    }, [])

    const colorTheme = useColorTheme()

    if (target == null) {
      return null
    }

    return (
      <div
        id={ToolbarIndicatorElementsOutsideVisibleAreaId}
        data-testid={ToolbarIndicatorElementsOutsideVisibleAreaId}
        style={{
          transform: `rotate(${angle}rad)`,
          fontSize: 12,
          fontWeight: 800,
          opacity: target.selected ? 1 : 0.5,
          position: 'fixed',
          left: mouse.x + 3,
          top: mouse.y + 14,
          color: colorTheme.primary.value,
        }}
      >
        <img
          src='/editor/icons/light/semantic/arrowOutOfBounds@2x.png'
          style={{
            width: 14,
            height: 14,
          }}
        />
      </div>
    )
  },
)

IndicatorArrow.displayName = 'IndicatorArrow'
