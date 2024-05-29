import React from 'react'
import type { WindowPoint } from '../../core/shared/math-utils'
import { windowPoint } from '../../core/shared/math-utils'
import { SquareButton, Tooltip, useColorTheme } from '../../uuiui'
import type { ElementOutsideVisibleAreaIndicator } from '../canvas/controls/elements-outside-visible-area-hooks'
import {
  getIndicatorAngleToTarget,
  useElementsOutsideVisibleArea,
} from '../canvas/controls/elements-outside-visible-area-hooks'
import { scrollToPosition } from './actions/action-creators'
import { useDispatch } from './store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from './store/store-hook'
import { isFollowMode } from './editor-modes'

export const ToolbarIndicatorElementsOutsideVisibleAreaId =
  'indicator-elements-outside-visible-area'

export const ElementsOutsideVisibleAreaIndicator = React.memo(() => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const target = useElementsOutsideVisibleArea()

  const mode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode,
    'ElementsOutsideVisibleAreaIndicator mode',
  )

  const scrollTo = React.useCallback(() => {
    if (target != null && !isFollowMode(mode)) {
      dispatch([scrollToPosition(target.rect, 'to-center')])
    }
  }, [dispatch, target, mode])

  if (target == null) {
    return null
  }

  return (
    <Tooltip title={`Scroll to element${target.elements > 1 ? 's' : ''}`} placement='bottom'>
      <SquareButton
        highlight
        style={{
          textAlign: 'center',
          width: 'min-content',
          padding: '0 8px 0px 2px',
          position: 'relative',
          color: colorTheme.dynamicBlue.value,
        }}
        onClick={scrollTo}
      >
        <IndicatorArrow target={target} />
      </SquareButton>
    </Tooltip>
  )
})

ElementsOutsideVisibleAreaIndicator.displayName = 'ElementsOutsideVisibleAreaIndicator'

const IndicatorArrow = React.memo(({ target }: { target: ElementOutsideVisibleAreaIndicator }) => {
  const ref = React.useRef<HTMLDivElement | null>(null)
  const canvasScale = useRefEditorState((store) => store.editor.canvas.scale)

  const [angle, setAngle] = React.useState<number>(0)

  React.useEffect(() => {
    // useEffect to make sure the origin point is calculated after the arrow has been rendered
    if (ref.current != null) {
      const rect = ref.current.getBoundingClientRect()
      const newAngle = getIndicatorAngleToTarget(
        getIndicatorOriginPoint(rect, canvasScale.current),
        target.position,
      )
      setAngle(newAngle)
    }
  }, [canvasScale, target])

  return (
    <div
      id={ToolbarIndicatorElementsOutsideVisibleAreaId}
      data-testid={ToolbarIndicatorElementsOutsideVisibleAreaId}
      ref={ref}
      style={{
        transform: `rotate(${angle}rad)`,
        fontSize: 14,
        fontWeight: 800,
        opacity: target.selected ? 1 : 0.5,
      }}
    >
      ←
    </div>
  )
})

IndicatorArrow.displayName = 'IndicatorArrow'

function getIndicatorOriginPoint(rect: DOMRect, canvasScale: number): WindowPoint {
  return windowPoint({
    x: (rect.x + rect.width / 2) * canvasScale,
    y: (rect.y + rect.height / 2) * canvasScale,
  })
}
