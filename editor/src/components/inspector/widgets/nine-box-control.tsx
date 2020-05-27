import * as R from 'ramda'
import * as React from 'react'
import { ControlProps } from '../controls/control'
import {
  CSSTransformOrigin,
  cssTransformOriginToNormalisedValue,
  normalisedCSSTransformOriginValueToCSSTransformValue,
} from '../new-inspector/css-utils'
import { UtopiaTheme } from 'uuiui'

export type NineBoxControlProps = ControlProps<CSSTransformOrigin>

const WIDTH = 22
const HEIGHT = 22
const INSETMARGIN = 4.5
const CIRCLESPREADX = WIDTH - INSETMARGIN * 2
const CIRCLESPREADY = HEIGHT - INSETMARGIN * 2

export function NineBoxControl(props: NineBoxControlProps) {
  const normalisedValue = cssTransformOriginToNormalisedValue(props.value)
  const showValue = !props.controlStyles.mixed

  let mouseDownOrigin = React.useRef({
    x: 0,
    y: 0,
  })

  const { onSubmitValue, onTransientSubmitValue } = props

  const setTransformOrigin = React.useCallback(
    (screenX: number, screenY: number, transient: boolean) => {
      const x = screenX - mouseDownOrigin.current.x
      const y = screenY - mouseDownOrigin.current.y
      const xNonant = Math.min(Math.max(Math.floor(x / (WIDTH / 3)), 0), 2)
      const yNonant = Math.min(Math.max(Math.floor(y / (HEIGHT / 3)), 0), 2)
      if (transient && onTransientSubmitValue != null) {
        onTransientSubmitValue({
          x: normalisedCSSTransformOriginValueToCSSTransformValue(xNonant / 2, 'x'),
          y: normalisedCSSTransformOriginValueToCSSTransformValue(yNonant / 2, 'y'),
        })
      } else {
        onSubmitValue({
          x: normalisedCSSTransformOriginValueToCSSTransformValue(xNonant / 2, 'x'),
          y: normalisedCSSTransformOriginValueToCSSTransformValue(yNonant / 2, 'y'),
        })
      }
    },
    [onSubmitValue, onTransientSubmitValue],
  )

  const onMouseMove = React.useCallback(
    (e: MouseEvent) => {
      setTransformOrigin(e.screenX, e.screenY, true)
    },
    [setTransformOrigin],
  )

  const onMouseUp = React.useCallback(() => {
    window.removeEventListener('mouseup', onMouseUp)
    window.removeEventListener('mousemove', onMouseMove)
  }, [onMouseMove])

  const onMouseDown = React.useCallback(
    (e: React.MouseEvent<SVGGElement>) => {
      window.addEventListener('mouseup', onMouseUp)
      window.addEventListener('mousemove', onMouseMove)
      mouseDownOrigin.current = {
        x: e.nativeEvent.screenX - e.nativeEvent.offsetX,
        y: e.nativeEvent.screenY - e.nativeEvent.offsetY,
      }
      setTransformOrigin(e.screenX, e.screenY, false)
    },
    [onMouseMove, onMouseUp, setTransformOrigin],
  )

  let circles: Array<JSX.Element> = new Array(9)
  for (let x = 0; x <= 1; x += 0.5) {
    for (let y = 0; y <= 1; y += 0.5) {
      const active = showValue && normalisedValue.x === x && normalisedValue.y === y
      circles.push(
        <circle
          key={x.toString() + y.toString()}
          cx={x * CIRCLESPREADX + INSETMARGIN}
          cy={y * CIRCLESPREADY + INSETMARGIN}
          r='1'
          fill={active ? props.controlStyles.mainColor : props.controlStyles.backgroundColor}
          stroke={active ? props.controlStyles.mainColor : props.controlStyles.secondaryColor}
          strokeWidth='1'
        />,
      )
    }
  }

  return (
    <div
      style={{
        borderRadius: `0 ${UtopiaTheme.inputBorderRadius}px ${UtopiaTheme.inputBorderRadius}px 0`,
        backgroundColor: `${props.controlStyles.backgroundColor}`,
        boxShadow: `-1px 0 0 ${props.controlStyles.borderColor} inset, 0 -1px 0 ${props.controlStyles.borderColor} inset, 0 1px 0 ${props.controlStyles.borderColor} inset`,
        width: WIDTH,
        height: HEIGHT,
        ...props.style,
      }}
    >
      <svg
        className={`${R.pathOr('', ['controlClassName'], props)}`}
        version='1.1'
        xmlns='http://www.w3.org/2000/svg'
        viewBox={`0 0 ${WIDTH} ${HEIGHT}`}
        width={WIDTH}
        height={HEIGHT}
        onMouseDown={props.controlStyles.interactive ? onMouseDown : undefined}
        onContextMenu={props.onContextMenu}
      >
        {circles}
      </svg>
    </div>
  )
}
