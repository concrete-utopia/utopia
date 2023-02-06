import React from 'react'
import { when } from '../../../../utils/react-conditionals'
import { LargerIcons, SquareButton, useColorTheme } from '../../../../uuiui'
import { Utils } from '../../../../uuiui-deps'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import CanvasActions from '../../canvas-actions'
import { Timeout } from './controls-common'

const FloatingScaleIndicatorWidth = 112
const FloatingScaleIndicatorHeight = 42

const FloatingScaleIndicatorTimeoutMS = 3500

interface FloatingScaleIndicatorProps {}

export const FloatingScaleIndicator = React.memo<FloatingScaleIndicatorProps>(() => {
  const timerHandleRef = React.useRef<Timeout | null>(null)
  const isFirstRenderRef = React.useRef<boolean>(true)

  const [shown, setShown] = React.useState<boolean>(false)

  const scale = useEditorState(
    Substores.canvasOffset,
    (store) => store.editor.canvas.scale,
    'FloatingScaleIndicator scale',
  )

  const disableHideTimer = React.useCallback(() => {
    if (timerHandleRef.current != null) {
      clearTimeout(timerHandleRef.current)
    }
    timerHandleRef.current = null
  }, [])

  const startHideTimer = React.useCallback(() => {
    setShown(true)
    disableHideTimer()
    timerHandleRef.current = setTimeout(() => setShown(false), FloatingScaleIndicatorTimeoutMS)
  }, [disableHideTimer])

  React.useEffect(() => {
    if (isFirstRenderRef.current === true) {
      isFirstRenderRef.current = false
      return
    }
    startHideTimer()
  }, [scale, startHideTimer])

  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const zoom100pct = React.useCallback(() => dispatch([CanvasActions.zoom(1)]), [dispatch])
  const zoomIn = React.useCallback(
    () => dispatch([CanvasActions.zoom(Utils.increaseScale(scale))]),
    [dispatch, scale],
  )
  const zoomOut = React.useCallback(
    () => dispatch([CanvasActions.zoom(Utils.decreaseScale(scale))]),
    [dispatch, scale],
  )

  return (
    <div
      onMouseEnter={disableHideTimer}
      onMouseLeave={startHideTimer}
      style={{
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        position: 'absolute',
        top: 4,
        left: `calc(50% - ${FloatingScaleIndicatorWidth / 2}px)`,
        width: FloatingScaleIndicatorWidth,
        height: FloatingScaleIndicatorHeight,
        backgroundColor: colorTheme.bg0.value,
        borderRadius: 24,
        padding: '4px 16px',
        border: `1px solid black`,
        fontSize: 12,
        opacity: shown ? 1 : 0,
        transform: 'opacity .3s',
      }}
    >
      <SquareButton highlight style={{ textAlign: 'center', width: 24 }} onClick={zoomOut}>
        <LargerIcons.MagnifyingGlassMinus />
      </SquareButton>
      <SquareButton highlight style={{ textAlign: 'center', width: 32 }} onClick={zoom100pct}>
        {scale}x
      </SquareButton>
      <SquareButton highlight style={{ textAlign: 'center', width: 24 }} onClick={zoomIn}>
        <LargerIcons.MagnifyingGlassPlus />
      </SquareButton>
    </div>
  )
})
