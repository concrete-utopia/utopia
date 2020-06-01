import * as Chroma from 'chroma-js'
import * as React from 'react'
import { colorTheme, FlexColumn } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import {
  CSSColor,
  cssColorToChromaColorOrDefault,
  CSSGradientStop,
  CSSNumber,
  orderStops,
  printColor,
  printLinearGradientBackgroundLayer,
} from '../../../common/css-utils'
import { checkerboardBackground } from '../../../common/inspector-utils'
import { ControlStyleDefaults } from '../../../common/control-status'
import { inspectorEdgePadding } from './background-picker'

interface GradientStopProps {
  stop: CSSGradientStop
  selected: boolean
  setSelectedIndex: (index: number) => void
  calculateDraggedValue: (
    index: number,
    screenX: number,
    dragOriginX: number,
    valueAtDragOrigin: number,
    transient: boolean,
  ) => void
  deleteStop: (index: number) => void
  index: number
}

const GradientStopSize = 24
const GradientStopCaratSize = 5

const GradientStop = betterReactMemo<GradientStopProps>('GradientStop', (props) => {
  let valueAtDragOrigin = React.useRef<number | undefined>(undefined)
  let dragScreenOrigin = React.useRef<
    | {
        x: number
        y: number
      }
    | undefined
  >(undefined)

  const rgba = cssColorToChromaColorOrDefault(props.stop.color).rgba()
  const rgbString = `rgba(${rgba[0]}, ${rgba[1]}, ${rgba[2]})`
  const rgbaString = `rgba(${rgba[0]}, ${rgba[1]}, ${rgba[2]}, ${rgba[3]})`

  const focusedStopBoxShadow = props.selected
    ? `, 0 0 0 1px ${colorTheme.inspectorFocusedColor.value}`
    : ''

  const { setSelectedIndex, calculateDraggedValue, index, selected, deleteStop } = props
  const onGradientStopMouseMove = React.useCallback(
    (e: MouseEvent) => {
      if (valueAtDragOrigin.current != null && dragScreenOrigin.current != null) {
        if (!selected) {
          const cachedIndex = index
          setSelectedIndex(cachedIndex)
          calculateDraggedValue(
            index,
            e.screenX,
            dragScreenOrigin.current.x,
            valueAtDragOrigin.current,
            true,
          )
        }
      }
    },
    [dragScreenOrigin, setSelectedIndex, calculateDraggedValue, index, valueAtDragOrigin, selected],
  )

  const onGradientStopMouseUp = React.useCallback(
    (e: MouseEvent) => {
      if (valueAtDragOrigin.current != null && dragScreenOrigin.current != null) {
        const deltaX = e.screenX - dragScreenOrigin.current.x
        const deltaY = e.screenY - dragScreenOrigin.current.y
        if (!selected) {
          setSelectedIndex(index)
        }
        if (Math.abs(deltaY) > 30) {
          deleteStop(index)
        } else {
          if (deltaX !== 0) {
            calculateDraggedValue(
              index,
              e.screenX,
              dragScreenOrigin.current.x,
              valueAtDragOrigin.current,
              false,
            )
          }
        }
      }
      document.removeEventListener('mousemove', onGradientStopMouseMove)
      document.removeEventListener('mouseup', onGradientStopMouseUp)
    },
    [
      dragScreenOrigin,
      onGradientStopMouseMove,
      valueAtDragOrigin,
      calculateDraggedValue,
      deleteStop,
      setSelectedIndex,
      selected,
      index,
    ],
  )

  const stopPositionValue = props.stop.position.value
  const onGradientStopMouseDown = React.useCallback(
    (e: React.MouseEvent<HTMLDivElement>) => {
      e.preventDefault()
      e.stopPropagation()
      if (!selected) {
        setSelectedIndex(index)
      }
      valueAtDragOrigin.current = stopPositionValue
      dragScreenOrigin.current = {
        x: e.nativeEvent.screenX,
        y: e.nativeEvent.screenY,
      }
      document.addEventListener('mousemove', onGradientStopMouseMove)
      document.addEventListener('mouseup', onGradientStopMouseUp)
    },
    [
      onGradientStopMouseMove,
      onGradientStopMouseUp,
      selected,
      setSelectedIndex,
      stopPositionValue,
      index,
    ],
  )

  const position = Math.max(0, Math.min(100, props.stop.position.value))

  return (
    <FlexColumn
      style={{
        position: 'absolute',
        left: `${position}%`,
        zIndex: props.selected ? 1 : undefined,
        marginLeft: -(GradientStopSize / 2),
        alignItems: 'center',
      }}
    >
      <div
        id={`gradient-stop-${props.index}`}
        onMouseDown={onGradientStopMouseDown}
        className={'ignore-react-onclickoutside'}
        style={{
          width: GradientStopSize,
          height: GradientStopSize,
          backgroundColor: 'white',
          boxShadow: `0 0 0 1px rgba(0, 0, 0, 0.12)${focusedStopBoxShadow}`,
          borderRadius: 3,
          overflow: 'hidden',
        }}
      >
        <div
          className='stop-color'
          style={{
            width: 'calc(100% - 4px)',
            height: 'calc(100% - 4px)',
            margin: 2,
            backgroundColor: 'white',
            borderRadius: 2,
            boxShadow: `0 0 2px rgba(0, 0, 0, 0.24) inset`,
            backgroundImage: `linear-gradient(135deg, transparent 65%, ${rgbString} 65%),
            linear-gradient(${rgbaString}, ${rgbaString}),
            ${checkerboardBackground.backgroundImage}`,
            backgroundSize: `100% 100%, 100% 100%, ${checkerboardBackground.backgroundSize}`,
            backgroundPosition: `0 0, 0 0, ${checkerboardBackground.backgroundPosition}`,
          }}
        />
      </div>
      <div
        style={{
          position: 'relative',
          width: GradientStopCaratSize * 2,
        }}
      >
        <div
          style={{
            position: 'absolute',
            left: 0,
            top: 0,
            width: 0,
            height: 0,
            borderColor: `${
              props.selected
                ? colorTheme.inspectorFocusedColor.value
                : colorTheme.inspectorSetBorderColor.value
            } transparent transparent transparent`,
            borderStyle: 'solid',
            borderWidth: `${GradientStopCaratSize}px ${GradientStopCaratSize}px 0 ${GradientStopCaratSize}px`,
          }}
        />
        <div
          style={{
            position: 'absolute',
            left: 1,
            top: 0,
            width: 0,
            height: 0,
            borderColor: 'white transparent transparent transparent',
            borderStyle: 'solid',
            borderWidth: `${GradientStopCaratSize - 1}px ${GradientStopCaratSize - 1}px 0 ${
              GradientStopCaratSize - 1
            }px`,
            transform: 'translateY(-.5px)',
          }}
        />
      </div>
    </FlexColumn>
  )
})

function calculateNewIndex(
  currentIndex: number,
  currentStops: Array<CSSGradientStop>,
  newPosition: number,
): number {
  const localCurrentStops = [...currentStops]
  localCurrentStops.splice(currentIndex, 1)
  if (newPosition < localCurrentStops[0].position.value) {
    return 0
  }
  if (localCurrentStops.length >= 2) {
    for (let i = 0; i < localCurrentStops.length - 1; i++) {
      const bottomStopPosition = localCurrentStops[i].position.value
      const topStopPosition = localCurrentStops[i + 1].position.value
      if (newPosition >= bottomStopPosition && newPosition <= topStopPosition) {
        return i + 1
      }
    }
  }
  return localCurrentStops.length
}

export interface GradientControlProps {
  stops: Array<CSSGradientStop>
  onSubmitValue: (newValue: Array<CSSGradientStop>) => void
  onTransientSubmitValue: (newValue: Array<CSSGradientStop>) => void
  selectedIndex: number
  setSelectedIndex: (index: number) => void
  style?: React.CSSProperties
}

function calculateIntermediateStopColor(
  newStopPoint: number,
  stops: Array<CSSGradientStop>,
): CSSColor {
  const orderedStops = orderStops([...stops])
  if (newStopPoint <= orderedStops[0].position.value) {
    return orderedStops[0].color
  } else if (newStopPoint >= orderedStops[orderedStops.length - 1].position.value) {
    return orderedStops[orderedStops.length - 1].color
  }
  for (let i = 0; i < orderedStops.length; i++) {
    if (
      newStopPoint >= orderedStops[i].position.value &&
      newStopPoint <= orderedStops[i + 1].position.value
    ) {
      const segmentLength = orderedStops[i + 1].position.value - orderedStops[i].position.value
      const localStopPosition =
        (newStopPoint - orderedStops[i].position.value) * (1.0 / segmentLength)

      const color1 = cssColorToChromaColorOrDefault(orderedStops[i].color)
      const color2 = cssColorToChromaColorOrDefault(orderedStops[i + 1].color)
      const hex = Chroma.mix(color1, color2, localStopPosition, 'rgb').hex().toUpperCase()
      return {
        hex,
        type: 'Hex',
      }
    }
  }
  return orderedStops[orderedStops.length - 1].color
}

export const GradientStopsEditor = betterReactMemo<GradientControlProps>(
  'GradientStopsEditor',
  (props) => {
    const ref: React.RefObject<HTMLDivElement> = React.useRef(null)
    const gradientMousedownPosition = React.useRef<number | undefined>(undefined)

    function calculateDraggedValue(
      index: number,
      screenX: number,
      dragOriginX: number,
      valueAtDragOrigin: number,
      transient: boolean,
    ): void {
      if (ref.current != null) {
        const width = ref.current.offsetWidth - 8
        const deltaX = screenX - dragOriginX
        const clampedValue = Number(
          (Math.max(0, Math.min(1, deltaX / width + valueAtDragOrigin / 100)) * 100).toFixed(2),
        )
        const newStops: Array<CSSGradientStop> = [...props.stops]
        const newStop: CSSGradientStop = {
          ...newStops[index],
          position: { unit: '%', value: clampedValue },
        }
        newStops[index] = newStop
        if (transient) {
          props.onTransientSubmitValue(newStops)
        } else {
          props.onSubmitValue(newStops)
        }
        props.setSelectedIndex(calculateNewIndex(index, newStops, clampedValue))
      }
    }
    const { stops, onSubmitValue } = props
    const handleGradientMouseUp = React.useCallback(
      (e: MouseEvent) => {
        if (
          ref.current != null &&
          gradientMousedownPosition.current != null &&
          gradientMousedownPosition.current === e.screenX
        ) {
          const width = ref.current.offsetWidth - GradientStopSize - 2
          const position: CSSNumber = {
            value: width > 0 ? Number(((e.offsetX / width) * 100).toFixed(2)) : 0,
            unit: '%',
          }
          const color = calculateIntermediateStopColor(position.value, stops)
          const stop: CSSGradientStop = {
            color,
            position,
          }
          const newStops = [...stops, stop]
          onSubmitValue(newStops)
          ref.current.removeEventListener('mouseup', handleGradientMouseUp)
        }
      },
      [stops, onSubmitValue],
    )

    const handleGradientMouseDown = React.useCallback(
      (e: React.MouseEvent<HTMLDivElement>) => {
        e.preventDefault()
        if (ref.current != null) {
          gradientMousedownPosition.current = e.nativeEvent.screenX
          ref.current.addEventListener('mouseup', handleGradientMouseUp)
        }
      },
      [handleGradientMouseUp],
    )

    const deleteStop = (index: number) => {
      if (props.stops.length > 2) {
        const newValue = [...props.stops]
        newValue.splice(index, 1)
        props.onSubmitValue(newValue)
      }
    }

    const gradientStopElements = stops.map((stop, index: number) => {
      return (
        <GradientStop
          key={`${printColor(stop.color)}${stop.position.value}${stop.position.unit}`}
          index={index}
          stop={stop}
          selected={props.selectedIndex === index}
          setSelectedIndex={props.setSelectedIndex}
          calculateDraggedValue={calculateDraggedValue}
          deleteStop={deleteStop}
        />
      )
    })

    return (
      <div
        style={{
          width: '100%',
          height: 75,
          paddingTop: inspectorEdgePadding,
        }}
      >
        <div
          style={{
            display: 'inline-block',
            position: 'relative',
            width: '100%',
            height: GradientStopSize,
            paddingLeft: GradientStopSize / 2 + inspectorEdgePadding,
            paddingRight: GradientStopSize / 2 + inspectorEdgePadding,
          }}
          ref={ref}
        >
          <div
            key='gradientStopBar'
            style={{
              marginLeft: 1,
              marginRight: 1,
              height: GradientStopSize + GradientStopCaratSize + 1,
              position: 'relative',
            }}
            onMouseDown={handleGradientMouseDown}
          >
            {gradientStopElements}
          </div>

          <div
            className='gradientcontrol-colorfield'
            key='gradientcontrol-colorfield'
            onMouseDown={handleGradientMouseDown}
            style={{
              height: '100%',
              width: '100%',
              position: 'relative',
              borderRadius: 4,
              backgroundColor: 'white',
              boxShadow: `0 0 0 1px ${ControlStyleDefaults.SetBorderColor} inset`,
              backgroundImage: `${printLinearGradientBackgroundLayer({
                type: 'linear-gradient',
                enabled: true,
                angle: {
                  default: false,
                  value: {
                    value: 90,
                    unit: 'deg',
                  },
                },
                stops,
              })},
              ${checkerboardBackground.backgroundImage}`,
              backgroundSize: `100% 100%, ${checkerboardBackground.backgroundSize}`,
              backgroundPosition: `0 0, ${checkerboardBackground.backgroundPosition}`,
            }}
          />
        </div>
      </div>
    )
  },
)
