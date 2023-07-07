import Chroma from 'chroma-js'
import React from 'react'
import type { CSSBackgroundLayers, CSSColor, CSSGradientStop } from '../../../common/css-utils'
import {
  cssColorToChromaColorOrDefault,
  cssNumber,
  orderStops,
  printLinearGradientBackgroundLayer,
} from '../../../common/css-utils'
import type { OnSubmitValueAndUpdateLocalState } from '../../../common/inspector-utils'
import type { UseSubmitValueFactory } from '../../../common/property-path-hooks'
import {
  GradientPickerWidth,
  GradientStopCaratSize,
  GradientStopSize,
  StopsPadding,
} from '../../../controls/color-picker'
import { inspectorEdgePadding } from './background-picker'
import { clampValue } from '../../../../../core/shared/math-utils'
import { useColorTheme, FlexColumn, UtopiaStyles } from '../../../../../uuiui'

const checkerboardBackground = UtopiaStyles.backgrounds.checkerboardBackground

interface GradientStopProps {
  stop: CSSGradientStop
  selected: boolean
  setSelectedIndex: (index: number) => void
  unorderedIndex: number
  focusStopEditor: () => void
  indexedUpdateStop: (newStop: CSSGradientStop, dragState: 'dragStart' | 'drag' | 'dragEnd') => void
  indexedDeleteStop: () => void
}

const GradientStop = React.memo<GradientStopProps>(
  ({
    stop,
    selected,
    setSelectedIndex,
    unorderedIndex,
    focusStopEditor,
    indexedUpdateStop,
    indexedDeleteStop,
  }) => {
    const colorTheme = useColorTheme()
    const valueAtDragOrigin = React.useRef<number | undefined>(undefined)
    const dragScreenOrigin = React.useRef<
      | {
          x: number
          y: number
        }
      | undefined
    >(undefined)

    const rgba = cssColorToChromaColorOrDefault(stop.color).rgba()
    const rgbString = `rgba(${rgba[0]}, ${rgba[1]}, ${rgba[2]})`
    const rgbaString = `rgba(${rgba[0]}, ${rgba[1]}, ${rgba[2]}, ${rgba[3]})`

    const focusedStopBoxShadow = selected
      ? `, 0 0 0 1px ${colorTheme.inspectorFocusedColor.value}`
      : ''

    const stopPositionValue = stop.position.value

    const onMouseMove = React.useCallback(
      (e: MouseEvent) => {
        if (valueAtDragOrigin.current != null && dragScreenOrigin.current != null) {
          indexedUpdateStop(
            updateStopWithDrag(
              stop,
              e.screenX,
              dragScreenOrigin.current.x,
              valueAtDragOrigin.current,
            ),
            'drag',
          )
        }
      },
      [dragScreenOrigin, valueAtDragOrigin, indexedUpdateStop, stop],
    )

    const onMouseUp = React.useCallback(
      (e: MouseEvent) => {
        if (valueAtDragOrigin.current != null && dragScreenOrigin.current != null) {
          const deltaX = e.screenX - dragScreenOrigin.current.x
          const deltaY = e.screenY - dragScreenOrigin.current.y
          if (Math.abs(deltaY) > 30) {
            indexedDeleteStop()
          } else {
            if (deltaX !== 0) {
              indexedUpdateStop(
                updateStopWithDrag(
                  stop,
                  e.screenX,
                  dragScreenOrigin.current.x,
                  valueAtDragOrigin.current,
                ),
                'dragEnd',
              )
            }
          }
        }
        document.removeEventListener('mousemove', onMouseMove)
        document.removeEventListener('mouseup', onMouseUp)
      },
      [
        dragScreenOrigin,
        onMouseMove,
        valueAtDragOrigin,
        indexedUpdateStop,
        stop,
        indexedDeleteStop,
      ],
    )

    const onMouseDown = React.useCallback(
      (e: React.MouseEvent) => {
        focusStopEditor()
        e.stopPropagation()
        setSelectedIndex(unorderedIndex)
        valueAtDragOrigin.current = stopPositionValue
        dragScreenOrigin.current = {
          x: e.nativeEvent.screenX,
          y: e.nativeEvent.screenY,
        }
        document.addEventListener('mousemove', onMouseMove)
        document.addEventListener('mouseup', onMouseUp)
      },
      [
        onMouseMove,
        onMouseUp,
        setSelectedIndex,
        stopPositionValue,
        unorderedIndex,
        focusStopEditor,
      ],
    )

    const position = Math.max(0, Math.min(100, stop.position.value))

    return (
      <FlexColumn
        style={{
          position: 'absolute',
          left: `${position}%`,
          zIndex: selected ? 1 : undefined,
          marginLeft: -(GradientStopSize / 2),
          alignItems: 'center',
        }}
      >
        <div
          id={`gradient-stop-${unorderedIndex}`}
          onMouseDown={onMouseDown}
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
                selected
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
  },
)

export interface GradientControlProps {
  stops: Array<CSSGradientStop>
  onSubmitValueAndUpdateLocalStops: OnSubmitValueAndUpdateLocalState<Array<CSSGradientStop>>
  useSubmitValueFactory: UseSubmitValueFactory<CSSBackgroundLayers>
  selectedStopUnorderedIndex: number
  setSelectedStopUnorderedIndex: (index: number) => void
  style?: React.CSSProperties
}

function calculateIntermediateStopColor(
  newStopPoint: number,
  stops: Array<CSSGradientStop>,
): CSSColor {
  const orderedStops = orderStops([...stops])
  // Is new stop before lowest stop?
  if (newStopPoint <= orderedStops[0].position.value) {
    return orderedStops[0].color
  }

  // Is new stop between existing stops?
  for (let i = 0; i < orderedStops.length - 1; i++) {
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

  // New stop is after highest stop
  return orderedStops[orderedStops.length - 1].color
}

function incrementSelectedStopPosition(
  incrementValue: number,
  oldValue: Array<CSSGradientStop>,
  index: number,
): Array<CSSGradientStop> {
  const workingValue = [...oldValue]
  const workingStop = { ...workingValue[index] }
  workingStop.position.value = clampValue(workingStop.position.value + incrementValue, 0, 100)
  workingValue[index] = workingStop
  return workingValue
}

function updateStopWithDrag(
  oldValue: CSSGradientStop,
  screenX: number,
  dragOriginX: number,
  valueAtDragOrigin: number,
): CSSGradientStop {
  const workingStop = { ...oldValue }
  if (workingStop != null) {
    const deltaX = screenX - dragOriginX
    const calculatedPercent = (deltaX / GradientPickerWidth) * 100 + valueAtDragOrigin
    const clamped = clampValue(calculatedPercent, 0, 100)
    const rounded = Math.round(clamped)
    const workingPosition = { ...workingStop.position }
    workingPosition.value = rounded
    workingStop.position = workingPosition
  }
  return workingStop
}

function getIndexedUpdateStop(
  index: number,
  oldValue: Array<CSSGradientStop>,
  setStateStops: OnSubmitValueAndUpdateLocalState<Array<CSSGradientStop>>,
): (newStop: CSSGradientStop, dragState: 'dragStart' | 'drag' | 'dragEnd') => void {
  return function updateStop(newStop, dragState) {
    const workingValue = [...oldValue]
    workingValue[index] = newStop
    setStateStops(workingValue, dragState)
  }
}

function insertStop(position: number, oldValue: Array<CSSGradientStop>): Array<CSSGradientStop> {
  const color = calculateIntermediateStopColor(position, oldValue)
  const stop: CSSGradientStop = {
    color,
    position: cssNumber(position, '%'),
  }
  return [...oldValue, stop]
}

function deleteStop(index: number, oldValue: Array<CSSGradientStop>): Array<CSSGradientStop> {
  const working = [...oldValue]
  if (working.length > 2) {
    delete working[index]
  }
  return working
}

function deleteStopAndUpdateIndex(
  index: number,
  stops: Array<CSSGradientStop>,
  setStops: OnSubmitValueAndUpdateLocalState<Array<CSSGradientStop>>,
  setSelectedStopUnorderedIndex: React.Dispatch<number>,
): void {
  if (stops[index] != null) {
    const previousPosition = stops[index].position.value
    const newStops = deleteStop(index, stops)
    const { lowestDistance, indexWithLowestDistance } = newStops.reduce(
      (working, stop: CSSGradientStop | undefined, i) => {
        if (stop != null) {
          const distance = Math.abs(previousPosition - stop.position.value)
          if (distance < working.lowestDistance) {
            working.lowestDistance = distance
            working.indexWithLowestDistance = i
          }
        }
        return working
      },
      {
        lowestDistance: Infinity,
        indexWithLowestDistance: -1,
      },
    )
    if (isFinite(lowestDistance) && indexWithLowestDistance >= 0) {
      setStops(newStops, 'dragEnd')
      setSelectedStopUnorderedIndex(indexWithLowestDistance)
    }
  }
}

export const GradientStopsEditor = React.memo<GradientControlProps>(
  ({
    selectedStopUnorderedIndex,
    setSelectedStopUnorderedIndex,
    stops,
    onSubmitValueAndUpdateLocalStops: setLocalAndEditorStops,
  }) => {
    const ref: React.RefObject<HTMLDivElement> = React.useRef(null)
    const colorTheme = useColorTheme()
    const onMouseDown = React.useCallback(
      (e: React.MouseEvent) => {
        if (ref.current != null) {
          const newStops = insertStop(
            Number(((e.nativeEvent.offsetX / GradientPickerWidth) * 100).toFixed(2)),
            stops,
          )
          setLocalAndEditorStops(newStops, 'dragStart')
          setSelectedStopUnorderedIndex(newStops.length - 1)
        }
      },
      [stops, setSelectedStopUnorderedIndex, setLocalAndEditorStops],
    )

    const onKeyDown = React.useCallback(
      (e: React.KeyboardEvent<HTMLDivElement>) => {
        if (e.key === 'Backspace' || e.key === 'Delete') {
          deleteStopAndUpdateIndex(
            selectedStopUnorderedIndex,
            stops,
            setLocalAndEditorStops,
            setSelectedStopUnorderedIndex,
          )
          e.stopPropagation()
        } else if (e.key === 'ArrowLeft') {
          // TODO: transient actions when holding
          setLocalAndEditorStops(
            incrementSelectedStopPosition(e.shiftKey ? -10 : -1, stops, selectedStopUnorderedIndex),
            'notDragging',
          )
          e.stopPropagation()
        } else if (e.key === 'ArrowRight') {
          // TODO: transient actions when holding
          setLocalAndEditorStops(
            incrementSelectedStopPosition(e.shiftKey ? 10 : 1, stops, selectedStopUnorderedIndex),
            'notDragging',
          )
          e.stopPropagation()
        }
      },
      [selectedStopUnorderedIndex, stops, setLocalAndEditorStops, setSelectedStopUnorderedIndex],
    )

    const focusStopEditor = React.useCallback(() => {
      if (ref.current != null) {
        ref.current.focus()
      }
    }, [ref])

    const onWrapperMouseDown = React.useCallback(
      (e: React.MouseEvent) => {
        e.stopPropagation()
        focusStopEditor()
      },
      [focusStopEditor],
    )

    const getIndexedDeleteStop = React.useCallback(
      (index: number) => () => {
        deleteStopAndUpdateIndex(
          index,
          stops,
          setLocalAndEditorStops,
          setSelectedStopUnorderedIndex,
        )
      },
      [stops, setLocalAndEditorStops, setSelectedStopUnorderedIndex],
    )

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
            position: 'relative',
            width: '100%',
            height: GradientStopSize,
            paddingLeft: StopsPadding,
            paddingRight: StopsPadding,
          }}
          ref={ref}
          onMouseDown={onWrapperMouseDown}
          tabIndex={0}
          onKeyDown={onKeyDown}
        >
          <div
            key='gradientStopBar'
            style={{
              marginLeft: 1,
              marginRight: 1,
              height: GradientStopSize + GradientStopCaratSize + 1,
              position: 'relative',
            }}
            onMouseDown={onMouseDown}
          >
            {stops.map((stop, unorderedIndex) =>
              stop != null ? (
                <GradientStop
                  // key={index} is usually an anti-pattern, but reorders only
                  // change stops' position values, leaving array order stable.
                  key={unorderedIndex}
                  stop={stop}
                  unorderedIndex={unorderedIndex}
                  selected={selectedStopUnorderedIndex === unorderedIndex}
                  setSelectedIndex={setSelectedStopUnorderedIndex}
                  focusStopEditor={focusStopEditor}
                  indexedUpdateStop={getIndexedUpdateStop(
                    unorderedIndex,
                    stops,
                    setLocalAndEditorStops,
                  )}
                  indexedDeleteStop={getIndexedDeleteStop(unorderedIndex)}
                />
              ) : null,
            )}
          </div>

          <div
            className='gradientcontrol-colorfield'
            key='gradientcontrol-colorfield'
            onMouseDown={onMouseDown}
            style={{
              height: '100%',
              width: '100%',
              position: 'relative',
              borderRadius: 4,
              backgroundColor: 'white',
              boxShadow: `0 0 0 1px ${colorTheme.neutralBorder.value} inset`,
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
