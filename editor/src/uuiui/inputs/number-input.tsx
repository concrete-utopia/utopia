/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import type { Interpolation } from '@emotion/react'
import { jsx } from '@emotion/react'
import type { MouseEventHandler } from 'react'
import React from 'react'
import type {
  CSSNumber,
  CSSNumberType,
  CSSNumberUnit,
  UnknownOrEmptyInput,
} from '../../components/inspector/common/css-utils'
import {
  cssNumber,
  cssNumberToString,
  emptyInputValue,
  getCSSNumberUnit,
  isCSSNumber,
  isEmptyInputValue,
  isUnknownInputValue,
  parseCSSNumber,
  setCSSNumberValue,
  unknownInputValue,
} from '../../components/inspector/common/css-utils'
import type {
  OnUnsetValues,
  SubmitValueFactoryReturn,
} from '../../components/inspector/common/property-path-hooks'
import type {
  InspectorControlProps,
  OnSubmitValue,
  OnSubmitValueOrEmpty,
  OnSubmitValueOrUnknownOrEmpty,
  OnSubmitValueOrUnknownOrEmptyMaybeTransient,
} from '../../components/inspector/controls/control'
import type { Either } from '../../core/shared/either'
import { isLeft, mapEither } from '../../core/shared/either'
import { clampValue, point } from '../../core/shared/math-utils'
import { memoize } from '../../core/shared/memoize'
import type { ControlStyles } from '../../uuiui-deps'
import { getControlStyles, CSSCursor } from '../../uuiui-deps'
import type { IcnProps } from '../icn'
import { Icn } from '../icn'
import { useColorTheme, UtopiaTheme } from '../styles/theme'
import { FlexRow } from '../widgets/layout/flex-row'
import type { BaseInputProps, BoxCorners, ChainedType } from './base-input'
import {
  getBorderRadiusStyles,
  getControlStylesAwarePlaceholder,
  InspectorInput,
} from './base-input'
import { usePropControlledStateV2 } from '../../components/inspector/common/inspector-utils'
import { useControlsDisabledInSubtree } from '../utilities/disable-subtree'
import { getPossiblyHashedURL } from '../../utils/hashed-assets'

function getDisplayValueNotMemoized(
  value: CSSNumber | null,
  defaultUnitToHide: CSSNumberUnit | null,
  mixed: boolean,
  showContent: boolean,
): string {
  if (!mixed && value != null && showContent) {
    const unit = getCSSNumberUnit(value)
    const showUnit = unit !== defaultUnitToHide
    return cssNumberToString(value, showUnit)
  } else {
    return '–'
  }
}

const getDisplayValue = memoize(getDisplayValueNotMemoized, { maxSize: 1000 })

function parseDisplayValueNotMemoized(
  input: string,
  numberType: CSSNumberType,
  defaultUnit: CSSNumberUnit | null,
): Either<string, CSSNumber> {
  const parsedInput = parseCSSNumber(input, numberType)
  return mapEither((value: CSSNumber) => {
    if (value.unit == null) {
      return cssNumber(value.value, defaultUnit)
    } else {
      return value
    }
  }, parsedInput)
}

const parseDisplayValue = memoize(parseDisplayValueNotMemoized, { maxSize: 1000 })

function dragDeltaSign(delta: number): 1 | -1 {
  return delta >= 0 ? 1 : -1
}

export function calculateDragDirectionDelta(
  delta: number,
  scalingFactor: number,
): {
  result: number
  inverse: (value: number) => number
} {
  const sign = dragDeltaSign(delta)
  const rawAbsDelta = Math.abs(delta)
  // Floor the value and then restore its sign so that it is rounded towards zero.
  const scaledAbsDelta = Math.floor(rawAbsDelta / scalingFactor)
  // save the diff for inverse calculation
  const diff = rawAbsDelta - scaledAbsDelta * scalingFactor
  return {
    result: sign * scaledAbsDelta,
    inverse: (value: number) => {
      return sign * (Math.abs(value) * scalingFactor + diff)
    },
  }
}

function calculateDragDelta(
  delta: number,
  scalingFactor: number = 2,
): {
  result: number
  inverse: (value: number) => number
} {
  return calculateDragDirectionDelta(delta, scalingFactor)
}

let incrementTimeout: number | undefined = undefined
let incrementAnimationFrame: number | undefined = undefined
const repeatThreshold: number = 500

export interface NumberInputOptions {
  innerLabel?: React.ReactChild
  minimum?: number
  maximum?: number
  stepSize?: number
  incrementControls?: boolean
  chained?: ChainedType
  height?: number
  roundCorners?: BoxCorners
  numberType: CSSNumberType
  defaultUnitToHide: CSSNumberUnit | null
  pasteHandler?: boolean
  descriptionLabel?: string
  disableScrubbing?: boolean
}

export interface AbstractNumberInputProps<T extends CSSNumber | number>
  extends NumberInputOptions,
    BaseInputProps,
    InspectorControlProps {
  value: T | null | undefined
  invalid?: boolean
}

export interface NumberInputProps extends AbstractNumberInputProps<CSSNumber> {
  onSubmitValue?: OnSubmitValueOrUnknownOrEmptyMaybeTransient<CSSNumber>
  onTransientSubmitValue?: OnSubmitValueOrUnknownOrEmptyMaybeTransient<CSSNumber>
  onForcedSubmitValue?: OnSubmitValueOrUnknownOrEmptyMaybeTransient<CSSNumber>
  setGlobalCursor?: (cursor: CSSCursor | null) => void
  onMouseEnter?: MouseEventHandler
  onMouseLeave?: MouseEventHandler
}

const ScrubThreshold = 3

export const NumberInput = React.memo<NumberInputProps>(
  ({
    value: propsValue,
    style,
    testId,
    inputProps = {},
    id,
    innerLabel,
    minimum: unscaledMinimum = -Infinity,
    maximum: unscaledMaximum = Infinity,
    stepSize: unscaledStepSize,
    incrementControls = false,
    chained = 'not-chained',
    height = UtopiaTheme.layout.inputHeight.default,
    roundCorners = 'all',
    onSubmitValue,
    onTransientSubmitValue,
    onForcedSubmitValue,
    controlStatus = 'simple',
    focusOnMount = false,
    numberType,
    defaultUnitToHide,
    setGlobalCursor,
    onMouseEnter,
    onMouseLeave,
    invalid,
    pasteHandler,
    disableScrubbing = false,
  }) => {
    const ref = React.useRef<HTMLInputElement>(null)
    const colorTheme = useColorTheme()

    const controlStyles = React.useMemo((): ControlStyles => {
      return {
        ...getControlStyles(controlStatus),
        invalid: invalid ?? false,
      }
    }, [controlStatus, invalid])

    const controlsDisabledInSubtree = useControlsDisabledInSubtree()

    const disabled = !controlStyles.interactive || controlsDisabledInSubtree

    const { mixed, showContent } = React.useMemo(
      () => ({
        mixed: controlStyles.mixed,
        showContent: controlStyles.showContent && !controlStyles.invalid,
      }),
      [controlStyles],
    )

    const [value, setValue] = usePropControlledStateV2(propsValue ?? null)
    const [displayValue, setDisplayValue] = usePropControlledStateV2(
      getDisplayValue(value, defaultUnitToHide, mixed, showContent),
    )
    React.useEffect(() => {
      if (mixed) {
        setDisplayValue('')
      }
    }, [mixed, setDisplayValue])

    const valueUnit = React.useMemo(() => value?.unit ?? null, [value])

    const [isActuallyFocused, setIsActuallyFocused] = React.useState<boolean>(false)
    const [isFauxcused, setIsFauxcused] = React.useState<boolean>(false)
    const isFocused = isActuallyFocused || isFauxcused

    const [, setValueAtDragOriginState] = React.useState<number>(0)
    const valueAtDragOrigin = React.useRef<number | null>(null)
    const setValueAtDragOrigin = (n: number) => {
      valueAtDragOrigin.current = n
      setValueAtDragOriginState(n)
    }

    const [dragOriginX, setDragOriginX] = React.useState<number | null>(null)
    const [dragOriginY, setDragOriginY] = React.useState<number | null>(null)

    const [, setScrubThresholdPassedState] = React.useState<boolean>(false)
    const scrubThresholdPassed = React.useRef(false)
    const setScrubThresholdPassed = (b: boolean) => {
      scrubThresholdPassed.current = b
      setScrubThresholdPassedState(b)
    }

    const simulatedPointerRef = React.useRef(null)
    const pointerOriginRef = React.useRef<HTMLDivElement>(null)

    const accumulatedMouseDeltaX = React.useRef(0)
    const clampedAccumulatedDelta = React.useRef(0)
    // This is here to alleviate a circular reference issue that I stumbled into with the callbacks,
    // it means that the cleanup callback isn't dependent on the event listeners, which result in
    // a break in the circle.
    const scrubbingCleanupCallbacks = React.useRef<Array<() => void>>([])

    const [valueChangedSinceFocus, setValueChangedSinceFocus] = React.useState<boolean>(false)

    const scaleFactor = valueUnit === '%' ? 100 : 1
    const minimum = scaleFactor * unscaledMinimum
    const maximum = scaleFactor * unscaledMaximum
    const stepSize = unscaledStepSize == null ? 1 : unscaledStepSize * scaleFactor

    const repeatedValueRef = React.useRef(value ?? null)

    const updateValue = React.useCallback(
      (newValue: CSSNumber | null) => {
        setValue(newValue)
        setDisplayValue(getDisplayValue(newValue, defaultUnitToHide, mixed, showContent))
      },
      [setValue, setDisplayValue, defaultUnitToHide, mixed, showContent],
    )

    const incrementBy = React.useCallback(
      (incrementStepSize: number, shiftKey: boolean, transient: boolean) => {
        if (value == null) {
          return cssNumber(0)
        }
        const delta = incrementStepSize * (shiftKey ? 10 : 1)
        const newNumericValue = clampValue(value.value + delta, minimum, maximum)
        const newValue = setCSSNumberValue(value, newNumericValue)
        if (transient) {
          if (onTransientSubmitValue != null) {
            onTransientSubmitValue(newValue, transient)
          } else if (onSubmitValue != null) {
            onSubmitValue(newValue, transient)
          }
        } else {
          if (onForcedSubmitValue != null) {
            onForcedSubmitValue(newValue, transient)
          } else if (onSubmitValue != null) {
            onSubmitValue(newValue, transient)
          }
        }
        repeatedValueRef.current = newValue
        setValueChangedSinceFocus(true)
        updateValue(newValue)
        return newValue
      },
      [
        maximum,
        minimum,
        onForcedSubmitValue,
        onSubmitValue,
        onTransientSubmitValue,
        value,
        updateValue,
      ],
    )

    const repeatIncrement = React.useCallback(
      (
        currentValue: CSSNumber,
        incrementStepSize: number,
        shiftKey: boolean,
        transient: boolean,
      ) => {
        const newValue = incrementBy(incrementStepSize, shiftKey, transient)
        incrementAnimationFrame = window.requestAnimationFrame(() =>
          repeatIncrement(newValue, incrementStepSize, shiftKey, transient),
        )
      },
      [incrementBy],
    )

    const setScrubValue = React.useCallback(
      (transient: boolean) => {
        if (valueAtDragOrigin.current != null) {
          const { result: dragDelta, inverse } = calculateDragDelta(clampedAccumulatedDelta.current)
          const totalClampedValue = clampValue(
            valueAtDragOrigin.current + stepSize * dragDelta,
            minimum,
            maximum,
          )
          const clampedDelta = (totalClampedValue - valueAtDragOrigin.current) / stepSize
          clampedAccumulatedDelta.current = inverse(clampedDelta)
          const newValue = cssNumber(totalClampedValue, valueUnit)

          if (transient) {
            if (onTransientSubmitValue != null) {
              onTransientSubmitValue(newValue, transient)
            } else if (onSubmitValue != null) {
              onSubmitValue(newValue, transient)
            }
          } else {
            if (onForcedSubmitValue != null) {
              onForcedSubmitValue(newValue, transient)
            } else if (onSubmitValue != null) {
              onSubmitValue(newValue, transient)
            }
          }
          updateValue(newValue)
        }
      },
      [
        stepSize,
        minimum,
        maximum,
        valueUnit,
        updateValue,
        onTransientSubmitValue,
        onSubmitValue,
        onForcedSubmitValue,
      ],
    )

    React.useEffect(() => {
      if (focusOnMount && ref.current != null) {
        ref.current.focus()
      }
    }, [focusOnMount, ref])

    const onThresholdPassed = (e: MouseEvent, fn: () => void) => {
      const thresholdPassed =
        scrubThresholdPassed.current || Math.abs(accumulatedMouseDeltaX.current) >= ScrubThreshold
      if (thresholdPassed) {
        fn()
      }
    }

    const cancelPointerLock = React.useCallback(
      (revertChanges: 'revert-nothing' | 'revert-changes') => {
        if (document.pointerLockElement === pointerOriginRef.current) {
          document.exitPointerLock()
        }
        if (
          revertChanges === 'revert-changes' &&
          onSubmitValue != null &&
          valueAtDragOrigin.current != null
        ) {
          const oldValue = cssNumber(valueAtDragOrigin.current, valueUnit)
          onSubmitValue(oldValue, false)
        }

        setIsFauxcused(false)
        ref.current?.focus()

        setScrubThresholdPassed(false)
        setGlobalCursor?.(null)
      },
      [onSubmitValue, setGlobalCursor, valueUnit],
    )

    const checkPointerLockChange = React.useCallback(() => {
      if (document.pointerLockElement !== pointerOriginRef.current) {
        cancelPointerLock('revert-changes')
        scrubbingCleanupCallbacks.current.forEach((fn) => fn())
      }
    }, [cancelPointerLock])

    const scrubOnMouseUp = React.useCallback(
      (e: MouseEvent) => {
        scrubbingCleanupCallbacks.current.forEach((fn) => fn())
        onThresholdPassed(e, () => {
          setScrubValue(false)
        })
        cancelPointerLock('revert-nothing')
      },
      [cancelPointerLock, setScrubValue],
    )

    const scrubOnMouseMove = React.useCallback(
      (e: MouseEvent) => {
        // Apply the movement to the accumulated delta, as the movement is
        // relative to the last event.
        accumulatedMouseDeltaX.current += e.movementX
        clampedAccumulatedDelta.current += e.movementX

        onThresholdPassed(e, () => {
          if (!scrubThresholdPassed.current) {
            setScrubThresholdPassed(true)
            if (pointerOriginRef.current != null) {
              pointerOriginRef.current.requestPointerLock()
              scrubbingCleanupCallbacks.current.push(() => {
                window.removeEventListener('mouseup', scrubOnMouseUp)
              })
              scrubbingCleanupCallbacks.current.push(() => {
                window.removeEventListener('mousemove', scrubOnMouseMove)
              })
              scrubbingCleanupCallbacks.current.push(() => {
                document.removeEventListener('pointerlockchange', checkPointerLockChange, true)
              })
            }
          }
          setScrubValue(true)
        })
      },
      [checkPointerLockChange, scrubOnMouseUp, setScrubValue],
    )

    const rc = roundCorners == null ? 'all' : roundCorners
    const borderRadiusStyles = getBorderRadiusStyles(chained, rc)

    const onFocus = React.useCallback(
      (e: React.FocusEvent<HTMLInputElement>) => {
        setIsActuallyFocused(true)
        e.target.select()
        if (inputProps.onFocus != null) {
          inputProps.onFocus(e)
        }
      },
      [inputProps],
    )

    const onKeyDown = React.useCallback(
      (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (e.key === 'ArrowUp') {
          updateValue(incrementBy(stepSize, e.shiftKey, true))
        } else if (e.key === 'ArrowDown') {
          e.preventDefault()
          updateValue(incrementBy(-stepSize, e.shiftKey, true))
        } else if (e.key === 'Enter' || e.key === 'Escape') {
          e.nativeEvent.stopImmediatePropagation()
          e.preventDefault()
          ref.current?.blur()
        }
      },
      [updateValue, incrementBy, stepSize],
    )

    const onKeyUp = React.useCallback(
      (e: React.KeyboardEvent<HTMLInputElement>) => {
        // todo make sure this isn't doubling up the value submit
        if ((e.key === 'ArrowUp' || e.key === 'ArrowDown') && onForcedSubmitValue != null) {
          if (value != null) {
            onForcedSubmitValue(value, false)
          }
        }
      },
      [onForcedSubmitValue, value],
    )

    const onBlur = React.useCallback(
      (e: React.FocusEvent<HTMLInputElement>) => {
        setIsActuallyFocused(false)

        function getNewValue() {
          if (displayValue == '') {
            return emptyInputValue()
          }
          const parsed = parseDisplayValue(displayValue, numberType, defaultUnitToHide)
          if (isLeft(parsed)) {
            return unknownInputValue(displayValue)
          }
          return parsed.value
        }

        const newValue = getNewValue()
        if (isUnknownInputValue(newValue)) {
          updateValue(value)
          return
        }
        updateValue(isEmptyInputValue(newValue) ? cssNumber(0) : newValue)

        if (inputProps.onBlur != null) {
          inputProps.onBlur(e)
        }
        if (valueChangedSinceFocus) {
          setValueChangedSinceFocus(false)
          if (onSubmitValue != null) {
            onSubmitValue(newValue, false)
          }
        }
      },
      [
        inputProps,
        onSubmitValue,
        valueChangedSinceFocus,
        defaultUnitToHide,
        numberType,
        updateValue,
        value,
        displayValue,
      ],
    )

    const onChange = React.useCallback(
      (e: React.ChangeEvent<HTMLInputElement>) => {
        if (inputProps.onChange != null) {
          inputProps.onChange(e)
        }
        setValueChangedSinceFocus(true)
        setDisplayValue(e.target.value)
      },
      [inputProps, setDisplayValue],
    )

    const onIncrementMouseUp = React.useCallback(() => {
      window.removeEventListener('mouseup', onIncrementMouseUp)
      setIsFauxcused(false)
      ref.current?.focus()
      if (incrementTimeout != null) {
        window.clearTimeout(incrementTimeout)
        incrementTimeout = undefined
      }
      if (incrementAnimationFrame != null) {
        window.cancelAnimationFrame(incrementAnimationFrame ?? 0)
        incrementAnimationFrame = undefined
      }

      // Clear transient state by setting the final value
      if (onSubmitValue != null) {
        onSubmitValue(
          repeatedValueRef.current != null
            ? repeatedValueRef.current
            : unknownInputValue(displayValue),
          false,
        )
      }

      if (repeatedValueRef.current != null) {
        updateValue(repeatedValueRef.current)
      }
    }, [ref, onSubmitValue, updateValue, displayValue])

    const onIncrementMouseDown = React.useCallback(
      (e: React.MouseEvent) => {
        if (disabled) {
          return
        }
        if (e.button === 0) {
          e.stopPropagation()
          setIsFauxcused(true)
          window.addEventListener('mouseup', onIncrementMouseUp)
          const shiftKey = e.shiftKey
          const newValue = incrementBy(stepSize, shiftKey, false)
          incrementTimeout = window.setTimeout(() => {
            repeatIncrement(newValue, stepSize, shiftKey, true)
          }, repeatThreshold)
        }
      },
      [incrementBy, stepSize, repeatIncrement, onIncrementMouseUp, disabled],
    )

    const onDecrementMouseUp = React.useCallback(() => {
      window.removeEventListener('mouseup', onDecrementMouseUp)
      setIsFauxcused(false)
      ref.current?.focus()
      if (incrementTimeout != null) {
        window.clearTimeout(incrementTimeout)
        incrementTimeout = undefined
      }
      if (incrementAnimationFrame != null) {
        window.cancelAnimationFrame(incrementAnimationFrame ?? 0)
        incrementAnimationFrame = undefined
      }

      // Clear transient state by setting the final value
      if (onSubmitValue != null) {
        onSubmitValue(
          repeatedValueRef.current != null
            ? repeatedValueRef.current
            : unknownInputValue(displayValue),
          false,
        )
      }

      if (repeatedValueRef.current != null) {
        updateValue(repeatedValueRef.current)
      }
    }, [ref, onSubmitValue, displayValue, updateValue])

    const onDecrementMouseDown = React.useCallback(
      (e: React.MouseEvent) => {
        if (disabled) {
          return
        }
        if (e.button === 0) {
          e.stopPropagation()
          setIsFauxcused(true)
          window.addEventListener('mouseup', onDecrementMouseUp)
          const shiftKey = e.shiftKey
          const newValue = incrementBy(-stepSize, shiftKey, false)
          incrementTimeout = window.setTimeout(
            () => repeatIncrement(newValue, -stepSize, shiftKey, true),
            repeatThreshold,
          )
        }
      },
      [incrementBy, stepSize, repeatIncrement, onDecrementMouseUp, disabled],
    )

    const onLabelMouseDown = React.useCallback(
      (e: React.MouseEvent<HTMLDivElement>) => {
        if (disabled || disableScrubbing) {
          return
        }
        if (e.button === 0) {
          e.stopPropagation()
          setIsFauxcused(true)
          window.addEventListener('mousemove', scrubOnMouseMove)
          window.addEventListener('mouseup', scrubOnMouseUp)
          document.addEventListener('pointerlockchange', checkPointerLockChange, true)
          setValueAtDragOrigin(value?.value ?? 0)
          setDragOriginX(e.pageX)
          setDragOriginY(e.pageY)
          setGlobalCursor?.(CSSCursor.ResizeEW)
          accumulatedMouseDeltaX.current = 0
          clampedAccumulatedDelta.current = 0
        }
      },
      [
        disabled,
        disableScrubbing,
        scrubOnMouseMove,
        scrubOnMouseUp,
        checkPointerLockChange,
        value?.value,
        setGlobalCursor,
      ],
    )

    const placeholder = getControlStylesAwarePlaceholder(controlStyles)

    const chainedStyles: Interpolation<any> | undefined =
      (chained === 'first' || chained === 'middle') && !isFocused
        ? {
            '&:not(:hover)::after': {
              content: '""',
              width: 1,
              height: UtopiaTheme.layout.inputHeight.default / 2,
              backgroundColor: controlStyles.borderColor,
              zIndex: 2,
              position: 'absolute',
              top: UtopiaTheme.layout.inputHeight.default / 2,
              right: 0,
              transform: 'translateX(0.5px)',
            },
          }
        : undefined

    let simulatedPointerTransformX: number | undefined = undefined
    if (pointerOriginRef.current != null && scrubThresholdPassed.current && dragOriginX != null) {
      const pointerOriginRect = pointerOriginRef.current.getBoundingClientRect()
      const intendedPointerX =
        (pointerOriginRect.left + accumulatedMouseDeltaX.current) % window.screen.width
      simulatedPointerTransformX = intendedPointerX - pointerOriginRect.left
    }

    return (
      <div
        onMouseEnter={onMouseEnter}
        onMouseLeave={onMouseLeave}
        ref={pointerOriginRef}
        style={style}
      >
        <div
          ref={simulatedPointerRef}
          style={{
            width: 5,
            height: 5,
            top: dragOriginY == null ? undefined : dragOriginY,
            transform:
              simulatedPointerTransformX == null
                ? undefined
                : `translateX(${simulatedPointerTransformX}px)`,
            position: 'fixed',
            visibility: scrubThresholdPassed.current ? 'visible' : 'hidden',
            zIndex: 999999,
          }}
        >
          <img
            style={{
              position: 'relative',
              userSelect: 'none',
              display: 'block',
            }}
            width={34}
            height={33}
            src={getPossiblyHashedURL(`/editor/cursors/cursor-ew-resize@2x.png`)}
          />
        </div>
        <div
          className='number-input-container'
          css={{
            color: controlStyles.mainColor,
            zIndex: isFocused ? 3 : undefined,
            position: 'relative',
            borderRadius: UtopiaTheme.inputBorderRadius,
            display: 'flex',
            flexDirection: 'row',
            gap: 5,
            alignItems: 'center',
            boxShadow: 'inset 0px 0px 0px 1px transparent',
            ...chainedStyles,
            '&:hover': {
              boxShadow: `inset 0px 0px 0px 1px ${colorTheme.fg7.value}`,
            },
            '&:focus-within': {
              boxShadow: `inset 0px 0px 0px 1px ${colorTheme.dynamicBlue.value}`,
            },
            '&:hover input': {
              color: controlStyles.mainColor,
            },
            '&:focus-within input': {
              color: controlStyles.mainColor,
            },
          }}
        >
          {incrementControls && !disabled ? (
            <div
              className='number-input-increment-controls'
              css={{
                position: 'absolute',
                top: 0,
                right: 2,
                flexDirection: 'column',
                alignItems: 'stretch',
                width: 11,
                height: UtopiaTheme.layout.inputHeight.default,
                boxShadow: `1px 0 ${controlStyles.borderColor} inset`,
                display: 'none',
                '.number-input-container:hover &': {
                  display: 'block',
                },
                '.number-input-container:focus-within &': {
                  display: 'block',
                },
              }}
            >
              <div
                css={{
                  height: '50%',
                  opacity: 0.6,
                  position: 'relative',
                  borderTopRightRadius: borderRadiusStyles.borderTopRightRadius,
                  ':active': {
                    opacity: 1,
                  },
                  '::after': {
                    content: '""',
                    width: 'calc(100% - 1px)',
                    height: 1,
                    position: 'absolute',
                    right: 1,
                    bottom: 0,
                    transform: 'translateY(0.5px)',
                    pointerEvents: 'none',
                  },
                }}
                onMouseDown={onIncrementMouseDown}
              >
                <Icn category='controls/input' type='up' color='secondary' width={11} height={11} />
              </div>
              <div
                css={{
                  height: '50%',
                  opacity: 0.6,
                  position: 'relative',
                  borderBottomRightRadius: borderRadiusStyles.borderBottomRightRadius,
                  ':active': {
                    opacity: 1,
                  },
                  '::after': {
                    content: '""',
                    width: 'calc(100% - 1px)',
                    height: 1,
                    position: 'absolute',
                    right: 1,
                    bottom: 0,
                    transform: 'translateY(0.5px)',
                    pointerEvents: 'none',
                  },
                }}
                onMouseDown={onDecrementMouseDown}
              >
                <Icn
                  category='controls/input'
                  type='down'
                  color='secondary'
                  width={11}
                  height={11}
                />
              </div>
            </div>
          ) : null}
          {innerLabel == null && controlStatus != 'off' ? null : (
            <div
              data-testid={`${testId}-mouse-down-handler`}
              onMouseDown={onLabelMouseDown}
              style={{
                paddingLeft: 4,
                cursor: disableScrubbing ? 'default' : CSSCursor.ResizeEW,
                fontSize: 9,
                textAlign: 'center',
                display: 'block',
                color: controlStyles.secondaryColor,
                width: 20,
              }}
            >
              {innerLabel}
            </div>
          )}
          <InspectorInput
            {...inputProps}
            chained={chained}
            controlStyles={controlStyles}
            controlStatus={controlStatus}
            testId={testId}
            pasteHandler={pasteHandler}
            disabled={disabled}
            focused={isFocused}
            hasLabel={innerLabel != null}
            roundCorners={roundCorners}
            mixed={mixed}
            value={displayValue}
            ref={ref}
            style={{
              color: colorTheme.fg1.value,
            }}
            css={{
              '::placeholder': { color: invalid ? colorTheme.error.value : undefined },
            }}
            className='number-input'
            height={height}
            id={id}
            placeholder={inputProps.placeholder ?? placeholder}
            onFocus={onFocus}
            onKeyDown={onKeyDown}
            onKeyUp={onKeyUp}
            onBlur={onBlur}
            onChange={onChange}
            autoComplete='off'
          />
        </div>
      </div>
    )
  },
)
NumberInput.displayName = 'NumberInput'

interface SimpleNumberInputProps extends Omit<AbstractNumberInputProps<number>, 'numberType'> {
  onSubmitValue: OnSubmitValueOrEmpty<number>
  onTransientSubmitValue: OnSubmitValueOrEmpty<number>
  onForcedSubmitValue: OnSubmitValueOrEmpty<number>
  setGlobalCursor?: (cursor: CSSCursor | null) => void
}

interface SimpleCSSNumberInputProps extends AbstractNumberInputProps<CSSNumber> {
  onSubmitValue: OnSubmitValueOrEmpty<number>
  onTransientSubmitValue: OnSubmitValueOrEmpty<number>
  onForcedSubmitValue: OnSubmitValueOrEmpty<number>
  setGlobalCursor?: (cursor: CSSCursor | null) => void
}

function wrappedSimpleOnSubmitValue(
  onSubmitValue: OnSubmitValueOrEmpty<number>,
): OnSubmitValueOrUnknownOrEmpty<CSSNumber> {
  return (value) => {
    if (isCSSNumber(value)) {
      onSubmitValue(value.value)
    }
  }
}

export const SimpleNumberInput = React.memo(
  ({
    value,
    onSubmitValue,
    onTransientSubmitValue,
    onForcedSubmitValue,
    ...sharedProps
  }: SimpleNumberInputProps) => {
    const wrappedProps: NumberInputProps = {
      ...sharedProps,
      value: value == null ? null : cssNumber(value),
      onSubmitValue: wrappedSimpleOnSubmitValue(onSubmitValue),
      onTransientSubmitValue: wrappedSimpleOnSubmitValue(onTransientSubmitValue),
      onForcedSubmitValue: wrappedSimpleOnSubmitValue(onForcedSubmitValue),
      numberType: 'Unitless',
    }
    return <NumberInput {...wrappedProps} />
  },
)

export const SimpleCSSNumberInput = React.memo(
  ({
    value,
    onSubmitValue,
    onTransientSubmitValue,
    onForcedSubmitValue,
    numberType,
    ...sharedProps
  }: SimpleCSSNumberInputProps) => {
    const wrappedProps: NumberInputProps = {
      ...sharedProps,
      value: value,
      onSubmitValue: wrappedSimpleOnSubmitValue(onSubmitValue),
      onTransientSubmitValue: wrappedSimpleOnSubmitValue(onTransientSubmitValue),
      onForcedSubmitValue: wrappedSimpleOnSubmitValue(onForcedSubmitValue),
      numberType: 'AnyValid',
    }
    return <NumberInput {...wrappedProps} />
  },
)

interface SimplePercentInputProps extends Omit<AbstractNumberInputProps<number>, 'numberType'> {
  onSubmitValue: OnSubmitValueOrEmpty<number>
  onTransientSubmitValue: OnSubmitValueOrEmpty<number>
  onForcedSubmitValue: OnSubmitValueOrEmpty<number>
}

function wrappedPercentOnSubmitValue(
  onSubmitValue: OnSubmitValueOrEmpty<number>,
): OnSubmitValueOrUnknownOrEmpty<CSSNumber> {
  return (value) => {
    if (isCSSNumber(value)) {
      onSubmitValue(value.value / 100)
    }
  }
}

export const SimplePercentInput = React.memo(
  ({
    value,
    onSubmitValue,
    onTransientSubmitValue,
    onForcedSubmitValue,
    ...sharedProps
  }: SimplePercentInputProps) => {
    const wrappedProps: NumberInputProps = {
      ...sharedProps,
      value: value == null ? null : cssNumber(value * 100, '%'),
      onSubmitValue: wrappedPercentOnSubmitValue(onSubmitValue),
      onTransientSubmitValue: wrappedPercentOnSubmitValue(onTransientSubmitValue),
      onForcedSubmitValue: wrappedPercentOnSubmitValue(onForcedSubmitValue),
      numberType: 'Percent',
    }
    return <NumberInput {...wrappedProps} />
  },
)

interface ChainedNumberControlProps {
  propsArray: Array<Omit<NumberInputProps, 'chained' | 'id'>>
  idPrefix: string
  style?: React.CSSProperties
  setGlobalCursor?: (cursor: CSSCursor | null) => void
  wrap?: boolean
}

export const ChainedNumberInput: React.FunctionComponent<
  React.PropsWithChildren<ChainedNumberControlProps>
> = React.memo(({ propsArray, idPrefix, style, setGlobalCursor, wrap }) => {
  return (
    <FlexRow style={{ flexWrap: wrap ? 'wrap' : 'nowrap', ...style }}>
      {propsArray.map((props, i) => {
        switch (i) {
          case 0: {
            return (
              <NumberInput
                key={`${idPrefix}-${i}`}
                id={`${idPrefix}-${i}`}
                {...props}
                chained='first'
                roundCorners='left'
                setGlobalCursor={setGlobalCursor}
              />
            )
          }
          case propsArray.length - 1: {
            return (
              <NumberInput
                key={`${idPrefix}-${i}`}
                id={`${idPrefix}-${i}`}
                {...props}
                chained='last'
                roundCorners='right'
                setGlobalCursor={setGlobalCursor}
              />
            )
          }
          default: {
            return (
              <NumberInput
                key={`${idPrefix}-${i}`}
                id={`${idPrefix}-${i}`}
                {...props}
                chained='middle'
                roundCorners='none'
                setGlobalCursor={setGlobalCursor}
              />
            )
          }
        }
      })}
    </FlexRow>
  )
})

export function wrappedEmptyOrUnknownOnSubmitValue<T>(
  onSubmitValue: OnSubmitValue<T>,
  onUnsetValue?: OnUnsetValues,
): OnSubmitValueOrUnknownOrEmpty<T> {
  return (value) => {
    if (isEmptyInputValue(value)) {
      if (onUnsetValue != null) {
        onUnsetValue()
      }
    } else if (!isUnknownInputValue(value)) {
      onSubmitValue(value)
    }
  }
}

export function useWrappedEmptyOrUnknownOnSubmitValue<T>(
  onSubmitValue: OnSubmitValue<T>,
  onUnsetValue?: OnUnsetValues,
): OnSubmitValueOrUnknownOrEmpty<T> {
  return React.useCallback(
    (value) => wrappedEmptyOrUnknownOnSubmitValue(onSubmitValue, onUnsetValue)(value),
    [onSubmitValue, onUnsetValue],
  )
}

/**
 * An easy wrapper for the submitValueFactory that applies useWrappedEmptyOrUnknownOnSubmitValue
 * to each submitValue type.
 */
export function useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue<T>(
  submitValueReturn: SubmitValueFactoryReturn<T>,
  onUnsetValue?: OnUnsetValues,
): SubmitValueFactoryReturn<UnknownOrEmptyInput<T>> {
  const index0 = useWrappedEmptyOrUnknownOnSubmitValue(submitValueReturn[0], onUnsetValue)
  const index1 = useWrappedEmptyOrUnknownOnSubmitValue(submitValueReturn[1], onUnsetValue)
  return [index0, index1]
}
