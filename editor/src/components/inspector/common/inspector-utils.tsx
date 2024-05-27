import React from 'react'
import type { ElementPath } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import { assertNever, fastForEach } from '../../../core/shared/utils'
import { Icn, SquareButton, useColorTheme } from '../../../uuiui'
import { useForceUpdate } from '../../editor/hook-utils'
import type { OnSubmitValue } from '../controls/control'
import type { ControlStatus } from './control-status'
import type { CSSBackgroundLayer, CSSTransformItem, CSSUnknownArrayItem } from './css-utils'
import type { ControlMode } from '../sections/layout-section/layout-system-subsection/split-chained-number-input'
import { useRefEditorState } from '../../editor/store/store-hook'
import { wrapValue } from '../../../core/shared/math-utils'

const isControlledStyling = (colorTheme: any) => ({
  color: colorTheme.dynamicBlue.value,
})

const isNotControlledStyling = {
  backgroundColor: undefined,
  color: undefined,
}

export function useGetSubsectionHeaderStyle(controlStatus: ControlStatus): React.CSSProperties {
  const colorTheme = useColorTheme()
  // TODO instead of this, make the inspector hook return the `PropertyStatus` too, and just use PropertyStatus.controlled
  const isControlled =
    controlStatus === 'controlled' ||
    controlStatus === 'multiselect-controlled' ||
    controlStatus === 'unoverwritable' ||
    controlStatus === 'multiselect-unoverwritable'

  return isControlled ? isControlledStyling(colorTheme) : isNotControlledStyling
}

export type CSSArrayItem = CSSBackgroundLayer | CSSTransformItem | CSSUnknownArrayItem

export function getIndexedSpliceArrayItem<T extends CSSArrayItem>(index: number) {
  return function spliceArrayItem(_: any, oldValue: ReadonlyArray<T>): ReadonlyArray<T> {
    let newArrayItems = [...oldValue]
    newArrayItems.splice(index, 1)
    return newArrayItems
  }
}

const forceUpdateFunction = (value: number) => value + 1

/**
 * @deprecated Please use usePropControlledStateV2 instead.
 */
export function usePropControlledState_DEPRECATED<T>(
  propValue: T,
): [T, React.Dispatch<T>, React.DispatchWithoutAction] {
  const [localState, setLocalState] = React.useState<T>(propValue)
  const [forceUpdateValue, forceUpdate] = React.useReducer(forceUpdateFunction, 0)
  React.useEffect(() => {
    setLocalState(propValue)
  }, [propValue, forceUpdateValue])
  return [localState, setLocalState, forceUpdate]
}

export function usePropControlledStateV2<T>(propValue: T): [T, React.Dispatch<T>] {
  const previousPropValueRef = React.useRef<T>(propValue)
  const localStateRef = React.useRef<T>(propValue)

  // if the prop changes, update the local state ref. there is no need to force a re-render, because we are already in a render phase with the new props
  if (propValue !== previousPropValueRef.current) {
    localStateRef.current = propValue
    previousPropValueRef.current = propValue
  }

  const forceUpdate = useForceUpdate()

  const setLocalState = React.useCallback(
    (newValue: T) => {
      localStateRef.current = newValue
      forceUpdate()
    },
    [forceUpdate],
  )

  return [localStateRef.current, setLocalState]
}

export type ReadonlyRef<T> = {
  readonly current: T
}

/** please only use this if you absolutely know what you are doing */
export function usePropControlledRef_DANGEROUS<T>(propValue: T): ReadonlyRef<T> {
  const ref = React.useRef(propValue)
  ref.current = propValue
  return ref
}

export type TransformedStateAndPropsEqualityTest<T> = (newStateValue: T, newPropValue: T) => boolean

export type OnSubmitValueAndUpdateLocalState<T> = (
  setStateAction: React.SetStateAction<T>,
  dragState: 'dragStart' | 'drag' | 'dragEnd' | 'notDragging',
) => void

/**
 * Hook with similar functionality to `usePropControlledState`, but with:
 * 1. a custom equality check so local state can be checked against the model.
 * 2. returns a streamlined setter that updates local state and the model.
 *
 * An example: in the gradient stop picker we need to keep stop index constant
 * even when stops' positions don't match their array index order. This means
 * props (which are ordered during printing) won't necessarily match the stops'
 * order. The @equalityTest parameter lets us order state stops to see if the
 * props value could be derived from the state value.
 */
export function useModelControlledTransformableState<T>(
  propValue: T,
  equalityTest: TransformedStateAndPropsEqualityTest<T>,
  onSubmitValue: OnSubmitValue<T>,
  onTransientSubmitValue?: OnSubmitValue<T>,
): [T, OnSubmitValueAndUpdateLocalState<T>] {
  const [localState, setLocalState] = React.useState<T>(propValue)
  const [isDragging, setIsDragging] = React.useState(false)

  const onSubmitValueAndUpdateLocalState: OnSubmitValueAndUpdateLocalState<T> = React.useCallback(
    (setStateAction, dragState) => {
      const newValue =
        typeof setStateAction === 'function'
          ? (setStateAction as (prevState: T) => T)(localState)
          : setStateAction
      if ((dragState === 'drag' || dragState === 'notDragging') && onTransientSubmitValue != null) {
        onTransientSubmitValue(newValue)
      } else {
        onSubmitValue(newValue)
      }
      setLocalState(newValue)

      switch (dragState) {
        case 'dragStart':
        case 'drag':
          setIsDragging(true)
          break
        case 'dragEnd':
        case 'notDragging':
        default:
          setIsDragging(false)
      }
    },
    [localState, onSubmitValue, onTransientSubmitValue],
  )

  React.useEffect(() => {
    const propsAndTransformedStateMatch = equalityTest(localState, propValue)
    if (!propsAndTransformedStateMatch && !isDragging) {
      setLocalState(propValue)
    }
  }, [localState, propValue, equalityTest, isDragging])
  return [localState, onSubmitValueAndUpdateLocalState]
}

export const stopPropagation = (e: { stopPropagation: () => void }) => {
  e.stopPropagation()
}

export const preventDefault = (e: { preventDefault: () => void }) => {
  e.preventDefault()
}

export const useHandleCloseOnESCOrEnter = (closePopup: (key: 'Escape' | 'Enter') => void): void => {
  const handleCloseOnESCOrEnter = React.useCallback(
    (e: KeyboardEvent) => {
      if (e.key === 'Escape' || e.key === 'Enter') {
        e.stopPropagation()
        if (closePopup != null) {
          closePopup(e.key)
        }
      }
    },
    [closePopup],
  )

  React.useEffect(() => {
    document.addEventListener('keydown', handleCloseOnESCOrEnter)
    return () => {
      document.removeEventListener('keydown', handleCloseOnESCOrEnter)
    }
  }, [handleCloseOnESCOrEnter])
}

export function clampString(value: string, maxLength: number) {
  return value.length > maxLength ? `${value.substring(0, maxLength)}…` : value
}

// TODO this function needs a better name :)
export function getElementsToTarget(paths: Array<ElementPath>): Array<ElementPath> {
  let result: Array<ElementPath> = []
  fastForEach(paths, (path) => {
    if (!EP.containsPath(path, result)) {
      result.push(path)
    }
  })
  return result
}

export type CycleDirection = 'forward' | 'backward'

function deltaFromDirection(direction: CycleDirection): number {
  switch (direction) {
    case 'backward':
      return -1
    case 'forward':
      return 1
    default:
      assertNever(direction)
  }
}

export function useControlModeWithCycle(
  initialValue: ControlMode,
  modes: Array<ControlMode>,
): [
  ControlMode | null,
  (mode: ControlMode | null, dir: CycleDirection) => void,
  React.DispatchWithoutAction,
] {
  const [controlMode, setControlMode] = usePropControlledStateV2<ControlMode | null>(initialValue)

  const cycleToNextMode = React.useCallback(
    (mode: ControlMode | null, direction: CycleDirection) => {
      const modeToUse = controlMode ?? mode ?? initialValue
      const delta = deltaFromDirection(direction)
      const index = modes.indexOf(modeToUse) + delta
      setControlMode(modes[wrapValue(index, 0, modes.length - 1)])
    },
    [initialValue, modes, controlMode, setControlMode],
  )

  const resetMode = React.useCallback(() => setControlMode(null), [setControlMode])

  return [controlMode, cycleToNextMode, resetMode]
}

export interface RemovePropertyButtonProps {
  onUnsetValues: () => void
  propertySet: boolean
  testId: string
}

export function RemovePropertyButton({
  testId,
  onUnsetValues,
  propertySet,
}: RemovePropertyButtonProps) {
  if (!propertySet) {
    return null
  }

  return (
    <SquareButton highlight onMouseDown={onUnsetValues} data-testid={testId} style={{ width: 12 }}>
      <Icn category='semantic' type='cross' width={12} height={12} />
    </SquareButton>
  )
}
