import React from 'react'
import { ElementPath } from 'src/core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import { fastForEach } from '../../../core/shared/utils'
import { useColorTheme } from '../../../uuiui'
import { useForceUpdate } from '../../editor/hook-utils'
import { OnSubmitValue } from '../controls/control'
import { ControlStatus } from './control-status'
import { CSSBackgroundLayer, CSSTransformItem, CSSUnknownArrayItem } from './css-utils'

const isControlledStyling = (colorTheme: any) => ({
  backgroundColor: colorTheme.primary.shade(5).value,
  color: colorTheme.primary.value,
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

export function usePropControlledState<T>(
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
  transient: boolean,
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

  const [dirty, setDirty] = React.useState(false)

  const onSubmitValueAndUpdateLocalState: OnSubmitValueAndUpdateLocalState<T> = React.useCallback(
    (setStateAction, transient) => {
      const newValue =
        typeof setStateAction === 'function'
          ? (setStateAction as (prevState: T) => T)(localState)
          : setStateAction
      if (transient && onTransientSubmitValue != null) {
        onTransientSubmitValue(newValue)
      } else {
        onSubmitValue(newValue)
      }
      setLocalState(newValue)
      setDirty(true)
    },
    [localState, onSubmitValue, onTransientSubmitValue],
  )

  React.useEffect(() => {
    const propsAndTransformedStateMatch = equalityTest(localState, propValue)
    if (propsAndTransformedStateMatch) {
      setDirty(false)
    } else if (!propsAndTransformedStateMatch && !dirty) {
      setLocalState(propValue)
    }
  }, [localState, propValue, equalityTest, dirty])
  return [localState, onSubmitValueAndUpdateLocalState]
}

export const stopPropagation = (e: React.MouseEvent) => {
  e.stopPropagation()
}

export const preventDefault = (e: React.SyntheticEvent) => {
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
