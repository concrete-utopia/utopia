import React from 'react'
import type { UseSpringProps } from 'react-spring'
import { useSprings } from 'react-spring'
import { useDrag } from 'react-use-gesture'
import { move } from '../../../core/shared/array-utils'
import clamp from 'lodash.clamp'

export type GetStyleForArrayDragItem = (
  order: Array<number>,
  reversed: boolean,
  rowHeight: number,
  down?: boolean,
  options?: {
    currentlyDraggingOriginalIndex: number
    currentlyDraggingTemporaryIndex: number
    y: number
  },
) => (index: number) => UseSpringProps

const getStyleForArrayDragItem: GetStyleForArrayDragItem = (
  order,
  reversed,
  rowHeight: number,
  down = false,
  options?,
) => {
  return (index) => {
    if (down && options != null && index === options.currentlyDraggingOriginalIndex) {
      const currentlyDraggingOriginalIndex = reversed
        ? order.length - options.currentlyDraggingOriginalIndex - 1
        : options.currentlyDraggingOriginalIndex
      return {
        y: currentlyDraggingOriginalIndex * rowHeight + options.y,
        zIndex: 1,
        scale: 0.975,
        immediate: (prop) => prop === 'zIndex' || prop === 'y',
      }
    } else {
      let orderIndex = order.indexOf(index) === -1 ? order.length : order.indexOf(index)
      orderIndex = reversed ? order.length - orderIndex - 1 : orderIndex
      return {
        y: orderIndex * rowHeight,
        zIndex: 0,
        scale: 1,
        immediate: !down,
      }
    }
  }
}

export function useArraySuperControl<T>(
  values: ReadonlyArray<T>,
  onSubmitValue: (newValue: ReadonlyArray<T>, transient?: boolean | undefined) => void,
  rowHeight: number,
  reversed: boolean = false,
) {
  const localDraggingIndex = React.useRef(values.map((_, index) => index))

  const [springs, setSprings] = useSprings(
    values.length,
    getStyleForArrayDragItem(localDraggingIndex.current, reversed, rowHeight),
    [values],
  )

  React.useEffect(() => {
    localDraggingIndex.current = values.map((_, index) => index)
    void setSprings(getStyleForArrayDragItem(localDraggingIndex.current, reversed, rowHeight))
  }, [values, setSprings, reversed, rowHeight])

  const bind = useDrag(({ args: [currentlyDraggingOriginalIndex], down, movement: [, y] }) => {
    const currentlyDraggingTemporaryIndex = localDraggingIndex.current.indexOf(
      currentlyDraggingOriginalIndex,
    )
    const newCurrentlyDraggingTemporaryIndex = clamp(
      Math.round((currentlyDraggingOriginalIndex * rowHeight + (reversed ? -y : y)) / rowHeight),
      0,
      values.length - 1,
    )
    localDraggingIndex.current = move(
      currentlyDraggingTemporaryIndex,
      newCurrentlyDraggingTemporaryIndex,
      localDraggingIndex.current,
    )
    void setSprings(
      getStyleForArrayDragItem(localDraggingIndex.current, reversed, rowHeight, down, {
        currentlyDraggingOriginalIndex: currentlyDraggingOriginalIndex as number,
        currentlyDraggingTemporaryIndex,
        y,
      }),
    )
    // Ensure to only change the ordering if it has actually changed.
    if (!down && currentlyDraggingOriginalIndex !== newCurrentlyDraggingTemporaryIndex) {
      const newValue = move(
        currentlyDraggingOriginalIndex as number,
        newCurrentlyDraggingTemporaryIndex,
        [...values],
      )
      onSubmitValue(newValue)
    }
  })
  return {
    bind,
    springs,
  }
}
