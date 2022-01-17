import { atom, useAtom } from 'jotai'
import { SetStateAction } from 'react'
import { canvasPoint, CanvasVector, roundPointTo } from '../../core/shared/math-utils'
import { BaseCanvasOffsetLeftPane } from '../editor/store/editor-state' // TODO move var here instead of import

export const CanvasOffsetAtom = atom<CanvasVector>(canvasPoint(BaseCanvasOffsetLeftPane))
export let CanvasOffset: CanvasVector = BaseCanvasOffsetLeftPane

type UpdateFunction<T> = (prevState: T) => T

export function useCanvasOffset(): [CanvasVector, (update: UpdateFunction<CanvasVector>) => void] {
  const [canvasOffset, setCanvasOffset] = useAtom(CanvasOffsetAtom)
  return [
    canvasOffset,
    (update: UpdateFunction<CanvasVector>): void => {
      setCanvasOffset((oldCanvasOffset) => {
        const newCanvasOffset = update(oldCanvasOffset)
        CanvasOffset = newCanvasOffset
        return newCanvasOffset
      })
    },
  ]
}

export function useRoundedCanvasOffset(): [
  CanvasVector,
  (update: UpdateFunction<CanvasVector>) => void,
] {
  const [canvasOffset, setCanvasOffset] = useCanvasOffset()
  return [roundPointTo(canvasOffset, 0), setCanvasOffset]
}
