import { atom, useAtom, useSetAtom } from 'jotai'
import React from 'react'

export const InspectorWidthAtom = atom<'regular' | 'wide'>('regular')

export interface CanvasControlWithProps<P> {
  control: React.NamedExoticComponent<P>
  props: P
  key: string
}

export const InspectorHoveredCanvasControls = atom<Array<CanvasControlWithProps<any>>>([])
export const InspectorFocusedCanvasControls = atom<Array<CanvasControlWithProps<any>>>([])

type GridProperty = keyof React.CSSProperties // coarse grained

const GridPropertiesSetAtom = atom<GridProperty[]>([])

export function useGridPropertiesSet() {
  const [gridPropsSet] = useAtom(GridPropertiesSetAtom)
  return gridPropsSet
}

export function useSetGridProperty(property: GridProperty, isSet: boolean) {
  const setGridProperties = useSetAtom(GridPropertiesSetAtom)
  React.useEffect(() => {
    setGridProperties((props) => {
      if (isSet && !props.includes(property)) {
        return [...props, property]
      } else {
        return props.filter((p) => p !== property)
      }
    })
  }, [isSet, property, setGridProperties])
}
