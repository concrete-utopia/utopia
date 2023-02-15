import { atom } from 'jotai'

export const InspectorWidthAtom = atom<'regular' | 'wide'>('regular')

export interface CanvasControlWithProps<P> {
  control: React.NamedExoticComponent<P>
  props: P
  key: string
}

export const InspectorHoveredCanvasControls = atom<Array<CanvasControlWithProps<any>>>([])
export const InspectorFocusedCanvasControls = atom<Array<CanvasControlWithProps<any>>>([])
