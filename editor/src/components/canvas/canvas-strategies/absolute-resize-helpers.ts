export type AbsolutePin = 'left' | 'top' | 'right' | 'bottom' | 'width' | 'height'

export const horizontalPins: Array<AbsolutePin> = ['left', 'width', 'right']
export const verticalPins: Array<AbsolutePin> = ['top', 'height', 'bottom']

export function hasAtLeastTwoPinsPerSide(props: { [key: string]: any }): boolean {
  return (
    horizontalPins.filter((pin) => props.style?.[pin] != null).length >= 2 &&
    verticalPins.filter((pin) => props.style?.[pin] != null).length >= 2
  )
}
