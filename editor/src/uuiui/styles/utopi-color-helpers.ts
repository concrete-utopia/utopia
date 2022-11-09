import ChromaWrongTypes from 'chroma-js'
const Chroma = ChromaWrongTypes as any

export interface UtopiColor {
  /**
   * css-compatible rgba string: `rgba(255,0,255,1)` format
   */
  value: string

  /**
   * The color palette shade of the base color.
   * @param value 0 is white, 200 is black, 100 is identity
   */
  shade: (value: number) => UtopiColor

  /**
   * Opacity or Alpha. Value goes from 0 to 100
   */
  o: (value: number) => UtopiColor
}

type ColorHex = string

const opacitycache: { [colorHex: string]: { [opacity: string]: ColorHex } } = {}
function opacity(this: UtopiColor, value: number): UtopiColor {
  if (opacitycache[this.value] == null) {
    opacitycache[this.value] = {}
  }
  if (opacitycache[this.value][value] == null) {
    const alpha = value / 100
    opacitycache[this.value][value] = Chroma(this.value).alpha(alpha).css('rgba')
  }
  return createUtopiColor(opacitycache[this.value][value])
}

const utopiColorCache: { [key: string]: UtopiColor } = {}

export function createUtopiColor(baseColor: string): UtopiColor {
  const key = `${baseColor}`
  const fromCache = utopiColorCache[key]
  if (fromCache == null) {
    const hexWithAlpha = Chroma(baseColor).css('rgba')

    const value = {
      value: hexWithAlpha,
      shade: shade,
      o: opacity,
    }
    utopiColorCache[key] = value
    return value
  } else {
    return fromCache
  }
}
