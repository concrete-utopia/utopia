import ChromaWrongTypes from 'chroma-js'
const Chroma = ChromaWrongTypes as any

export interface UtopiColor {
  /**
   * css-compatible rgba string: `rgba(255,0,255,1)` format
   */
  cssValue: string

  /**
   * css variable associated with this color: val(--variable-name) format
   */
  value: string

  /**
   * Opacity or Alpha. Value goes from 0 to 100
   */
  o: (value: number) => UtopiColor
}

type ColorHex = string

const opacitycache: { [colorHex: string]: { [opacity: string]: ColorHex } } = {}
function opacity(this: UtopiColor, value: number): UtopiColor {
  if (opacitycache[this.cssValue] == null) {
    opacitycache[this.cssValue] = {}
  }
  if (opacitycache[this.cssValue][value] == null) {
    const alpha = value / 100
    opacitycache[this.cssValue][value] = Chroma(this.cssValue).alpha(alpha).css('rgba')
  }
  return createUtopiColor(opacitycache[this.cssValue][value])
}

const utopiColorCache: { [key: string]: UtopiColor } = {}

export function createUtopiColor(baseColor: string): UtopiColor {
  const key = `${baseColor}`
  const fromCache = utopiColorCache[key]
  if (fromCache == null) {
    const hexWithAlpha = Chroma(baseColor).css('rgba')
    const cssVar = '--null'

    const value = {
      value: `var(${cssVar})`,
      cssValue: hexWithAlpha,
      o: opacity,
    }
    utopiColorCache[key] = value
    return value
  } else {
    return fromCache
  }
}
