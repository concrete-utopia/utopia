import ChromaWrongTypes from 'chroma-js'
import { clamp } from '../../core/shared/math-utils'
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

  /**
   * Label. A friendly name for the color + shade combo.
   */
  label: string
  /**
   * Use this when generating file names, eg for icons in a specific colorway. Ensure you also create those!
   */
  fileNameFragment: string
}

type ColorHex = string
type ColorShades = Array<ColorHex>

const colorShadeCache: { [colorHex: string]: ColorShades } = {}
function shade(this: UtopiColor, value: number): UtopiColor {
  if (colorShadeCache[this.value] == null) {
    const alpha = Chroma(this.value).rgba()[3]
    const whiteWithOpacity = Chroma('white').alpha(alpha)
    const blackWithOpacity = Chroma('black').alpha(alpha)
    // shades go from 0 to 200, so we ask Chroma to create a 201-long color range
    colorShadeCache[this.value] = Chroma.scale([
      whiteWithOpacity,
      this.value,
      blackWithOpacity,
    ]).colors(201)
  }
  const index = clamp(0, 200, Math.floor(value))
  return createUtopiColor(colorShadeCache[this.value][index], this.label, this.fileNameFragment)
}

const opacitycache: { [colorHex: string]: { [opacity: string]: ColorHex } } = {}
function opacity(this: UtopiColor, value: number): UtopiColor {
  if (opacitycache[this.value] == null) {
    opacitycache[this.value] = {}
  }
  if (opacitycache[this.value][value] == null) {
    const alpha = value / 100
    opacitycache[this.value][value] = Chroma(this.value).alpha(alpha).css('rgba')
  }
  return createUtopiColor(opacitycache[this.value][value], this.label, this.fileNameFragment)
}

const utopiColorCache: { [key: string]: UtopiColor } = {}

export function createUtopiColor(
  baseColor: string,
  label: string,
  fileNameFragment: string,
): UtopiColor {
  const key = `${baseColor}-#-${label}-#-${fileNameFragment}`
  const fromCache = utopiColorCache[key]
  if (fromCache == null) {
    const hexWithAlpha = Chroma(baseColor).css('rgba')
    const value = {
      value: hexWithAlpha,
      shade: shade,
      o: opacity,
      label: label,
      fileNameFragment: fileNameFragment,
    }
    utopiColorCache[key] = value
    return value
  } else {
    return fromCache
  }
}
