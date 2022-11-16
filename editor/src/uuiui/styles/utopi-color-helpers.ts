import ChromaWrongTypes from 'chroma-js'
const Chroma = ChromaWrongTypes as any

export class UtopiColor {
  /**
   * css variable associated with this color: var(--variable-name) format
   */
  value: string = 'var(--utopitheme-not-set)'
  /**
   * css-compatible rgba string: `rgba(255,0,255,1)` format
   */
  cssValue: string = '--utopitheme-not-set'

  constructor(baseColor: string, path?: string) {
    const hexWithAlpha = Chroma(baseColor).css('rgba')
    const cssVar = '--utopitheme-not-set'

    this.value = `var(${path || cssVar})`
    this.cssValue = hexWithAlpha
    return this
  }

  /**
   * Opacity or Alpha. Value goes from 0 to 100
   */
  o(value: number): UtopiColor {
    const alpha = value / 100
    return new UtopiColor(Chroma(this.cssValue).alpha(alpha).css('rgba'))
  }

  static createUtopiColor(baseColor: string, path?: string): UtopiColor {
    return new UtopiColor(baseColor, path)
  }
}
