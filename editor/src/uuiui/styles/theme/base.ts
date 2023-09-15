import { createUtopiColor } from '../utopi-color-helpers'

export const base = {
  blue: createUtopiColor('lch(50 85 280 / 1)'),
  blue10: createUtopiColor('lch(50 85 280 / 10%)'),
  blue30: createUtopiColor('lch(50 85 280 / 30%)'),
  purple: createUtopiColor('lch(39 129 307 / 1)'),
  purple70: createUtopiColor('lch(39 129 307 / 70%)'),
  purple05: createUtopiColor('lch(39 129 307 / 5%)'),
  red: createUtopiColor('lch(57 100 16 / 1)'),
  orange: createUtopiColor('lch(81 111 79 / 1)'),
  orange20: createUtopiColor('lch(81 111 79 / 20%)'),
  neonpink: createUtopiColor('lch(62 122 330 / 1)'),
  neonpink30: createUtopiColor('lch(62 122 330 / 30%)'),
  neongreen: createUtopiColor('lch(88 100 157)'),
  jsYellow: createUtopiColor('#b7b73b'),
  almostBlack: createUtopiColor('hsl(0,0%,10%)'),
  white: createUtopiColor('white'),
  offWhite: createUtopiColor('#eee'),
  black: createUtopiColor('black'),
  darkgray: createUtopiColor('hsl(0, 0%, 50%)'),
  darkorange: createUtopiColor('lch(55 109.96 54.75)'),
  transparent: createUtopiColor('transparent'),
}
