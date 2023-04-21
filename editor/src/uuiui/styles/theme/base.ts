import { createUtopiColor } from '../utopi-color-helpers'

export const base = {
  blue: createUtopiColor('color(display-p3 0 0.59 1)'),
  blue30: createUtopiColor('color(display-p3 0 0.59 1 30%)'),
  purple: createUtopiColor('color(display-p3 0.38 0 1)'),
  purple70: createUtopiColor('color(display-p3 0.38 0 1 70%)'),
  red: createUtopiColor('color(display-p3 1 0 0.38)'),
  orange: createUtopiColor('color(display-p3 0.92 0.64 0)'),
  neonpink: createUtopiColor('color(display-p3 1 0 1)'),
  neonpink30: createUtopiColor('color(display-p3 1 0 1 30%)'),
  neongreen: createUtopiColor('color(display-p3 0 0.93 0.7)'),
  jsYellow: createUtopiColor('#b7b73b'),
  almostBlack: createUtopiColor('hsl(0,0%,10%)'),
  white: createUtopiColor('white'),
  offWhite: createUtopiColor('#eee'),
  black: createUtopiColor('black'),
  darkgray: createUtopiColor('hsl(0, 0%, 50%)'),
  darkorange: createUtopiColor('#D05300'),
  transparent: createUtopiColor('transparent'),
}
