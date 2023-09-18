import { createUtopiColor } from '../utopi-color-helpers'

export const base = {
  blue: createUtopiColor('oklch(59% 0.25 254)'),
  blue10: createUtopiColor('oklch(59% 0.25 254 / 10%)'),
  blue30: createUtopiColor('oklch(59% 0.25 254 / 30%)'),
  purple: createUtopiColor('oklch(53% 0.31 290)'),
  purple70: createUtopiColor('oklch(53% 0.31 290 / 70%)'),
  red: createUtopiColor('oklch(66% 0.3 11.65)'),
  orange: createUtopiColor('oklch(83.6% 0.198 81.5)'),
  orange20: createUtopiColor('oklch(83.6% 0.198 81.5 / 20%)'),
  neonpink: createUtopiColor('oklch(72.2% 0.36 331.7)'),
  neonpink30: createUtopiColor('oklch(72.2% 0.36 331.7 / 30%)'),
  neongreen: createUtopiColor('oklch(86.6% 0.27 158.6)'),

  almostBlack: createUtopiColor('oklch(21.56% 0 0)'),
  white: createUtopiColor('oklch(100% 0 0)'),
  offWhite: createUtopiColor('oklch(94.91% 0 0)'),
  black: createUtopiColor('oklch(0% 0 0)'),
  darkgray: createUtopiColor('oklch(59.82% 0 0)'),
  darkorange: createUtopiColor('oklch(63% 0.22 41)'),
  transparent: createUtopiColor('oklch(0% 0 0 / 0%)'),
}
