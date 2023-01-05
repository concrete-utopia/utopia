import { createUtopiColor } from '../utopi-color-helpers'
import { PartialThemeObject } from './types'

export const lightNeutralPartial: PartialThemeObject = {
  name: 'light-neutral-partial',
  iconColor: createUtopiColor('on-light-main'),
  fg0: createUtopiColor('rgba(255,255,255,1)'),
  fg1: createUtopiColor('rgba(255,255,255,0.8)'),
  fg2: createUtopiColor('rgba(255,255,255,0.7)'),
}

export const lightInversePartial: PartialThemeObject = {
  name: 'light-inverse-partial',
  iconColor: createUtopiColor('on-light-main'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}
