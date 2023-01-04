import { createUtopiColor } from '../utopi-color-helpers'
import { PartialThemeObject } from './types'

export const luminousPartial: PartialThemeObject = {
  name: 'push-lozenge',
  iconColor: createUtopiColor('on-light-main'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}
