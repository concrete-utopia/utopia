import { createUtopiColor } from '../utopi-color-helpers'
import { base } from './base'
import { SubThemeObject } from './types'

export const navigatorItemHighlighted: SubThemeObject = {
  name: 'navigator-item-highlighted',
  iconColor: 'main',
  bg0: createUtopiColor('rgba(87, 75, 226,0.7)'),
  fg0: createUtopiColor('rgba(255,255,255,1)'),
  fg1: createUtopiColor('rgba(255,255,255,0.8)'),
  fg2: createUtopiColor('rgba(255,255,255,0.7)'),
  error: base.red,
  warning: createUtopiColor('#FFB859'),
}

export const pullLozenge: SubThemeObject = {
  name: 'pull-lozenge',
  iconColor: 'main',
  bg0: createUtopiColor('#FFB859'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
  error: base.red,
  warning: createUtopiColor('#FFB859'),
}

export const pushLozenge: SubThemeObject = {
  name: 'push-lozenge',
  iconColor: 'main',
  bg0: createUtopiColor('#49B6FF'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
  error: base.red,
  warning: createUtopiColor('#FFB859'),
}

export const mergeConflictLozenge: SubThemeObject = {
  ...pushLozenge,
  name: 'merge-conflict-lozenge',
  iconColor: 'main',
}
