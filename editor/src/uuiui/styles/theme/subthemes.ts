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
}

export const errorForeground: SubThemeObject = {
  name: 'error-foreground',
  iconColor: 'error',
  bg0: createUtopiColor('transparent'),
  fg0: base.red,
  fg1: createUtopiColor('rgba(253, 0, 59, 0.8)'),
  fg2: createUtopiColor('rgba(253, 0, 59, 0.6)'),
}

export const errorEmphasized: SubThemeObject = {
  name: 'error-emphasized',
  iconColor: 'main',
  bg0: createUtopiColor('#FF7759'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const warningEmphasized: SubThemeObject = {
  name: 'warning-emphasized',
  iconColor: 'main',
  bg0: createUtopiColor('#FFB859'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const pullLozenge: SubThemeObject = {
  name: 'pull-lozenge',
  iconColor: 'main',
  bg0: createUtopiColor('#FFB859'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const pushLozenge: SubThemeObject = {
  name: 'push-lozenge',
  iconColor: 'main',
  bg0: createUtopiColor('#49B6FF'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const mergeConflictLozenge: SubThemeObject = {
  ...errorEmphasized,
  name: 'merge-conflict-lozenge',
  iconColor: 'main',
}
