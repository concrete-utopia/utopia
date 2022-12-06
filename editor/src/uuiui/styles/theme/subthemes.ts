import { createUtopiColor } from '../utopi-color-helpers'
import { SubThemeObject } from './types'

export const errorEmphasized: SubThemeObject = {
  name: 'error-emphasized',
  bg0: createUtopiColor('#FF7759'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const warningEmphasized: SubThemeObject = {
  name: 'warning-emphasized',
  bg0: createUtopiColor('#FFB859'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const pullLozenge: SubThemeObject = {
  name: 'pull-lozenge',
  bg0: createUtopiColor('#FFB859'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const pushLozenge: SubThemeObject = {
  name: 'push-lozenge',
  bg0: createUtopiColor('#49B6FF'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const mergeConflictLozenge: SubThemeObject = {
  ...errorEmphasized,
  name: 'merge-conflict-lozenge',
}
