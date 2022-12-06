import { createUtopiColor, UtopiColor } from '../utopi-color-helpers'

export type SubThemeObject = {
  name: string
  bg0: UtopiColor
  fg0: UtopiColor
  fg1: UtopiColor
  fg2: UtopiColor
}

export const errorEmphasized = {
  name: 'error-emphasized',
  bg0: createUtopiColor('#FF7759'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const warningEmphasized = {
  name: 'warning-emphasized',
  bg0: createUtopiColor('#FFB859'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const pullLozenge = {
  name: 'pull-lozenge',
  bg0: createUtopiColor('#FFB859'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const pushLozenge = {
  name: 'push-lozenge',
  bg0: createUtopiColor('#49B6FF'),
  fg0: createUtopiColor('#FFFFFF'),
  fg1: createUtopiColor('#D9DCE3'),
  fg2: createUtopiColor('#c9cCc3'),
}

export const mergeConflictLozenge = {
  ...errorEmphasized,
  name: 'merge-conflict-lozenge',
}
