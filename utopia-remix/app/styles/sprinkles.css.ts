import { defineProperties, createSprinkles } from '@vanilla-extract/sprinkles'

const colors = {
  black: '#000',
  white: '#fff',
  blue: '#09f',
}

const colorProperties = defineProperties({
  conditions: {
    lightMode: {},
    darkMode: { '@media': '(prefers-color-scheme: dark)' },
  },
  defaultCondition: 'lightMode',
  properties: {
    color: colors,
    background: colors,
  },
})

const borderRadii = {
  rounded: 4,
  roundedFull: '100%',
}

const borderProperties = defineProperties({
  properties: {
    borderRadius: borderRadii,
  },
})

const shadows = {
  shadow: '0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)',
}

const shadowProperties = defineProperties({
  properties: {
    boxShadow: shadows,
  },
})

export const sprinkles = createSprinkles(colorProperties, borderProperties, shadowProperties)

export type Sprinkles = Parameters<typeof sprinkles>[0]
