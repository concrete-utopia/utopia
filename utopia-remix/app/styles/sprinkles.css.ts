import { defineProperties, createSprinkles } from '@vanilla-extract/sprinkles'

export const colors = {
  black: '#000',
  white: '#fff',
  primary: '#0075F9',
  secondary: '#E5E5E5',
  aqua: '#00E3E3',
  darkModeBlack: '#181C20',
  lightModeBlack: '#2B2B2B',
  separator: '#dddddd',
  error: '#993344',
}

const colorProperties = defineProperties({
  conditions: {
    lightMode: {},
    darkMode: { '@media': '(prefers-color-scheme: dark)' },
  },
  defaultCondition: 'lightMode',
  properties: {
    color: colors,
    backgroundColor: colors,
  },
})

const borderRadii = {
  small: 3,
  medium: 10,
  large: 30,
  full: '100%',
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

const margins = {
  big: 30,
  medium: 20,
  small: 10,
}

const marginProperties = defineProperties({
  properties: {
    margin: margins,
  },
})

export const sprinkles = createSprinkles(
  colorProperties,
  borderProperties,
  shadowProperties,
  marginProperties,
)

export type Sprinkles = Parameters<typeof sprinkles>[0]
