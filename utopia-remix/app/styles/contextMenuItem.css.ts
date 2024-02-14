import { recipe } from '@vanilla-extract/recipes'
import { colors, sprinkles } from './sprinkles.css'

export const contextMenuItem = recipe({
  base: [
    sprinkles({
      borderRadius: 'small',
      color: 'lightModeBlack',
    }),
    {
      outline: 'none',
      padding: '6px 8px',
      cursor: 'pointer',
      border: 'none !important',
      ':hover': {
        backgroundColor: colors.primary,
        color: 'white',
      },
    },
  ],
})
