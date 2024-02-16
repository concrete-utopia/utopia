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
      padding: '0px 5px',
      cursor: 'pointer',
      border: 'none !important',
      height: 25,
      display: 'flex',
      alignItems: 'center',
      userSelect: 'none',
      ':hover': {
        backgroundColor: colors.primary,
        color: 'white',
      },
    },
  ],
})
