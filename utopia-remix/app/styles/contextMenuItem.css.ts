import { recipe } from '@vanilla-extract/recipes'
import { sprinkles } from './sprinkles.css'

export const contextMenuItem = recipe({
  base: [
    sprinkles({
      borderRadius: 'small',
      color: 'lightModeBlack',
    }),
    {
      padding: '4px 4px',
      cursor: 'default',
      border: 'none',
    },
  ],
})
