import { recipe } from '@vanilla-extract/recipes'
import { sprinkles } from './sprinkles.css'

export const text = recipe({
  variants: {
    size: {
      default: { fontSize: 11 },
      small: { fontSize: 12 },
      medium: { fontSize: 16, fontWeight: 600 },
    },
  },

  defaultVariants: {
    size: 'small',
  },
})
