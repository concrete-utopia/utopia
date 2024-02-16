import { recipe } from '@vanilla-extract/recipes'
import { sprinkles } from './sprinkles.css'

export const button = recipe({
  base: [
    sprinkles({
      borderRadius: 'medium',
      color: 'white',
      boxShadow: 'shadow',
    }),
    {
      border: 'none',
      cursor: 'pointer',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      gap: 4,
      ':disabled': {
        opacity: 0.5,
        cursor: 'not-allowed',
      },
    },
  ],

  variants: {
    color: {
      neutral: { background: 'whitesmoke' },
      brand: { background: 'grey' },
      accent: { background: 'lime' },
      danger: { background: 'red' },
    },
    size: {
      small: { padding: '6px 12px', },
      medium: { padding: 10, },
      large: { padding: 16,  },
    },
  },

  defaultVariants: {
    color: 'brand',
    size: 'medium',
  },
})
