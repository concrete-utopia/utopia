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
      small: { padding: 8, fontSize: '.9em' },
      medium: { padding: 10, fontSize: '1em' },
      large: { padding: 16, fontSize: '1.1em' },
    },
  },

  compoundVariants: [
    {
      variants: {
        color: 'neutral',
        size: 'large',
      },
      style: {
        background: 'ghostwhite',
      },
    },
  ],

  defaultVariants: {
    color: 'brand',
    size: 'medium',
  },
})
