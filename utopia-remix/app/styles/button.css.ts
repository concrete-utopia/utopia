import { recipe } from '@vanilla-extract/recipes'
import { sprinkles } from './sprinkles.css'

export const button = recipe({
  base: [
    sprinkles({
      borderRadius: 'small',
    }),
    {
      outline: 'none',
      border: 'none',
      boxShadow: 'none',
      cursor: 'pointer',
      display: 'flex',
      flexDirection: 'row',
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
      neutral: {
        padding: '6px 12px',
        background: 'transparent',
        ':hover': {
          backgroundColor: '#a4a4a415',
        },
        ':active': {
          backgroundColor: '#a4a4a415',
        },
      },
      brand: { background: 'grey' },
    },
  },

  defaultVariants: {
    color: 'neutral',
  },
})
