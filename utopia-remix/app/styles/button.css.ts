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
        background: 'transparent',
        ':hover': {
          backgroundColor: '#a4a4a415',
        },
        ':active': {
          backgroundColor: '#a4a4a415',
        },
      },
      subtle: {
        backgroundColor: '#a4a4a420',
        ':hover': {
          backgroundColor: '#a4a4a435',
        },
      },
    },
    size: {
      default: {
        padding: '6px 10px'
      },
      square: {
        padding: 6
      }
    }
  },

  defaultVariants: {
    color: 'neutral',
    size: 'default'
  },
})
