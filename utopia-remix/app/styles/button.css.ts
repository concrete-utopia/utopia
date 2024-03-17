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
        backgroundColor: '#a4a4a430',
        ':hover': {
          backgroundColor: '#a4a4a440',
        },
      },
      selected: {
        backgroundColor: '#a4a4a430',
      },
      transparent: {
        backgroundColor: 'transparent',
      },
    },
    size: {
      default: {
        padding: '0px 10px',
        height: 32,
      },
      square: {
        height: 16,
        width: 16,
        padding: 6,
      },
      ellipses: {
        padding: '0px 6px',
      },
    },
  },

  defaultVariants: {
    color: 'neutral',
    size: 'default',
  },
})
