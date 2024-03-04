import { recipe } from '@vanilla-extract/recipes'
import { sprinkles } from './sprinkles.css'

export const newProjectButton = recipe({
  base: [
    sprinkles({
      borderRadius: 'medium',
      color: 'lightModeBlack',
    }),
    {
      border: 'none',
      cursor: 'pointer',
      display: 'flex',
      alignItems: 'center',
      gap: 10,
      fontSize: 16,
      fontWeight: 500,
      color: 'white',
      height: 40,
      opacity: 1,
      ':hover': {
        opacity: 0.8,
      },
    },
  ],

  variants: {
    color: {
      orange: { background: '#FF9243' },
      pink: { background: '#FF43A9' },
      purple: { background: '#9643FF' },
      blue: { background: '#3992FF' },
      green: { background: '#96BF48' },
    },

    size: {
      medium: { padding: '15px 25px' },
    },
  },

  defaultVariants: {
    color: 'orange',
    size: 'medium',
  },
})
