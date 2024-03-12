import { recipe } from '@vanilla-extract/recipes'
import { sprinkles } from './sprinkles.css'

export const projectCategoryButton = recipe({
  base: [
    sprinkles({
      borderRadius: 'large',
    }),
    {
      border: 'none',
      cursor: 'pointer',
      display: 'flex',
      alignItems: 'center',
      gap: 10,
      padding: '3px 14px',
      height: 32,
      fontWeight: 600,
      fontSize: '12px',
    },
  ],

  variants: {
    color: {
      neutral: {
        background: 'transparent',
        ':hover': {
          background: '#0090ff30',
        },
      },
      selected: { background: '#0090ff', color: 'white' },
    },
  },

  defaultVariants: {
    color: 'neutral',
  },
})

export const userName = recipe({
  base: [
    sprinkles({
      borderRadius: 'small',
    }),
    {
      border: 'none',
      fontSize: 12,
      fontWeight: 500,
    },
  ],
})
