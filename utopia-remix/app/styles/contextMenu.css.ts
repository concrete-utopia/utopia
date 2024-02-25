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
      transition: `.1s all ease-in-out`,
      ':hover': {
        backgroundColor: colors.primary,
        color: 'white',
      },
    },
  ],
})

export const contextMenuDropdown = recipe({
  base: [
    sprinkles({
      borderRadius: 'small',
      backgroundColor: 'white',
      color: 'lightModeBlack',
    }),
    {
      padding: 4,
      boxShadow: '2px 3px 4px #00000030',
      border: '1px solid #ccc',
      display: 'flex',
      flexDirection: 'column',
      gap: 4,
      minWidth: 170,
      position: 'relative',
    },
  ],
})
