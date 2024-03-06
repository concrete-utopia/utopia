import { recipe } from '@vanilla-extract/recipes'
import { colors, sprinkles } from './sprinkles.css'

export const projectCards = recipe({
  base: [
    {
      flexGrow: 1,
      overflow: 'hidden scroll',
      scrollbarColor: 'lightgrey transparent',
      display: 'grid',
      gridTemplateColumns: 'repeat(auto-fill, minmax(200px, 1fr))',
      justifyContent: 'space-between',
      alignContent: 'flex-start',
      gridGap: '40px',
      paddingRight: 30,
    },
  ],
})

export const projectRows = recipe({
  base: [
    {
      flexGrow: 1,
      overflowY: 'scroll',
      scrollbarColor: 'lightgrey transparent',
      gap: 10,
    },
  ],
})
