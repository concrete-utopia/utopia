import { recipe } from '@vanilla-extract/recipes'
import { colors, sprinkles } from './sprinkles.css'

export const projectCards = recipe({
  base: [
    {
      flexGrow: 1,
      overflow: 'hidden scroll',
      scrollbarColor: 'lightgrey transparent',
      display: 'grid',
      gridTemplateColumns: 'repeat(auto-fill, 280px)',
      justifyContent: 'space-between',
      gridGap: 30,
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
