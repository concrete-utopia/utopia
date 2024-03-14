import { recipe } from '@vanilla-extract/recipes'

export const projectCards = recipe({
  base: [
    {
      flexGrow: 1,
      overflow: 'hidden scroll',
      scrollbarColor: '#a1a1a130 transparent',
      display: 'grid',
      gridTemplateColumns: 'repeat(auto-fill, minmax(270px, 1fr))',
      justifyContent: 'space-between',
      alignContent: 'flex-start',
      gridGap: '40px',
      paddingBottom: 30,
    },
  ],
})

export const projectRows = recipe({
  base: [
    {
      flexGrow: 1,
      overflowY: 'scroll',
      scrollbarColor: '#a1a1a130 transparent',
      gap: 10,
      paddingBottom: 30,
    },
  ],
})
