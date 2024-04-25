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

export const projectTemplate = recipe({
  base: [
    {
      border: '2px solid transparent',
      borderRadius: '10px',
      overflow: 'hidden',
      height: '100%',
      aspectRatio: '1.6 / 1',
      background:
        'linear-gradient(rgba(161, 161, 161, 0.19), rgba(161, 161, 161, 0.082)) no-repeat local',
      position: 'relative',
      width: '100%',
      ':hover': {
        border: '2px solid rgb(0, 144, 255)',
      },
    },
  ],
})
