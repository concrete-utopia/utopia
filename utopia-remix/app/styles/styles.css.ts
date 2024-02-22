import { style } from '@vanilla-extract/css'
import { sprinkles } from './sprinkles.css'

export const styles = {
  root: style([
    {
      fontFamily: 'Inter, sans-serif',
      fontSize: '11px',
    },
    sprinkles({
      backgroundColor: {
        lightMode: 'white',
        darkMode: 'darkModeBlack',
      },
      color: {
        lightMode: 'lightModeBlack',
        darkMode: 'white',
      },
    }),
  ]),
}
