import '@emotion/react'
import { colorTheme } from '../uuiui'

type UtopiaTheme = typeof colorTheme
declare module '@emotion/react' {
  export interface Theme extends UtopiaTheme {}
}
