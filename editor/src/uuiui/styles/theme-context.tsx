import React from 'react'
import { colorTheme, useColorTheme } from '..'
import { useEditorState } from '../../components/editor/store/store-hook'
import { propsStyleIsSpreadInto } from '../../core/model/element-template-utils'

const ThemeContext = React.createContext<typeof colorTheme | null>(null)
ThemeContext.displayName = 'ThemeContext'

export const ThemeProvider: React.FC<{ children?: React.ReactNode }> = (props) => {
  const theme: typeof colorTheme = useColorTheme()

  return <ThemeContext.Provider value={theme}>{props.children}</ThemeContext.Provider>
}

export const ThemeConsumer = ThemeContext.Consumer

export default ThemeContext
