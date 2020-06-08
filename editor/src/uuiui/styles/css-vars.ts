import { UtopiColor } from './utopi-color-helpers'
import { colorTheme } from './theme'

function setCssColor(name: string, value: UtopiColor) {
  setGlobalCssVar(name, value.value)
}

function setGlobalCssVar(name: string, value: string) {
  document.documentElement!.style.setProperty(`--${name}`, value)
}

export function updateCssVars() {
  // Inspector vars
  // react-contexify still needs this
  setCssColor('contextmenu-background', colorTheme.contextMenuBackground)
  setCssColor('contextmenu-foreground', colorTheme.contextMenuForeground)
  setCssColor('contextmenu-highlightBackground', colorTheme.contextMenuHighlightBackground)
  setCssColor('contextmenu-highlightForeground', colorTheme.contextMenuHighlightForeground)
  setCssColor('contextmenu-separator', colorTheme.contextMenuSeparator)

  setCssColor('focusedColor', colorTheme.inspectorFocusedColor)
}
