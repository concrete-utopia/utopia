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

  setCssColor('control-set-mainColor', colorTheme.inspectorSetMainColor)
  setCssColor('control-set-secondaryColor', colorTheme.inspectorSetSecondaryColor)
  setCssColor('control-set-borderColor', colorTheme.inspectorSetBorderColor)
  setCssColor('control-set-backgroundColor', colorTheme.inspectorSetBackgroundColor)
  setCssColor('control-set-backgroundOnColor', colorTheme.inspectorSetBackgroundOnColor)
  setCssColor('control-set-backgroundOffColor', colorTheme.inspectorSetBackgroundOffColor)
  setCssColor('control-unset-mainColor', colorTheme.inspectorUnsetMainColor)
  setCssColor('control-unset-secondaryColor', colorTheme.inspectorUnsetSecondaryColor)
  setCssColor('control-unset-borderColor', colorTheme.inspectorUnsetBorderColor)
  setCssColor('control-unset-backgroundColor', colorTheme.inspectorUnsetBackgroundColor)
  setCssColor('control-unset-backgroundOnColor', colorTheme.inspectorUnsetBackgroundOffColor)
  setCssColor('control-unset-backgroundOffColor', colorTheme.inspectorUnsetBackgroundOnColor)
  setCssColor('control-off-mainColor', colorTheme.inspectorOffMainColor)
  setCssColor('control-off-secondaryColor', colorTheme.inspectorOffSecondaryColor)
  setCssColor('control-off-backgroundColor', colorTheme.inspectorOffBackgroundColor)
  setCssColor('control-off-backgroundOnColor', colorTheme.inspectorOffBackgroundOnColor)
  setCssColor('control-off-backgroundOffColor', colorTheme.inspectorOffBackgroundOffColor)
  setCssColor('control-off-borderColor', colorTheme.inspectorOffBorderColor)
  setCssColor('control-disabled-mainColor', colorTheme.inspectorDisabledMainColor)
  setCssColor('control-disabled-secondaryColor', colorTheme.inspectorDisabledSecondaryColor)
  setCssColor('control-disabled-backgroundColor', colorTheme.inspectorDisabledBackgroundColor)
  setCssColor('control-disabled-backgroundOnColor', colorTheme.inspectorDisabledBackgroundOnColor)
  setCssColor('control-disabled-backgroundOffColor', colorTheme.inspectorDisabledBackgroundOffColor)
  setCssColor('control-disabled-borderColor', colorTheme.inspectorDisabledBorderColor)
  setCssColor(
    'control-controlled-component-mainColor',
    colorTheme.inspectorControlledComponentMainColor,
  )
  setCssColor(
    'control-controlled-component-borderColor',
    colorTheme.inspectorControlledComponentBorderColor,
  )
  setCssColor(
    'control-controlled-component-backgroundColor',
    colorTheme.inspectorControlledComponentBackgroundColor,
  )
  setCssColor(
    'control-controlled-component-backgroundOnColor',
    colorTheme.inspectorControlledComponentBackgroundOnColor,
  )
  setCssColor(
    'control-controlled-component-backgroundOffColor',
    colorTheme.inspectorControlledComponentBackgroundOffColor,
  )
  setCssColor(
    'control-controlled-nodegraph-mainColor',
    colorTheme.inspectorControlledNodegraphMainColor,
  )
  setCssColor(
    'control-controlled-nodegraph-borderColor',
    colorTheme.inspectorControlledNodegraphBorderColor,
  )
  setCssColor(
    'control-controlled-nodegraph-backgroundColor',
    colorTheme.inspectorControlledNodegraphBackgroundColor,
  )
  setCssColor(
    'control-controlled-nodegraph-backgroundOnColor',
    colorTheme.inspectorControlledNodegraphBackgroundOnColor,
  )
  setCssColor(
    'control-controlled-nodegraph-backgroundOffColor',
    colorTheme.inspectorControlledNodegraphBackgroundOffColor,
  )
  setCssColor('control-uneditable-mainColor', colorTheme.inspectorUneditableMainColor)
  setCssColor('control-uneditable-secondaryColor', colorTheme.inspectorUnsetSecondaryColor)
  setCssColor('focusedColor', colorTheme.inspectorFocusedColor)
}
