import * as UtopiaHighVis from './themes/utopia-high-vis-light.json'

import * as NoctisAzureus from './themes/noctis-azureus.json'
import * as Dracula from './themes/dracula.json'
import * as AyuLight from './themes/ayu-light.json'

// https://github.com/jolaleye/horizon-theme-vscode/tree/master/themes
import * as Horizon from './themes/horizon.json'
import * as HorizonBold from './themes/horizon-bold.json'
import * as HorizonBright from './themes/horizon-bright.json'
import * as HorizonBrightBold from './themes/horizon-bright-bold.json'
import * as HorizonBrightItalic from './themes/horizon-bright-italic.json'

// Source: https://github.com/akamud/vscode-theme-onedark/tree/master/themes
// manual fix: changing "white" to hex value, removing "inherit" from color strings
// and adding "type": "dark"
import * as AtomOneDark from './themes/one-dark.json'

// Source: https://github.com/JonaDuran/Material-Light-Theme/tree/master/themes
import * as MaterialColorTheme from './themes/material-color-theme.json'
import * as MaterialDarkColorTheme from './themes/material-dark-color-theme.json'

// Shades of Purple
// Source: https://github.com/ahmadawais/shades-of-purple-vscode/tree/master/themes
// Requires manual removal of comments
import * as ShadesOfPurple from './themes/shades-of-purple-color-theme.json'

// https://github.com/AregGhazaryan/GlassUI-VScode/blob/master/themes/Glass%20UI-color-theme.json
import * as Glass from './themes/glass.json'

// https://github.com/Binaryify/OneDark-Pro/blob/master/themes/OneDark-Pro.json
import * as OneDarkPro from './themes/onedarkpro.json'

// https://github.com/mrpbennett/atlantic-night-vscode-theme/tree/master/themes
// requires manual removal of comments
import * as AtlanticNight from './themes/atlantic-night-color-theme.json'

// https://github.com/johnpapa/vscode-winteriscoming/tree/master/themes
// requires manual fix of three-letter colors like #fff
import * as WinterIsComingLight from './themes/winter-is-coming-light.json'
import * as WinterIsComingBlue from './themes/winter-is-coming-dark-blue.json'

// https://github.com/azemoh/vscode-one-monokai/blob/master/themes/OneMonokai-color-theme.json
import * as OneMonokai from './themes/one-monokai.json'

// https://github.com/whizkydee/vscode-material-palenight-theme/blob/master/themes/palenight.json
// requires replacing of null values with transparent color, I used #FFFFFF00
import * as PaleNight from './themes/palenight.json'

// https://github.com/firefox-theme/visual-studio-code/blob/master/themes/light.json
// a lot of comments in this one
import * as FirefoxLight from './themes/firefox-light.json'

// https://github.com/ryanolsonx/vscode-solarized-theme/tree/master/themes
import * as SolarizedLight from './themes/solarized-light.json'
import * as SolarizedDark from './themes/solarized-dark.json'

// https://github.com/ginfuru/vscode-better-solarized/tree/master/themes
import * as BetterSolarizedLight from './themes/better-solarized-light.json'
import * as BetterSolarizedDark from './themes/better-solarized-dark.json'

// https://github.com/dustypomerleau/new-england/blob/master/themes/new-england.json
// a few comments to nuke
import * as NewEngland from './themes/new-england.json'

// the default vscode theme in light
import * as UtopiaLight from './themes/utopia-light.json'
import * as UtopiaDark from './themes/utopia-dark.json'

export const DefaultTheme = 'utopia-highvis'

const LightThemesAlphabetical = {
  'ayu-light': AyuLight,
  'better-solarized-light': BetterSolarizedLight,
  'firefox-light': FirefoxLight,
  glass: Glass,
  'horizon-bright': HorizonBright,
  'horizon-bright-bold': HorizonBrightBold,
  'horizon-bright-italic': HorizonBrightItalic,
  'material-color-theme': MaterialColorTheme,
  'new-england': NewEngland,
  'solarized-light': SolarizedLight,
  'winter-is-coming-light': WinterIsComingLight,
  'utopia-highvis': UtopiaHighVis,
  'vs-light': UtopiaLight,
}

const DarkThemesAlphabetical = {
  'atlantic-night': AtlanticNight,
  'better-solarized-dark': BetterSolarizedDark,
  dracula: Dracula,
  horizon: Horizon,
  'horizon-bold': HorizonBold,
  'material-dark-color-theme': MaterialDarkColorTheme,
  'noctis-azureus': NoctisAzureus,
  'one-dark': AtomOneDark,
  onedarkpro: OneDarkPro,
  'one-monokai': OneMonokai,
  palenight: PaleNight,
  'shades-of-purple': ShadesOfPurple,
  'solarized-dark': SolarizedLight,
  'winter-is-coming-blue': WinterIsComingBlue,
  'vs-dark': UtopiaDark,
}

export const CodeEditorThemeCollection: Record<string, any> = {
  ...LightThemesAlphabetical,
  ...DarkThemesAlphabetical,
}

export type CodeEditorTheme = keyof typeof CodeEditorThemeCollection
export const CodeEditorThemeList = Object.keys(CodeEditorThemeCollection) as CodeEditorTheme[]

export function getThemeDefinition(name: CodeEditorTheme): any {
  return CodeEditorThemeCollection[name]
}
