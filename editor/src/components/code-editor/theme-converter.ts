import * as monaco from 'monaco-editor'

function getScopeAsArray(scope: any): string[] {
  if (Array.isArray(scope)) {
    return scope
  } else if (typeof scope === 'string') {
    const scopes: string[] = scope.split(',')
    // removing all whitespaces
    return scopes.map((s) => s.trim())
  } else {
    return []
  }
}

interface TokenColorization {
  scope: string[] | string | undefined
  settings: {
    foreground?: string
    background?: string
    fontStyle?: string // italic, underline, bold
  }
}

export function convertVSThemeToMonacoTheme(theme: any): monaco.editor.IStandaloneThemeData {
  // convert vscode tokenColors to monaco themerules
  const tokenColors: TokenColorization[] = theme.tokenColors ?? []
  let rules: monaco.editor.ITokenThemeRule[] = []
  tokenColors.forEach((token) => {
    const tokenRules = getScopeAsArray(token.scope).map((scope) => ({
      token: scope,
      ...token.settings,
    }))
    rules.push(...tokenRules)
  })

  return {
    base: theme.type === 'dark' ? 'vs-dark' : 'vs',
    inherit: true,
    colors: theme.colors,
    rules: rules,
  }
}
