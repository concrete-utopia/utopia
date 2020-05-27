import { loadWASM } from 'onigasm' // peer dependency of 'monaco-textmate'
import { Registry } from 'monaco-textmate' // peer dependency
import { wireTmGrammars } from 'monaco-editor-textmate'

// these language definitions are from vscode
import * as JSReactTM from './tmLanguages/JavaScriptReact.tmLanguage.json'
import * as TSReactTM from './tmLanguages/TypeScriptReact.tmLanguage.json'

const WASMFilePath = '/editor/onigasm.wasm'

// load it only once to the monaco editor after the first render.
let isLoaded = false
export async function liftOff(monaco: any) {
  const load = async () => {
    await loadWASM(WASMFilePath)

    const registry = new Registry({
      getGrammarDefinition: async (scopeName: string) => {
        if (scopeName === 'source.tsx') {
          return {
            format: 'json',
            content: TSReactTM,
          }
        } else {
          return {
            format: 'json',
            content: JSReactTM,
          }
        }
      },
    })

    // map of monaco "language id's" to TextMate scopeNames
    // https://github.com/NeekSandhu/monaco-editor-textmate#limitation
    const grammars = new Map()
    grammars.set('typescript', 'source.tsx')
    grammars.set('javascript', 'source.js.jsx')

    await wireTmGrammars(monaco, registry, grammars)
  }

  if (isLoaded) {
    return
  } else {
    isLoaded = true
    load()
  }
}
