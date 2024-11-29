import { convertCssToUtopia } from '../shared/css-utils'
import type { LoadModule, MatchFile, ModuleLoader } from './loader-types'
import { loadModuleResult } from './loader-types'

export const InjectedCSSFilePrefix = 'injected-css-file-'

const matchFile: MatchFile = (filename: string) => {
  return filename.endsWith('.css')
}

const loadModule: LoadModule = (filename: string, contents: string) => {
  const canvasScopedCSS = convertCssToUtopia(contents)

  const loadedContents = `
    Object.defineProperty(module, 'exports', {
      get() {
        // this side effect will run every time the module is required
        (function() {
          'use strict';
          const filename = ${JSON.stringify(filename)}
          const content = ${JSON.stringify(canvasScopedCSS)}
          const elementId = ${JSON.stringify(InjectedCSSFilePrefix)} + filename;
          const maybeExistingTag = document.getElementById(elementId);
          if (maybeExistingTag != null) {
            if (maybeExistingTag.textContent === content) {
              return;
            } else {
              maybeExistingTag.parentElement.removeChild(maybeExistingTag);
            }
          }
          const styleTag = document.createElement("style");
          styleTag.type = "text/css";
          styleTag.id = elementId;
          styleTag.appendChild(document.createTextNode(content));
          document.querySelector("head").appendChild(styleTag);
        })();
        return {};
      },
    })
  `

  // FIXME Replace CSS custom evaluation from evaluator.ts with this
  return loadModuleResult(filename + '.js', loadedContents)
}

export const CSSLoader: ModuleLoader = {
  match: matchFile,
  load: loadModule,
}

// FIXME This doesn't quite fit in here. We need a generalised strategy for unloading files when they're removed from the list of imports
export function unimportAllButTheseCSSFiles(filesToKeep: Array<string>): void {
  const head = document.querySelector('head')
  const shouldBeRemoved = (elemId: string) =>
    elemId.startsWith(InjectedCSSFilePrefix) &&
    !filesToKeep.some((filename) => elemId.endsWith(filename))
  if (head != null) {
    let inlinedCSSImportsToRemove: Array<Element> = []
    for (let index = 0; index < head.children.length; index++) {
      const child = head.children[index]
      if (shouldBeRemoved(child.id)) {
        inlinedCSSImportsToRemove.push(child)
      }
    }

    for (let index = inlinedCSSImportsToRemove.length - 1; index >= 0; index--) {
      // Done in reverse and separately to the above to save mutating an array whilst trying to read from it based on index
      inlinedCSSImportsToRemove[index].remove()
    }
  }
}
