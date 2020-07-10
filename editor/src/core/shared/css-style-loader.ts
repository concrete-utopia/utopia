export const InjectedCSSFilePrefix = 'injected-css-file-'

export function unimportCSSFile(filename: string): void {
  const head = document.querySelector('head')
  if (head != null) {
    for (let index = 0; index < head.children.length; index++) {
      const child = head.children[index]
      if (child.id.startsWith(InjectedCSSFilePrefix) && child.id.endsWith(filename)) {
        // Finding the exact ID requires knowing the fully resolved module, so until we remove the
        // micro-loader this seems like a good enough solution. Once that has happened we should consider
        // how we could supporting unloading of a file via whatever the new solution is
        child.remove()
      }
    }
  }
}

function createCodeAppendingCss(filename: string, content: string): string {
  return `
  'use strict';
  const filename = ${JSON.stringify(filename)}
  const content = ${JSON.stringify(content)}
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
`
}

export function transformCssSystemModule(filename: string, content: string): string {
  return `
System.register([], function(exports_1, context_1) {
  'use strict'
  
  return {
    setters: [],
    execute: function execute() {
      ${createCodeAppendingCss(filename, content)}
    },
  }
})
`
}

export function transformCssNodeModule(filename: string, content: string): string {
  return `
    Object.defineProperty(module, 'exports', {
      get() {
        // this side effect will run every time the module is required
        (function() {${createCodeAppendingCss(filename, content)}})();
        return {};
      },
    })
`
}
