export function transformCssNodeModule(filename: string, content: string): string {
  return `
  'use strict';
  const filename = ${JSON.stringify(filename)}
  const content = ${JSON.stringify(content)}
  const maybeExistingTag = document.getElementById(filename);
  if (maybeExistingTag != null) {
    if (maybeExistingTag.textContent === content) {
      return;
    } else {
      maybeExistingTag.parentElement.removeChild(maybeExistingTag);
    }
  }
  const styleTag = document.createElement("style");
  styleTag.type = "text/css";
  styleTag.id = filename;
  styleTag.appendChild(document.createTextNode(content));
  console.log('### Szia Balint', filename) // TODO DELETE ME :)
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
      ${transformCssNodeModule(filename, content)}
    },
  }
})
`
}
