import { CodeFile, UIJSFile, RevisionsState } from '../shared/project-file-types'
import { codeFile, uiJsFile } from './project-file-utils'
import { lintAndParse } from '../workers/parser-printer/parser-printer'

export function getDefaultUIJsFile(): UIJSFile {
  const result = lintAndParse('code.tsx', sampleCode)
  return uiJsFile(result, null, RevisionsState.BothMatch, Date.now())
}

export const sampleCode = `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'
export var App = (props) => {
  return (
    <div
      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    />
  )
}
export var storyboard = (
  <Storyboard>
    <Scene
      component={App}
      props={{}}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    />
  </Storyboard>
)

`

export function getSamplePreviewFile(): CodeFile {
  return codeFile(samplePreviewFile, null)
}

const samplePreviewFile = `import * as React from "react";
import * as ReactDOM from "react-dom";
import { App } from "../src/app";

const root = document.getElementById("root");
if (root != null) {
  ReactDOM.render(<App />, root);
}`

export function getSamplePreviewHTMLFile(): CodeFile {
  return codeFile(previewHtml, null)
}

/** If you change these two values, please change them in src/templates/preview.html too */
export const generatedExternalResourcesLinksOpen = '<!-- Begin Generated Utopia External Links -->'
export const generatedExternalResourcesLinksClose = '<!-- End Generated Utopia External Links -->'

export const previewHtml = `<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Utopia React App</title>
    ${generatedExternalResourcesLinksOpen}
    ${generatedExternalResourcesLinksClose}
  </head>
  <body>
    <div id="root"></div>
  </body>
</html>`
