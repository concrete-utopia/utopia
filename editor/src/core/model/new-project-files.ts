import {
  codeFile,
  isParseSuccess,
  RevisionsState,
  textFile,
  TextFile,
  textFileContents,
} from '../shared/project-file-types'
import { emptySet } from '../shared/set-utils'
import { lintAndParse } from '../workers/parser-printer/parser-printer'

export const sampleAppJSCode = `
import * as React from 'react'
export var App = (props) => {
  return (
    <div
      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF', position: 'relative' }}
    />
  )
}`

export function appJSFile(): TextFile {
  const result = lintAndParse('/src/app.js', sampleAppJSCode, null, emptySet())
  return textFile(
    textFileContents(sampleAppJSCode, result, RevisionsState.BothMatch),
    null,
    isParseSuccess(result) ? result : null,
    Date.now(),
  )
}

export function getDefaultUIJsFile(): TextFile {
  const result = lintAndParse('code.tsx', sampleCode, null, emptySet())
  return textFile(
    textFileContents(sampleCode, result, RevisionsState.BothMatch),
    null,
    isParseSuccess(result) ? result : null,
    Date.now(),
  )
}

export const sampleCode = `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
export var storyboard = (
  <Storyboard>
    <Scene
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App />
    </Scene>
  </Storyboard>
)

`

export function getSamplePreviewFile(): TextFile {
  return codeFile(samplePreviewFile, null)
}

const samplePreviewFile = `import * as React from "react";
import * as ReactDOM from "react-dom";
import { App } from "../src/app";

const root = document.getElementById("root");
if (root != null) {
  ReactDOM.render(<App />, root);
}`

export function getSamplePreviewHTMLFile(): TextFile {
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
