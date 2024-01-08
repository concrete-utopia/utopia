import type { TextFile } from '../shared/project-file-types'
import {
  codeFile,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../shared/project-file-types'

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
  return textFile(
    textFileContents(sampleAppJSCode, unparsed, RevisionsState.CodeAhead),
    null,
    null,
    0,
  )
}

export function getDefaultUIJsFile(): TextFile {
  return textFile(textFileContents(sampleCode, unparsed, RevisionsState.CodeAhead), null, null, 0)
}

export const sampleCode = `
import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
  registerModule,
} from 'utopia-api'
import { App } from '/src/app.js'
export var storyboard = (
  <Storyboard data-uid='sample-storyboard'>
    <Scene
      data-uid='sample-scene'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='sample-app' />
    </Scene>
  </Storyboard>
)

`

export function getEmptyUIJsFile(): TextFile {
  return textFile(
    textFileContents(emptyUiJsSampleCode, unparsed, RevisionsState.CodeAhead),
    null,
    null,
    0,
  )
}

export const emptyUiJsSampleCode = `
import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
  registerModule,
} from 'utopia-api'
export var storyboard = (
  <Storyboard data-uid='sample-storyboard'>
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
  ReactDOM.render(<App data-uid='preview-app' />, root);
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
