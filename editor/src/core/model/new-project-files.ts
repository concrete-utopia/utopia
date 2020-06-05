import { CodeFile, UIJSFile, RevisionsState } from '../shared/project-file-types'
import { codeFile, uiJsFile } from './project-file-utils'
import { lintAndParse } from '../workers/parser-printer/parser-printer'

export function getDefaultUIJsFile(): UIJSFile {
  const result = lintAndParse('code.tsx', sampleCode)
  return uiJsFile(result, null, RevisionsState.BothMatch, Date.now())
}

export const sampleCode = `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, View, jsx } from 'utopia-api'
export var App = (props) => {
  return (
    <View
      style={{ ...(props.style || {}), backgroundColor: '#FFFFFF' }}
      layout={{ layoutSystem: 'pinSystem' }}
    />
  )
}
export var storyboard = (
  <Storyboard layout={{ layoutSystem: 'pinSystem' }}>
    <Scene
      component={App}
      props={{ style: { top: 0, left: 0, bottom: 0, right: 0 } }}
      style={{ left: 0, top: 0, width: 375, height: 812 }}
      layout={{ layoutSystem: 'pinSystem' }}
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

const previewHtml = `<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Utopia React App</title>
  </head>
  <body>
    <div id="root"></div>
  </body>
</html>`
