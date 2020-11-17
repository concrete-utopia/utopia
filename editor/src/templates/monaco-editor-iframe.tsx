import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { STATIC_BASE_URL } from '../common/env-vars'
import { CodeEditorEntryPoint } from '../components/code-editor/code-editor-container'
import { addStyleSheetToPage } from '../core/shared/dom-utils'

addStyleSheetToPage(`${STATIC_BASE_URL}editor/fonts.css`, true)
addStyleSheetToPage(`${STATIC_BASE_URL}editor/css/light/monaco-overrides.css`, true)

ReactDOM.render(<CodeEditorEntryPoint />, document.getElementById('code-editor-root'))
