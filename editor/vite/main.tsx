import React from 'react'
import ReactDOM from 'react-dom'
import { EditorRoot } from './editor-fast-entry'
// import Buffer from 'buffer'
// ;(window as any).Buffer = Buffer

ReactDOM.render(
  <React.StrictMode>
    <EditorRoot />
  </React.StrictMode>,
  document.getElementById('root'),
)
