import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { MONACO_EDITOR_IFRAME_BASE_URL } from '../common/env-vars'
import { createIframeUrl } from '../core/shared/utils'

function VSCodeOuterIframe(): React.ReactElement {
  // TODO: Alternative root handling.
  const iframeSrc = createIframeUrl(
    MONACO_EDITOR_IFRAME_BASE_URL,
    'vscode-editor-inner-iframe.html',
  )
  return (
    <iframe
      id={'vscode-outer'}
      allow='autoplay'
      style={{
        flex: 1,
        backgroundColor: 'transparent',
        borderWidth: 0,
      }}
      src={iframeSrc}
    />
  )
}

ReactDOM.render(<VSCodeOuterIframe />, document.getElementById('vscode-outer-root'))
