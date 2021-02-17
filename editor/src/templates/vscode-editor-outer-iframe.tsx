import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { MONACO_EDITOR_IFRAME_BASE_URL } from '../common/env-vars'
import { createIframeUrl } from '../core/shared/utils'

function VSCodeOuterIframe(): React.ReactElement {
  // TODO: Alternative root handling.
  const urlParams = new URLSearchParams(window.location.search)
  const projectID = urlParams.get('project_id')!
  const baseIframeSrc = createIframeUrl(MONACO_EDITOR_IFRAME_BASE_URL, 'vscode-editor-inner-iframe')
  const url = new URL(baseIframeSrc)
  url.searchParams.append('project_id', projectID)
  return (
    <iframe
      id={'vscode-outer'}
      allow='autoplay'
      style={{
        flex: 1,
        backgroundColor: 'transparent',
        borderWidth: 0,
      }}
      src={url.toString()}
    />
  )
}

ReactDOM.render(<VSCodeOuterIframe />, document.getElementById('vscode-outer-root'))
