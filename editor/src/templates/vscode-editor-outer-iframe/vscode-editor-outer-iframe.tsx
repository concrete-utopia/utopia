import React from 'react'
import * as ReactDOM from 'react-dom'
import { createRoot } from 'react-dom/client'

import { VSCODE_EDITOR_IFRAME_BASE_URL } from '../../common/env-vars'
import { createIframeUrl } from '../../core/shared/utils'
import { setBranchNameFromURL } from '../../utils/branches'

function VSCodeOuterIframe(): React.ReactElement {
  // TODO: Alternative root handling.
  const urlParams = new URLSearchParams(window.location.search)
  const projectID = urlParams.get('project_id')!
  const baseIframeSrc = createIframeUrl(
    VSCODE_EDITOR_IFRAME_BASE_URL,
    'vscode-editor-inner-iframe/',
  )
  const url = new URL(baseIframeSrc)
  url.searchParams.append('project_id', projectID)
  setBranchNameFromURL(url.searchParams)
  return (
    <iframe
      id={'vscode-outer'}
      allow='autoplay'
      style={{
        flex: 1,
        backgroundColor: 'transparent',
        borderWidth: 0,
        position: 'absolute',
        top: 0,
        left: 0,
        height: '100%',
        width: '100%',
      }}
      src={url.toString()}
    />
  )
}

const container = document.getElementById('vscode-outer-root')
const root = createRoot(container!)
root.render(<VSCodeOuterIframe />)
