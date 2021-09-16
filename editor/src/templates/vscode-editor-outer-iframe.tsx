import * as React from 'react'
import * as ReactDOM from 'react-dom'

import { VSCODE_EDITOR_IFRAME_BASE_URL } from '../common/env-vars'
import { createIframeUrl } from '../core/shared/utils'
import { setBranchNameFromURL } from '../utils/branches'

const Chevron = () => (
  <svg
    width='16'
    height='16'
    viewBox='0 0 16 16'
    xmlns='http://www.w3.org/2000/svg'
    fill='currentColor'
  >
    <path
      fillRule='evenodd'
      clipRule='evenodd'
      d='M10.072 8.024L5.715 3.667l.618-.62L11 7.716v.618L6.333 13l-.618-.619 4.357-4.357z'
    />
  </svg>
)

const JSIcon = () => (
  <svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32' width={19} height={19}>
    <path
      fill='#F1DD3F'
      d='M11.4 10h2.7v7.6c0 3.4-1.6 4.6-4.3 4.6-.6 0-1.5-.1-2-.3l.3-2.2c.4.2.9.3 1.4.3 1.1 0 1.9-.5 1.9-2.4V10zm5.1 9.2c.7.4 1.9.8 3 .8 1.3 0 1.9-.5 1.9-1.3s-.6-1.2-2-1.7c-2-.7-3.3-1.8-3.3-3.6 0-2.1 1.7-3.6 4.6-3.6 1.4 0 2.4.3 3.1.6l-.6 2.2c-.5-.2-1.3-.6-2.5-.6s-1.8.5-1.8 1.2c0 .8.7 1.1 2.2 1.7 2.1.8 3.1 1.9 3.1 3.6 0 2-1.6 3.7-4.9 3.7-1.4 0-2.7-.4-3.4-.7l.6-2.3z'
    />
  </svg>
)

const VSCodeLoadingScreen = () => (
  <div
    style={{
      fontSize: 13,
      display: 'flex',
      flexDirection: 'column',
      fontFamily: '-apple-system, system-ui, sans-serif',
    }}
  >
    {/* tab row */}
    <div
      style={{
        display: 'flex',
        alignItems: 'flex-end',
        height: 35,
        background: '#f3f3f3',
      }}
    >
      {/* single tab */}
      <div
        style={{
          background: '#FAFAFA',
          color: 'rgb(51,51,51)',
          borderRight: '1px solid rgb(243, 243, 243)',
          height: 35,
          display: 'flex',
          alignItems: 'center',
          paddingLeft: 12,
          paddingRight: 12,
          width: 140,
        }}
      >
        <div style={{ color: '#b7b73b', display: 'flex' }}>
          <JSIcon />
        </div>
        <span style={{}}>storyboard.js</span>
      </div>
    </div>
    {/* breadcrumbs */}
    <div
      className='monaco-breadcrumbs'
      style={{
        paddingLeft: 15,
        height: 22,
        display: 'flex',
        alignItems: 'center',
        color: '#888',
      }}
    >
      <div
        className='folder monaco-breadcrumb-item'
        role='listitem'
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <div
          className='monaco-icon-label'
          style={{
            height: 22,
            lineHeight: '22px',
            alignItems: 'center',
            justifyContent: 'center',
          }}
        >
          <div className='monaco-icon-label-container' title='~/utopia'>
            <span className='monaco-icon-name-container'>
              <a className='label-name'>utopia</a>
            </span>
            <span className='monaco-icon-description-container'></span>
          </div>
        </div>
        <div style={{ height: 16 }}>
          <Chevron />
        </div>
      </div>
      <div
        className='file monaco-breadcrumb-item'
        role='listitem'
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <div
          className='monaco-icon-label file-icon storyboard.js-name-file-icon js-ext-file-icon ext-file-icon javascript-lang-file-icon'
          style={{
            paddingRight: 6,
            height: 22,
            lineHeight: '22px',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
          }}
        >
          <div style={{ color: '#b7b73b', display: 'flex', paddingRight: 4 }}>
            <JSIcon />
          </div>
          <div className='monaco-icon-label-container' title='~/utopia/storyboard.js'>
            <span className='monaco-icon-name-container'>
              <a className='label-name'>storyboard.js</a>
            </span>
            <span className='monaco-icon-description-container'></span>
          </div>
          <div style={{ height: 16 }}>
            <Chevron />
          </div>
        </div>
      </div>
    </div>
  </div>
)

function VSCodeOuterIframe(): React.ReactElement {
  // TODO: Alternative root handling.
  const urlParams = new URLSearchParams(window.location.search)
  const projectID = urlParams.get('project_id')!
  const baseIframeSrc = createIframeUrl(VSCODE_EDITOR_IFRAME_BASE_URL, 'vscode-editor-inner-iframe')
  const url = new URL(baseIframeSrc)
  url.searchParams.append('project_id', projectID)
  setBranchNameFromURL(url.searchParams)
  return (
    <React.Fragment>
      <VSCodeLoadingScreen />
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
    </React.Fragment>
  )
}

ReactDOM.render(<VSCodeOuterIframe />, document.getElementById('vscode-outer-root'))
