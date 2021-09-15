import * as React from 'react'
import * as ReactDOM from 'react-dom'
import { Global, css } from '@emotion/react'

import { VSCODE_EDITOR_IFRAME_BASE_URL } from '../common/env-vars'
import { createIframeUrl } from '../core/shared/utils'
import { FlexColumn, FlexRow } from '../uuiui'

function VSCodeOuterIframe(): React.ReactElement {
  // TODO: Alternative root handling.
  const urlParams = new URLSearchParams(window.location.search)
  const projectID = urlParams.get('project_id')!
  const baseIframeSrc = createIframeUrl(VSCODE_EDITOR_IFRAME_BASE_URL, 'vscode-editor-inner-iframe')
  const url = new URL(baseIframeSrc)
  url.searchParams.append('project_id', projectID)

  const fakeCode = [
    {
      indent: 0,
      code: "import * as React from 'react'",
    },
    {
      indent: 0,
      code: "import { Scene, Storyboard } from 'utopia-api''",
    },
    {
      indent: 0,
      code: "import { App } from '/src/app.js'",
    },
    {
      indent: 0,
      code: 'export var storyboard = (',
    },
    {
      indent: 1,
      code: '<Storyboard>',
    },
    {
      indent: 2,
      code: '<Scene>',
    },
    {
      indent: 3,
      code: "style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}",
    },
    {
      indent: 2,
      code: '>',
    },
    {
      indent: 3,
      code: '<App />',
    },
    {
      indent: 2,
      code: '</Scene',
    },
    {
      indent: 1,
      code: '</Storyboard>',
    },
    {
      indent: 0,
      code: ')',
    },
  ]

  return (
    <div
      style={{
        fontSize: 13,
        display: 'flex',
        flexDirection: 'column',
        fontFamily: '-apple-system, system-ui, sans-serif',
      }}
    >
      <Global
        styles={css`
          @keyframes placeholderShimmer {
            0% {
              background-position: -468px 0;
            }
            100% {
              background-position: 468px 0;
            }
          }

          .shimmer {
            color: transparent;
            animation-name: placeholderShimmer;
            animation-duration: 1.25s;
            animation-fill-mode: forwards;
            animation-iteration-count: infinite;
            animation-timing-function: linear;
            background: #f6f6f6;
            background: linear-gradient(to right, #f6f6f6 8%, #f0f0f0 18%, #f6f6f6 33%);
            background-size: 800px 104px;
            position: relative;
          }
        `}
      />
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
            gap: 8,
            width: 140,
          }}
        >
          <span style={{ fontFamily: 'seti', fontSize: 19, color: '#b7b73b' }}>JS</span>
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
          gap: 8,
          alignItems: 'center',
          color: '#888',
        }}
      >
        <div className='folder monaco-breadcrumb-item' role='listitem'>
          <div className='monaco-icon-label'>
            <div className='monaco-icon-label-container' title='~/utopia'>
              <span className='monaco-icon-name-container'>
                <a className='label-name'>utopia</a>
              </span>
              <span className='monaco-icon-description-container'></span>
            </div>
          </div>
          <div className='codicon codicon-breadcrumb-separator'></div>
        </div>
        <div className='file monaco-breadcrumb-item' role='listitem'>
          <div className='monaco-icon-label file-icon storyboard.js-name-file-icon js-ext-file-icon ext-file-icon javascript-lang-file-icon'>
            <div className='monaco-icon-label-container' title='~/utopia/storyboard.js'>
              <span className='monaco-icon-name-container'>
                <a className='label-name'>storyboard.js</a>
              </span>
              <span className='monaco-icon-description-container'></span>
            </div>
          </div>
          <div className='codicon codicon-breadcrumb-separator'></div>
        </div>
      </div>
      {/* code */}
      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '66px 500px',
          gridTemplateRows: 'repeat(10, 18px)',
          alignItems: 'center',
          fontSize: 12,
          color: '#6d705b',
          fontFamily: 'Menlo, Monaco, "Courier New", monospace',
        }}
      >
        {fakeCode.map((line, rowNumber) => (
          <React.Fragment>
            <span
              style={{
                textAlign: 'right',
                paddingRight: 27,
              }}
            >
              {rowNumber}
            </span>
            <div>
              <span
                className='shimmer'
                style={{
                  marginLeft: line.indent * 14,
                  wordWrap: 'normal',
                  whiteSpace: 'nowrap',
                  overflow: 'hidden',
                }}
              >
                {line.code}
              </span>
            </div>
          </React.Fragment>
        ))}
      </div>
    </div>

    // <iframe
    //   id={'vscode-outer'}
    //   allow='autoplay'
    //   style={{
    //     flex: 1,
    //     backgroundColor: 'transparent',
    //     borderWidth: 0,
    //   }}
    //   src={url.toString()}
    // />
  )
}

ReactDOM.render(<VSCodeOuterIframe />, document.getElementById('vscode-outer-root'))
