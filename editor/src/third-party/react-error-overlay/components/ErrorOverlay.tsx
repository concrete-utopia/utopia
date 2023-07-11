/**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* @flow */
import React from 'react'
import { Component, ReactNode } from 'react'
import { colorTheme } from '../../../uuiui'
import { black } from '../styles'

const overlayStyle = {
  pointerEvents: 'initial',
  position: 'relative',
  display: 'inline-flex',
  flexDirection: 'column',
  height: '100%',
  maxWidth: '100%',
  overflowX: 'hidden',
  overflowY: 'auto',
  padding: '0.5rem',
  boxSizing: 'border-box',
  textAlign: 'left',
  fontFamily: 'Consolas, Menlo, monospace',
  fontSize: '11px',
  whiteSpace: 'pre-wrap',
  wordBreak: 'break-word',
  lineHeight: 1.5,
  color: colorTheme.fg2.value,
} as const

type Props = {
  children: ReactNode
  shortcutHandler?: (eventKey: string) => void
}

type State = {
  collapsed: boolean
}

class ErrorOverlay extends Component<Props, State> {
  iframeWindow: Window | null = null

  getIframeWindow = (element: HTMLDivElement | null) => {
    if (element) {
      const document = element.ownerDocument
      this.iframeWindow = document!.defaultView
    }
  }

  onKeyDown = (e: KeyboardEvent) => {
    const { shortcutHandler } = this.props
    if (shortcutHandler) {
      shortcutHandler(e.key)
    }
  }

  componentDidMount() {
    window.addEventListener('keydown', this.onKeyDown)
    if (this.iframeWindow) {
      this.iframeWindow.addEventListener('keydown', this.onKeyDown)
    }
  }

  componentWillUnmount() {
    window.removeEventListener('keydown', this.onKeyDown)
    if (this.iframeWindow) {
      this.iframeWindow.removeEventListener('keydown', this.onKeyDown)
    }
  }

  render() {
    return (
      <div
        style={{ ...overlayStyle, backgroundColor: colorTheme.neutralBackground.value }}
        className='react-error-overlay-allow-text-selection'
        ref={this.getIframeWindow}
      >
        {this.props.children}
      </div>
    )
  }
}

export default ErrorOverlay
