/**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* @flow */
import React from 'react'
import { Component, ElementType as ReactElement } from 'react'
import { black } from '../styles'

const _collapsibleStyle = {
  color: black,
  cursor: 'pointer',
  border: 'none',
  display: 'block',
  width: '100%',
  textAlign: 'left',
  background: '#fff',
  fontFamily: 'Consolas, Menlo, monospace',
  fontSize: '1em',
  padding: '0px',
  lineHeight: '1.5',
} as const

const collapsibleCollapsedStyle = {
  ..._collapsibleStyle,
  marginBottom: '1.5em',
}

const collapsibleExpandedStyle = {
  ..._collapsibleStyle,
  marginBottom: '0.6em',
}

type Props = { children?: React.ReactNode }

type State = {
  collapsed: boolean
}

class Collapsible extends Component<Props, State> {
  state = {
    collapsed: true,
  }

  toggleCollapsed = () => {
    this.setState((state) => ({
      collapsed: !state.collapsed,
    }))
  }

  render() {
    const count = React.Children.count(this.props.children)
    const collapsed = this.state.collapsed
    return (
      <div>
        <button
          onClick={this.toggleCollapsed}
          style={collapsed ? collapsibleCollapsedStyle : collapsibleExpandedStyle}
        >
          {(collapsed ? '▶' : '▼') +
            ` ${count} stack frames were ` +
            (collapsed ? 'collapsed.' : 'expanded.')}
        </button>
        <div style={{ display: collapsed ? 'none' : 'block' }}>
          {this.props.children}
          <button onClick={this.toggleCollapsed} style={collapsibleExpandedStyle}>
            {`▲ ${count} stack frames were expanded.`}
          </button>
        </div>
      </div>
    )
  }
}

export default Collapsible
