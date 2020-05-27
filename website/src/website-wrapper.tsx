/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/core'
import { Global } from '@emotion/core'
import Navigation from './navigation'

export default class Wrapper extends React.Component {
  render() {
    return (
      <>
        <Global
          styles={{
            body: {
              overflow: 'scroll',
              margin: 0,
            },
          }}
        />
        <div
          id='wrapper'
          style={{
            margin: 0,
            padding: 0,
            paddingBottom: 128,
            width: '100%',
          }}
        >
          <Navigation />
          {this.props.children}
        </div>
      </>
    )
  }
}
