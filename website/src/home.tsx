/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/core'
import facepaint from 'facepaint'
import canAutoPlay from 'can-autoplay'
import Wrapper from './website-wrapper'

export interface BasicStyledComponent {
  emotion?: Record<string, any>
}

const breakpointLocations = ['40em', '52em', '64em', '80em']

export const bodyStyles = {
  fontFamily: 'Work Sans, sans-serif',
  fontSize: [18, 22, 24],
  lineHeight: 1.4,
  fontWeight: 400,
}

export const breakpoints = facepaint(breakpointLocations.map((bp) => `@media (min-width: ${bp})`))

export default class Home extends React.Component {
  render() {
    return (
      <div
        style={{
          textAlign: 'center',
          width: '100%',
          paddingTop: '10em',
        }}
      >
        <a
          className='logo'
          href='/'
          css={breakpoints({
            display: 'inline-block',
            textDecoration: 'none',
            backgroundImage: [
              'url("/static/index/logo-smiangle-64x64@2x.png")',
              'url("/static/index/logo-smiangle-96x96@2x.png")',
              'url("/static/index/logo-smiangle-96x96@2x.png")',
            ],
            backgroundSize: '100%',
            width: [64, 96, 96],
            height: [64, 96, 96],
            flex: ['0 0 64px', '0 0 96px', '0 0 96px'],
          })}
        >
          &nbsp;
        </a>
      </div>
    )
  }
}
