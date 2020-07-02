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
          width: '100%',
          paddingTop: '10em',
          paddingBottom: '10em',
          textAlign: 'center',
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          lineHeight: 1.2,
          fontSize: 18,
          fontFamily: "'Overpass Mono', monospace",
          backgroundColor: '#FF6464',
          color: '#230055',
        }}
      >
        <div style={{ maxWidth: 650, textAlign: 'left' }}>
          <h1
            style={{
              fontFamily: "'Overpass Mono', monospace",
              fontSize: 24,
            }}
          >
            Join us building the future of{' '}
            <b style={{ backgroundColor: 'white' }}>design and code</b>, together.
          </h1>

          <p>
            We’re looking for experienced <b style={{ backgroundColor: 'white' }}>core engineers</b>{' '}
            to join us on our journey. If any of these are you, we’d love to hear from you: perhaps
            you’ve contributed to Chrome or another browser, React or another declarative UI
            library, maybe you’ve built something that makes use of the Typescript AST, or worked on
            code editors, linters, parsers, lexers, or anything deeply technical that excited you.{' '}
          </p>

          <p>
            We’re also looking for a{' '}
            <b style={{ backgroundColor: 'white' }}>lead creative engineer</b>. Perhaps you have
            experience with creative coding, declarative animations, physics engines, visual
            scripting languages like Max MSP or Origami, or web audio. Perhaps you’ve done work with
            physics-based layout systems, or constraint-based animations. Or you’ve worked along the
            limits of building delightful experiences on low-powered devices, or built
            next-generation web technologies like Houdini.{' '}
          </p>

          <p>
            And we’re looking for a <b style={{ backgroundColor: 'white' }}>product designer</b>.
            You are comfortable decomposing and recomposing complex and complicated problems, read
            code fluently, and have a portfolio of product design work. This is an abstract yet
            super hands-on role: you will think, create, build, and ship. Speed and taste are both
            essential.
          </p>

          <p>
            You’ll all be joining a small, international, fully distributed and pretty synchronous
            team where everyone works on everything. We’re a super welcoming group with no egos, an
            absurd sense of humour, and some of the best music recommendations in the industry.
            <br />
            If you’re comfortable hanging out on Slack or Discord, hopping on a screensharing
            session, and love making things, we’re your jam… or your money back 😊{' '}
            <b style={{ backgroundColor: 'white' }}>hi@utopia.fm</b>
          </p>
        </div>
      </div>
    )
  }
}
