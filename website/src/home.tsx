/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import facepaint from 'facepaint'

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

const Paragraph = styled.p({
  paddingTop: '1em',
  paddingBottom: '1em',
})

export default class Home extends React.Component {
  render() {
    return (
      <React.Fragment>
        <style>
          body {'{'} margin: 0; background-color: #C1EDFF; {'}'}
        </style>
        <div
          css={breakpoints({
            width: '100%',
            paddingTop: ['3em', '10em'],
            paddingBottom: '10em',
            paddingLeft: 16,
            paddingRight: 16,
            textAlign: 'center',
            lineHeight: 1.4,
            fontSize: [18, 24],
            fontFamily: "'Inter', sans-serif",
            backgroundColor: '#C1EDFF',
            color: '#094173',
            fontWeight: 100,
          })}
        >
          <div
            style={{
              width: '100%',
              display: 'flex',
              flexDirection: 'column',
              alignItems: 'center',
            }}
          >
            <div style={{ maxWidth: 650, textAlign: 'left' }}>
              <img src='/static/logotype@2x.png' width={180} height={36} />
              <h1
                css={breakpoints({
                  // fontFamily: "'Inter'",
                  fontSize: 62,
                  fontWeight: 900,
                  lineHeight: 1,
                  letterSpacing: -3,
                  textTransform: 'capitalize',
                  paddingBottom: ['0.3em', '0.5em'],
                })}
              >
                Join us building the future of design and code, together.
              </h1>

              <Paragraph>
                We’re looking for experienced <b>core engineers</b> to join us on our journey. If
                any of these are you, we’d love to hear from you: perhaps you’ve contributed to
                Chrome or another browser, React or another declarative UI library, maybe you’ve
                built something that makes use of the Typescript AST, or worked on code editors,
                linters, parsers, lexers, or anything deeply technical that excited you.{' '}
              </Paragraph>

              <Paragraph>
                We’re also looking for a <b>lead creative engineer</b>. Perhaps you have experience
                with creative coding, declarative animations, physics engines, visual scripting
                languages like Max MSP or Origami, or web audio. Perhaps you’ve done work with
                physics-based layout systems, or constraint-based animations. Or you’ve worked along
                the limits of building delightful experiences on low-powered devices, or built
                next-generation web technologies like Houdini.{' '}
              </Paragraph>

              <Paragraph>
                And we’re looking for a <b>product designer</b>. You are comfortable decomposing and
                recomposing complex and complicated problems, read code fluently, and have a
                portfolio of product design work. This is an abstract yet super hands-on role: you
                will think, create, build, and ship. Speed and taste are both essential.
              </Paragraph>

              <Paragraph>
                You’ll all be joining a small, international, fully distributed and pretty
                synchronous team where everyone works on everything. We’re a super welcoming group
                with no egos, an absurd sense of humour, and some of the best music recommendations
                in the industry.
                <br />
                If you’re comfortable hanging out on Slack or Discord, hopping on a screensharing
                session, and love making things, we’re your jam… or your money back 😊{' '}
                <b>hi@utopia.fm</b>
              </Paragraph>
            </div>
          </div>
        </div>
      </React.Fragment>
    )
  }
}
