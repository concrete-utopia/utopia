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

export default class Prerelease extends React.Component {
  private videoRef: React.RefObject<HTMLVideoElement> = React.createRef()

  Section: React.FunctionComponent<BasicStyledComponent> = (props) => (
    <div
      css={breakpoints({
        paddingBottom: ['2rem', '5rem', '5rem'],
        ...props.emotion,
      })}
    >
      {props.children}
    </div>
  )

  SectionHead: React.FunctionComponent<BasicStyledComponent> = (props) => (
    <div
      css={breakpoints({
        maxWidth: 800,
        margin: 'auto',
        lineHeight: 1,
        padding: '.25em 1rem',
        fontSize: [36, 52, 72],
        letterSpacing: '-0.033em',
        fontWeight: 600,
        fontFamily: 'Circular, "Work Sans", sans-serif',
        ...props.emotion,
      })}
    >
      {props.children}
    </div>
  )

  SubHead: React.FunctionComponent<BasicStyledComponent> = (props) => (
    <div
      css={breakpoints({
        fontSize: [36, 36, 48],
        padding: '0 1rem',
        marginTop: ['1em', 0, 0],
        textAlign: ['center', 'left', 'left'],
        letterSpacing: '-0.033em',
        fontWeight: 600,
        fontFamily: 'Circular, "Work Sans", sans-serif',
        ...props.emotion,
      })}
    >
      {props.children}
    </div>
  )

  BodyText: React.FunctionComponent<BasicStyledComponent> = (props) => (
    <div
      css={breakpoints({
        ...bodyStyles,
        maxWidth: 800,
        marginLeft: 'auto',
        marginRight: 'auto',
        fontFamily: 'Work Sans',
        padding: '0 1rem',
        ...props.emotion,
      })}
    >
      {props.children}
    </div>
  )

  BodyList: React.FunctionComponent<BasicStyledComponent> = (props) => (
    <ul
      css={breakpoints({
        ...bodyStyles,
        maxWidth: 800,
        marginLeft: 'auto',
        marginRight: 'auto',
        fontFamily: 'Work Sans',
        padding: '0 1em 0 2em',
        ...props.emotion,
      })}
    >
      {props.children}
    </ul>
  )

  BulletText: React.FunctionComponent<BasicStyledComponent> = (props) => (
    <div
      css={breakpoints({
        ...bodyStyles,
        textAlign: ['center', 'left', 'left'],
        marginTop: 16,
        margin: 'auto 0',
        padding: '0 1rem',
        fontFamily: 'Work Sans',
        ...props.emotion,
      })}
    >
      {props.children}
    </div>
  )

  CTA: React.FunctionComponent<BasicStyledComponent & { href: string }> = (props) => (
    <a
      href={props.href}
      css={breakpoints({
        fontSize: [20, 24, 24],
        fontFamily: 'Circular, "Work Sans", sans-serif',
        '&, & a:visited': {
          color: '#027aff',
        },
        display: 'block',
        textAlign: 'center',
        marginLeft: 'auto',
        marginRight: 'auto',
        maxwidth: 800,
        marginTop: 16,
        lineHeight: 1.4,
        fontWeight: 600,
        ...props.emotion,
      })}
    >
      {props.children}
    </a>
  )

  // componentDidMount() {
  //   canAutoPlay
  //   .video({timeout: 200, muted: true})
  //   .then(({result, error}: {result: boolean | undefined, error: any}) => {
  //     if (result !== true){
  //       if (this.videoRef.current != null) {
  //         this.videoRef.current.controls = true
  //       }
  //     } else {
  //     }
  //   })

  // }

  render() {
    return (
      <Wrapper>
        <this.Section>
          <this.SectionHead
            emotion={{
              textAlign: 'center',
              margin: '1em auto 1em',
            }}
          >
            Design × Code
          </this.SectionHead>
          <this.BodyText
            emotion={{
              textAlign: 'center',
              margin: '0 auto',
              maxWidth: 600,
            }}
          >
            Utopia is an online environment for building interfaces with design and code.
          </this.BodyText>
          <div
            css={{
              backgroundImage:
                'linear-gradient(to top, rgba(69, 36, 251, 1) 0%, rgba(69, 36, 251, 1) 75%, rgba(69, 36, 251, 0) 75.01%)',
              padding: '2rem 0',
              color: 'white',
            }}
          >
            <img src='https://s3.amazonaws.com/utopia-website-assets/index/hero.png' width='100%' />
          </div>
        </this.Section>
        <this.Section>
          <div
            id='features'
            css={breakpoints({
              display: 'flex',
              flexWrap: 'wrap',
            })}
          >
            <div
              css={breakpoints({
                width: ['100%', '50%', '33.33%'],
              })}
            >
              <img
                src='https://s3.amazonaws.com/utopia-website-assets/index/section-1.png'
                width='100%'
              />
            </div>
            <div
              css={breakpoints({
                width: ['100%', '50%', '66.66%'],
                textAlign: 'left',
                paddingLeft: [0, '3em', '3em'],
              })}
            >
              <this.SectionHead>
                World-class
                <br />
                design tooling
              </this.SectionHead>
              <this.BodyText>
                Utopia is advanced screen design platform, built for speed. Use constraints and
                dynamic layouts, design with generators and data, and style with familiar tools.
              </this.BodyText>
            </div>
          </div>
        </this.Section>
        <this.Section
          emotion={{
            backgroundColor: '#FFF846',
            color: '#444',
            paddingTop: '2rem',
          }}
        >
          <div
            css={breakpoints({
              margin: 'auto',
              maxWidth: 1200,
            })}
          >
            <div
              css={breakpoints({
                display: 'flex',
                flexWrap: 'wrap',
                justifyContent: 'space-around',
              })}
            >
              <div
                css={breakpoints({
                  width: ['100%', '100%', `33.33%`],
                })}
              >
                <this.BodyList>
                  <li>Responsive layouts</li>
                  <li>Layout systems</li>
                  <li>Typography</li>
                  <li>Vector editing (coming soon)</li>
                </this.BodyList>
              </div>
              <div
                css={breakpoints({
                  width: ['100%', '100%', `33.33%`],
                })}
              >
                <this.BodyList>
                  <li>60fps</li>
                  <li>Keyboard shortcuts</li>
                  <li>Zoom</li>
                </this.BodyList>
              </div>
              <div
                css={breakpoints({
                  width: ['100%', '100%', `33.33%`],
                })}
              >
                <this.BodyList>
                  <li>Visual composition</li>
                  <li>Overrides</li>
                  <li>Generative designs</li>
                </this.BodyList>
              </div>
            </div>
          </div>
        </this.Section>

        <this.Section>
          <div
            id='features'
            css={breakpoints({
              display: 'flex',
              flexWrap: 'wrap',
            })}
          >
            <div
              css={breakpoints({
                width: ['100%', '50%', '66.66%'],
                textAlign: 'left',
                paddingLeft: [0, '3em', '3em'],
                marginTop: '2rem',
              })}
            >
              <this.SectionHead>Built for code</this.SectionHead>
              <this.BodyText>
                Utopia is an online coding environment. Create components, edit files, import
                modules. Code editing is powered by Monaco, the open source editor behind Visual
                Studio, and offers syntax highlighting, autocompletion, and syntax correction.
              </this.BodyText>
            </div>
            <div
              css={breakpoints({
                width: ['100%', '50%', '33.33%'],
              })}
            >
              <img
                src='https://s3.amazonaws.com/utopia-website-assets/index/section-3.png'
                width='100%'
              />
            </div>
          </div>
        </this.Section>
        <this.Section>
          <this.SectionHead
            emotion={{
              textAlign: 'center',
            }}
          >
            Everything connects
          </this.SectionHead>
          <this.BodyText
            emotion={{
              textAlign: 'center',
            }}
          >
            Utopia offers a powerful visual scripting environment for rapid prototyping and
            construction.
          </this.BodyText>
          <this.CTA href='/login'>Try Utopia &gt;</this.CTA>
        </this.Section>
      </Wrapper>
    )
  }
}
