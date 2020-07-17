/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/core'
import { breakpoints, BasicStyledComponent, bodyStyles } from './jobs'
import Wrapper from './website-wrapper'

export default class Team extends React.Component {
  TeamBox: React.FunctionComponent<BasicStyledComponent> = (props) => (
    <div
      css={breakpoints({
        textAlign: ['center', 'left', 'left'],
        width: ['100%', '100%', `33.33%`],
        margin: 'auto 0',
        marginTop: '2rem',
        paddingRight: '1rem',
        display: 'flex',
        flexDirection: ['column', 'row', 'row'],
        alignItems: ['center', 'flex-start', 'flex-start'],
        minWidth: [0, 380, 380],
        ...props.emotion,
      })}
    >
      {props.children}
    </div>
  )

  TeamName: React.FunctionComponent<BasicStyledComponent> = (props) => (
    <span
      css={breakpoints({
        ...props.emotion,
        fontWeight: 600,
      })}
    >
      {props.children}
    </span>
  )

  TeamRole: React.FunctionComponent<BasicStyledComponent> = (props) => (
    <span
      css={breakpoints({
        color: '#157EFB',
        ...props.emotion,
      })}
    >
      {props.children}
    </span>
  )

  TeamHeadshot: React.FunctionComponent<{ src: string }> = (props) => (
    <div
      css={breakpoints({
        backgroundColor: '#4524FB',
        borderRadius: '50%',
        width: 64,
        height: 64,
        backgroundImage: `url('/static/index/team/${props.src}')`,
        backgroundSize: '100% 100%',
        backgroundPosition: 'center center',
        flex: '0 0 64px',
      })}
    />
  )

  TeamBio: React.FunctionComponent<BasicStyledComponent> = (props) => (
    <div
      css={breakpoints({
        ...bodyStyles,
        fontSize: [16, 18, 18],
        marginLeft: '1em',
      })}
    >
      {props.children}
    </div>
  )

  render() {
    return (
      <Wrapper>
        <div
          css={breakpoints({
            maxWidth: 1200,
            margin: '3rem auto',
          })}
        >
          <div
            css={breakpoints({
              display: 'flex',
              flexWrap: 'wrap',
              justifyContent: ['center', 'left', 'left'],
            })}
          >
            <this.TeamBox>
              <this.TeamHeadshot src='malte.jpg' />
              <this.TeamBio>
                <div>
                  <this.TeamName>Malte Nuhn</this.TeamName>{' '}
                  <this.TeamRole>CEO &amp; Founder</this.TeamRole>
                </div>
                <div>
                  CEO Yourvine (team of 15),
                  <br />
                  Ops Lead &amp; PM, LinkedIn
                  <br />
                  Oxford, PPE
                </div>
              </this.TeamBio>
            </this.TeamBox>
            <this.TeamBox>
              <this.TeamHeadshot src='rheese.jpg' />
              <this.TeamBio>
                <div>
                  <this.TeamName>Rheese Burgess</this.TeamName> <this.TeamRole>Eng</this.TeamRole>
                </div>
                <div>
                  Mind Candy Tech Lead
                  <br />
                  Part of BAFTA-Winning Team
                  <br />
                  Imperial SW Eng
                </div>
              </this.TeamBio>
            </this.TeamBox>
            <this.TeamBox>
              <this.TeamHeadshot src='sean.jpg' />
              <this.TeamBio>
                <div>
                  <this.TeamName>Sean Parsons</this.TeamName> <this.TeamRole>Eng</this.TeamRole>
                </div>
                <div>
                  Mindcandy Principal Eng
                  <br />
                  Part of BAFTA-Winning Team
                  <br />
                  Southampton Solent SW Eng
                </div>
              </this.TeamBio>
            </this.TeamBox>
            <this.TeamBox>
              <this.TeamHeadshot src='balazs.jpg' />
              <this.TeamBio>
                <div>
                  <this.TeamName>Balazs Bajorics</this.TeamName> <this.TeamRole>Eng</this.TeamRole>
                </div>
                <div>
                  Prezi Tech Lead, Editor
                  <br />
                  Founder, Hollr
                  <br />
                  Budapest Uni SW Eng
                </div>
              </this.TeamBio>
            </this.TeamBox>
            <this.TeamBox>
              <this.TeamHeadshot src='eni.jpg' />
              <this.TeamBio>
                <div>
                  <this.TeamName>Eniko Demeter</this.TeamName> <this.TeamRole>Eng</this.TeamRole>
                </div>
                <div>
                  Prezi Editor Engineer
                  <br />
                  Self-Taught
                </div>
              </this.TeamBio>
            </this.TeamBox>
            <this.TeamBox>
              <this.TeamHeadshot src='alec.jpg' />
              <this.TeamBio>
                <div>
                  <this.TeamName>Alec Molloy</this.TeamName> <this.TeamRole>Product</this.TeamRole>
                </div>
                <div>
                  Adobe PM
                  <br />
                  Kano PM + Creative Technologist
                </div>
              </this.TeamBio>
            </this.TeamBox>
            <this.TeamBox>
              <this.TeamHeadshot src='balint.jpg' />
              <this.TeamBio>
                <div>
                  <this.TeamName>Balint Gabor</this.TeamName> <this.TeamRole>Eng</this.TeamRole>
                </div>
                <div>
                  Ethereum Foundation
                  <br />
                  Prezi Tech Lead &amp; Eng Manager
                  <br />
                  Budapest Uni SW Eng
                </div>
              </this.TeamBio>
            </this.TeamBox>
          </div>
        </div>
      </Wrapper>
    )
  }
}
