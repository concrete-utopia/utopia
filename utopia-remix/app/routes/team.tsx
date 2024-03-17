import type { LinksFunction } from '@remix-run/node'
import * as React from 'react'

import stylesheet from '~/styles/next-tailwind.css'

export const links: LinksFunction = () => [
  // css
  { rel: 'stylesheet', href: stylesheet },
]

const TeamBox = (props: { children: React.ReactNode }) => (
  <div className='flex gap-4 text-xl leading-7 my-4 pr-4 min-w-[380px]'>{props.children}</div>
)

const TeamName = (props: { children: React.ReactNode }) => (
  <span className='font-semibold'>{props.children}</span>
)

const TeamRole = (props: { children: React.ReactNode }) => (
  <span className='text-blue-500'>{props.children}</span>
)

const TeamHeadshot = (props: { src: string }) => (
  <div
    className='rounded-full'
    style={{
      width: 64,
      height: 64,
      backgroundImage: `url('/team/${props.src}')`,
      backgroundSize: '100% 100%',
      backgroundPosition: 'center center',
      flex: '0 0 64px',
    }}
  />
)

const TeamBio = (props: { children: React.ReactNode }) => <div style={{}}>{props.children}</div>

const TeamPage = React.memo(() => {
  return (
    <div
      style={{
        maxWidth: 1200,
        margin: '3rem auto',
        padding: 20,
      }}
    >
      <div
        style={{
          display: 'flex',
          flexWrap: 'wrap',
          justifyContent: 'left',
        }}
      >
        <TeamBox>
          <TeamHeadshot src='malte.jpg' />
          <TeamBio>
            <div>
              <TeamName>Malte Nuhn</TeamName> <TeamRole>CEO &amp; Founder</TeamRole>
            </div>
            <div>
              CEO Yourvine (team of 15),
              <br />
              Ops Lead &amp; PM, LinkedIn
              <br />
              Oxford, PPE
            </div>
          </TeamBio>
        </TeamBox>
        <TeamBox>
          <TeamHeadshot src='rheese.jpg' />
          <TeamBio>
            <div>
              <TeamName>Rheese Burgess</TeamName> <TeamRole>Eng</TeamRole>
            </div>
            <div>
              Mind Candy Tech Lead
              <br />
              Part of BAFTA-Winning Team
              <br />
              Imperial SW Eng
            </div>
          </TeamBio>
        </TeamBox>
        <TeamBox>
          <TeamHeadshot src='sean.jpg' />
          <TeamBio>
            <div>
              <TeamName>Sean Parsons</TeamName> <TeamRole>Eng</TeamRole>
            </div>
            <div>
              Mindcandy Principal Eng
              <br />
              Part of BAFTA-Winning Team
              <br />
              Southampton Solent SW Eng
            </div>
          </TeamBio>
        </TeamBox>
        <TeamBox>
          <TeamHeadshot src='balazs.jpg' />
          <TeamBio>
            <div>
              <TeamName>Balazs Bajorics</TeamName> <TeamRole>Eng</TeamRole>
            </div>
            <div>
              Prezi Tech Lead, Editor
              <br />
              Founder, Hollr
              <br />
              Budapest Uni SW Eng
            </div>
          </TeamBio>
        </TeamBox>
        <TeamBox>
          <TeamHeadshot src='eni.jpg' />
          <TeamBio>
            <div>
              <TeamName>Eniko Demeter</TeamName> <TeamRole>Eng</TeamRole>
            </div>
            <div>
              Prezi Editor Engineer
              <br />
              Self-Taught
            </div>
          </TeamBio>
        </TeamBox>
      </div>
    </div>
  )
})
TeamPage.displayName = 'TeamPage'

export default TeamPage
