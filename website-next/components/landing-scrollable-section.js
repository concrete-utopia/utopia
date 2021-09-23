import * as React from 'react'
import { GhostBrowserDark } from './ghostbrowser'
import { HostedImage } from './hosted-image'

const Content = [
  {
    image: '/screenshots/screenshot2.png',
    title: 'Multifile Projects',
  },
  {
    image: '/screenshots/screenshot2.png',
    title: 'NPM imports',
  },
  {
    image: '/screenshots/screenshot2.png',
    title: 'Assets, fonts, external',
  },
  {
    image: '/screenshots/screenshot2.png',
    title: 'Multifile Projects',
  },
]

export const LandingScrollableSection = (props) => {
  return (
    <div
      className='px-6'
      style={{
        overflowX: 'scroll',
      }}
    >
      <div
        style={{
          display: 'flex',
          gap: 32,
          width: Content.length * 472,
        }}
      >
        {Content.map((c) => (
          <div style={{ width: 440 }}>
            <GhostBrowserDark className='w-full object-cover'>
              <HostedImage scr={c.image} />
            </GhostBrowserDark>
            <div className='pt-4' style={{ color: '#D0D0D0' }}>
              {c.title}
            </div>
          </div>
        ))}
      </div>
    </div>
  )
}
