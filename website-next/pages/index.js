/* This example requires Tailwind CSS v2.0+ */
import styles from '../styles/Home.module.css'

import { Fragment } from 'react'
import { Popover, Transition } from '@headlessui/react'
import SwitchHorizontalIcon from '@heroicons/react/outline/SwitchHorizontalIcon'
import { Menu } from '../components/menu'
import { GhostBrowser, GhostBrowserDark } from '../components/ghostbrowser'
import { Header } from '../components/header'
import { ImageContainer } from '../components/image-container'
import { LandingScrollableSection } from '../components/landing-scrollable-section'
import { HostedImage } from '../components/hosted-image'
import { MainTitle } from '../components/main-title'
import { Subtitle } from '../components/subtitle'
import Blog from '../pages/blog'

function HeroButton({ href, children }) {
  return (
    <div className='mt-3'>
      <a
        href={href}
        className={
          'flex items-center justify-center font-button font-light text-lg leading-loose pl-16 pr-16'
        }
        style={{
          color: '#FFFFFF',
          backgroundImage: `linear-gradient(90deg, rgba(255,255,255,0.00) 0%, #393D4B 18%, #393D4B 84%, rgba(255,255,255,0.00) 100%)`,
        }}
      >
        {children}
      </a>
    </div>
  )
}

function HeroSection() {
  return (
    <>
      <div className='max-w-7xl mx-auto pt-40'>
        <div className='relative z-10 sm:pb-16 pb-20'>
          <main className='mt-10 mx-auto max-w-7xl px-2 sm:mt-12 sm:px-6 md:mt-16 '>
            <div className='text-center'>
              <MainTitle>
                Code{' '}
                <SwitchHorizontalIcon
                  className={'h-10 w-10  md:h-12 md:w-12 inline pb-2 ' + styles['rotating-icon']}
                />{' '}
                Design
              </MainTitle>
              <Subtitle>
                Utopia is the production-grade online coding and design tool for React that reads
                and writes code you’ll want to commit.
              </Subtitle>
              <div
                className='mt-12 sm:mt-12
                sm:flex sm:justify-center gap-x-3
                '
              >
                <HeroButton href='/project'>Create a Project</HeroButton>
              </div>
            </div>
          </main>
        </div>
      </div>
      <div className='px-2 pt-12 pb-24 max-w-7xl mx-auto'>
        <GhostBrowser className='max-w-7xl object-cover'>
          <HostedImage src='/screenshots/screenshot2.png' />
        </GhostBrowser>
      </div>
    </>
  )
}

const LandingPageSection2 = (props) => (
  <div className='pt-24 pb-24' style={{ width: '100%', backgroundColor: '#FFFFFF' }}>
    <div className='max-w-7xl mx-auto flex'>
      <div className='text-left w-2/4 pr-4'>
        <MainTitle>Work on the Real Thing</MainTitle>
        <Subtitle>
          Utopia connects design and code in real time. It’s built for real-world code, including
          nested components.
        </Subtitle>
      </div>
      <div className='w-2/4 pl-8'>
        <GhostBrowser>
          <HostedImage src='/screenshots/screenshot2.png' />
        </GhostBrowser>
      </div>
    </div>
  </div>
)

const LandingPageSection3 = (props) => (
  <div className='pt-24 pb-24' style={{ width: '100%', backgroundColor: '#FFFFFF' }}>
    <div className='max-w-7xl mx-auto flex'>
      <div className='w-2/4 grid grid-flow-col grid-cols-2 grid-rows-3 gap-4 pr-4'>
        <ImageContainer />
        <ImageContainer />
        <ImageContainer />
        <ImageContainer />
        <ImageContainer />
        <ImageContainer />
      </div>
      <div className='text-right w-2/4 pl-4'>
        <MainTitle>Immediately Familiar</MainTitle>
        <Subtitle>
          Utopia includes a though-out design tool to inspect, edit and create. It uses your code as
          a source of truth, and you can see the changes it makes to it in real time.
        </Subtitle>
      </div>
    </div>
  </div>
)

const LandingPageSection4 = (props) => (
  <div className='pt-24 pb-24' style={{ width: '100%', backgroundColor: '#181818' }}>
    <div className='max-w-7xl mx-auto text-center'>
      <MainTitle dark>Designed to Code</MainTitle>
      <Subtitle dark>
        Utopia includes a browser-based development environment for React. It’s powered by
        Microsoft’s VSCode, takes seconds to spin up, and give you a real-time preview of your app
        and components. And it includes everything you expect:
      </Subtitle>
    </div>
    <div className='max-w-7xl pt-16 mx-auto'>
      <LandingScrollableSection />
    </div>
  </div>
)

const OpenSourceSection = (props) => (
  <div className='pt-24 pb-24' style={{ width: '100%', backgroundColor: '#FFFFFF' }}>
    <div className='max-w-7xl mx-auto text-center'>
      <MainTitle>Proudly Open Source</MainTitle>
      <Subtitle>Utopia is an open-source project under the MIT license.</Subtitle>
    </div>
  </div>
)

// TODO this is the landing page, it should be the default export once it's enabled
// export default function LandingPage() {
function LandingPage() {
  return (
    <div>
      <Header />
      <div
      // className='relative bg-blue-100 overflow-hidden bg-no-repeat bg-cover'
      // style={{ backgroundImage: 'url(/backgrounds/mesh-pinkblue-sky.jpg)' }}
      >
        <div
          style={{
            backgroundImage: 'linear-gradient(white 0%, white 50%, #ffffff00 100%)',
          }}
          className='h-8 sm:h-16 lg:h-32'
        >
          <div
            id='menu'
            className='bg-white fixed flex w-screen justify-center z-100 h-16 items-center'
          >
            <Menu />
          </div>
        </div>

        <HeroSection />
        <LandingPageSection2 />
        <LandingPageSection3 />
        <LandingPageSection4 />
        <OpenSourceSection />
        <div className='pt-80' />
      </div>
    </div>
  )
}

export default Blog
