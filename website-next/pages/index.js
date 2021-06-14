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

function HeroButton({ href, children, primary }) {
  return (
    <div className='rounded-2xl shadow mt-3'>
      <a
        href={href}
        className={
          'rounded-2xl w-full flex items-center justify-center text-base font-medium font-button ' +
          'md:text-lg px-4 py-2 md:px-6 md:py-2 ' +
          (primary
            ? 'border-2 border-black bg-gradient-to-b from-blue-100 to-pink-300'
            : 'border border-transparent text-purple-600 bg-indigo-100 hover:bg-indigo-200')
        }
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
                <HeroButton primary href='/project'>
                  Create a Project
                </HeroButton>
              </div>
            </div>
          </main>
        </div>
      </div>
      <div className='px-2 pt-6 pb-24 sm:pt-16 lg:pt-32 max-w-7xl mx-auto'>
        <GhostBrowser className='max-w-7xl object-cover'>
          <HostedImage src='/screenshots/screenshot2.png' />
        </GhostBrowser>
      </div>
    </>
  )
}

function DesignToolForCodeSection() {
  return (
    <>
      <div className='max-w-7xl mx-auto pt-48'>
        <MainTitle>A design tool built for code</MainTitle>
        <Subtitle>
          Utopia writes and understands code, and gives you the tools to compose and manipulate it.
          Use all the design features you know, and combine them with the power of flexbox, nested
          components, and component props with a UI.
        </Subtitle>
      </div>
      <div className='px-2 pt-6 sm:pt-16 lg:pt-32 max-w-screen-2xl mx-auto'>
        <GhostBrowser className='w-full object-cover'>
          <HostedImage src='/screenshots/screenshot2.png' />
        </GhostBrowser>
      </div>
    </>
  )
}

function CodeEditorForDesignSection() {
  return (
    <>
      <div className='max-w-7xl mx-auto pt-48 text-right'>
        <MainTitle>A code editor built for design</MainTitle>
        <Subtitle>
          Utopia is powered by VSCode’s Monaco editor and we’ve given it superpowers: selecting
          elements jumps to code, hovering over code highlights elements.
        </Subtitle>
      </div>
      <div className='px-2 pt-6 sm:pt-16 lg:pt-32 max-w-screen-2xl mx-auto'>
        <GhostBrowser className='w-full object-cover'>
          <HostedImage src='/screenshots/screenshot2.png' />
        </GhostBrowser>
      </div>
    </>
  )
}

function AlwaysLiveSection() {
  return (
    <>
      <div className='max-w-7xl mx-auto pt-40 text-center'>
        <MainTitle>Always Live</MainTitle>
        <Subtitle>
          See code and design change instantly. Instantly jump between between design and preview
          mode. See and edit the same component in multiple configurations.
        </Subtitle>
      </div>
      <div className='px-2 pt-6 sm:pt-16 lg:pt-32 max-w-screen-2xl mx-auto'>
        <GhostBrowser className='w-full object-cover'>
          <HostedImage src='/screenshots/screenshot2.png' />
        </GhostBrowser>
      </div>
    </>
  )
}

const LandingPageSection2 = props => (
  <div className='pt-24 pb-24' style={{width: '100%', backgroundColor: '#FFFFFF'}}>
    <div className='max-w-7xl mx-auto flex'>
      <div className='text-left w-2/4 pr-4'>
        <MainTitle>Work on the Real Thing</MainTitle>
        <Subtitle>
          Utopia connects design and code in real time. It’s built for real-world code, including nested components.
        </Subtitle>
      </div>
      <div  className='w-2/4 pl-8'>
        <GhostBrowser>
            <HostedImage src='/screenshots/screenshot2.png' />
        </GhostBrowser>
      </div>
    </div>
  </div>
)

const LandingPageSection3 = props => (
  <div className='pt-24 pb-24' style={{width: '100%', backgroundColor: '#FFFFFF'}}>
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
          Utopia includes a though-out design tool to inspect, edit and create.
          It uses your code as a source of truth, and you can see the changes it makes to it in real time.
        </Subtitle>
      </div>
    </div>
  </div>
)

const LandingPageSection4 = props => (
  <div className='pt-24 pb-24' style={{width: '100%', backgroundColor: '#181818'}}>
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

const OpenSourceSection = props => (
  <div className='pt-24 pb-24' style={{width: '100%', backgroundColor: '#FFFFFF'}}>
    <div className='max-w-7xl mx-auto text-center'>
      <MainTitle>Proudly Open Source</MainTitle>
      <Subtitle>
        Utopia is an open-source project under the MIT license.
      </Subtitle>
    </div>
  </div>
)

export default function LandingPage() {
  return (
    <div>
      <Header />
      <div
        className='relative bg-blue-100 overflow-hidden bg-no-repeat bg-cover'
        style={{ backgroundImage: 'url(/backgrounds/mesh-pinkblue-sky.jpg)' }}
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
        <LandingPageSection2/>
        <LandingPageSection3/>
        <LandingPageSection4/>
        <OpenSourceSection />
        {/* <DesignToolForCodeSection /> */}
        {/* <CodeEditorForDesignSection /> */}
        {/* <AlwaysLiveSection /> */}
        <div className='pt-80' />
      </div>
    </div>
  )
}
