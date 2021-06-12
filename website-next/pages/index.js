/* This example requires Tailwind CSS v2.0+ */
import styles from '../styles/Home.module.css'

import { Fragment } from 'react'
import { Popover, Transition } from '@headlessui/react'
import SwitchHorizontalIcon from '@heroicons/react/outline/SwitchHorizontalIcon'
import { Menu } from '../components/menu'
import { GhostBrowser } from '../components/ghostbrowser'
import { Header } from '../components/header'

function MainTitle({ children }) {
  return (
    <h1
      className='text-5xl md:text-6xl tracking-tight text-gray-900 font-extrabold font-headline'
      style={{
        textShadow: '0 2px 59px #00FFCD',
      }}
    >
      {children}
    </h1>
  )
}

function Subtitle({ children, center }) {
  return (
    <p
      className={
        'mt-14 sm:mt-16' +
        'sm:max-w-xl md:max-w-3xl ' +
        'text-xl md:text-2xl ' +
        'leading-snug md:leading-normal ' +
        'font-body font-medium ' +
        'tracking-wider ' +
        'inline-block '
      }
    >
      {children}
    </p>
  )
}

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
                {/* <HeroButton>Read More</HeroButton> */}
              </div>
            </div>
          </main>
        </div>
      </div>
      <div className='px-2 pt-6 sm:pt-16 lg:pt-32 max-w-screen-2xl mx-auto'>
        <GhostBrowser className='w-full object-cover'>
          <img src='/screenshots/screenshot2.png' />
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
          <img src='/screenshots/screenshot2.png' />
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
          <img src='/screenshots/screenshot2.png' />
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
          <img src='/screenshots/screenshot2.png' />
        </GhostBrowser>
      </div>
    </>
  )
}

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
        {/* <DesignToolForCodeSection />
        <CodeEditorForDesignSection />
        <AlwaysLiveSection /> */}
        <div className='pt-80' />
      </div>
    </div>
  )
}
