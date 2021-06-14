import * as React from 'react'
import { Header } from '../components/header'
import { Menu } from '../components/Menu'
import { MainTitle } from '../components/main-title'
import { Paragraph } from '../components/paragraph'
import { GhostBrowser } from '../components/ghostbrowser'
import { HostedImage } from '../components/hosted-image'

export default function Blog() {
  return (
    <div>
      <Header />
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
      <div className='pt-28 pb-28 text-center'>
        <MainTitle dark={false}>Introducing Utopia</MainTitle>
      </div>
      <div className='text-center max-w-4xl mx-auto'>
        <Paragraph dark={false}>Text Content here</Paragraph>
        <Paragraph dark={false}>Text Content here</Paragraph>
      </div>
      <GhostBrowser className='max-w-4xl mx-auto'><HostedImage src=''/></GhostBrowser>
    </div>
  )
}
