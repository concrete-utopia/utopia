import * as React from 'react'
import Head from 'next/head'

export function Header() {
  return (
    <Head>
      <title>Utopia</title>
      <meta name='viewport' content='width=device-width,initial-scale=1,shrink-to-fit=no' />

      {/* favicons cropped to fix vertical alignment, but I don't like that they are cropped */}
      <link rel='icon' href='/favicon-32.png' />
      <link rel='icon' href='/favicon-128.png' />
      <link rel='icon' href='/favicon-152.png' />
      <link rel='icon' href='/favicon-167.png' />
      <link rel='icon' href='/favicon-180.png' />
      <link rel='icon' href='/favicon-192.png' />

      {/* 
      these new favicons are not cropped, but their vertical alignment is different
      <link rel='icon' type='image/png' sizes='32x32' href='/favicon-32x32.png' />
      <link rel='icon' type='image/png' sizes='16x16' href='/favicon-16x16.png' /> */}

      {/*  Mobile and misc. favicons where cropping was not necessary */}
      <link rel='apple-touch-icon' sizes='180x180' href='/apple-touch-icon.png' />
      <link rel='manifest' href='/site.webmanifest' />
      <meta name='apple-mobile-web-app-title' content='Utopia' />
      <meta name='application-name' content='Utopia' />
      <meta name='msapplication-TileColor' content='#da532c' />
      <meta name='theme-color' content='#ffffff'></meta>
      {/* the safari outline icon I'm very proud of. */}
      <link rel='mask-icon' href='/safari-pinned-tab.svg' color='#5bbad5' />

      {/* OpenGraph tags */}
      <meta property='og:title' content='Utopia:Design and Code on one platform' />
      <meta
        property='og:description'
        content='Utopia is a production-grade online coding and design tool for React that reads and writes code you’ll want to commit.'
      />
      <meta property='og:image' content='/utopia_og_preview_image@2x.png' />
      <meta property='og:type' content='website' />
    </Head>
  )
}
