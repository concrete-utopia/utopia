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

// <head><meta charset="utf-8"><meta name="theme-color" content="#ffffff"><meta property="og:title" content="Utopia"><meta property="og:description" content="A creative platform for designing, coding, and shipping interactive interfaces"><meta property="og:image" content="/static/brand/logo-exactsized-brandpurple-115x90@2x.png"><link rel="preconnect" href="https://fonts.gstatic.com/" crossorigin="use-credentials"><link href="https://fonts.googleapis.com/css?family=Inter:300,400,500,600,700&amp;display=swap" rel="stylesheet"><link rel="manifest" href="/static/manifest.json"><link rel="apple-touch-icon" sizes="57x57" href="/static/apple-icon-57x57.png"><link rel="apple-touch-icon" sizes="60x60" href="/static/apple-icon-60x60.png"><link rel="apple-touch-icon" sizes="72x72" href="/static/apple-icon-72x72.png"><link rel="apple-touch-icon" sizes="76x76" href="/static/apple-icon-76x76.png"><link rel="apple-touch-icon" sizes="114x114" href="/static/apple-icon-114x114.png"><link rel="apple-touch-icon" sizes="120x120" href="/static/apple-icon-120x120.png"><link rel="apple-touch-icon" sizes="180x180" href="/static/apple-icon-180x180.png"><link rel="apple-touch-icon" sizes="144x144" href="/static/apple-icon-144x144.png"><link rel="apple-touch-icon" sizes="152x152" href="/static/apple-icon-152x152.png"><link rel="icon" type="image/x-icon" href="/static/apple-icon-152x152.png"><link rel="icon" type="image/png" sizes="16x16" href="/static/favicon-16x16.png"><link rel="icon" type="image/png" sizes="32x32" href="/static/favicon-32x32.png"><link rel="icon" type="image/png" sizes="96x96" href="/static/favicon-96x96.png"><link rel="icon" type="image/png" sizes="192x192" href="/static/android-icon-192x192.png"><meta name="msapplication-TileColor" content="#ffffff"><meta name="msapplication-TileImage" content="/static/ms-icon-144x144.png"><link data-utopia="" rel="stylesheet" href="/editor/css/utopions.css"><title>Utopia</title><style data-emotion="css" data-s=""></style></head>
