/** @jsx jsx */
import { jsx } from '@emotion/core'
import styled from '@emotion/styled'

// this is the expensive fully-loaded Prism. If you read this comment, and it's after the 7th of May, 2020,
// you're welcome to try using the *lighter* experience by following the instructions here:
// https://github.com/conorhastings/react-syntax-highlighter

import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { base16AteliersulphurpoolLight as syntaxTheme } from 'react-syntax-highlighter/dist/cjs/styles/prism'

import * as React from 'react'
import { colorTheme, UtopiaStyles, SimpleFlexRow } from 'uuiui'
import { H1, H2, PrettyKeys, EM, CalloutPrimary as Callout, A, P } from './documentation-components'

const codeString = `
App.propertyControls = {
  greetings: {
    type: 'string',
    title: 'Greetings',
    defaultValue: 'Hello World',
  },
  alpha: {
    type: 'slider',
    title: 'Amount',
    defaultValue: 0,
    min: 0,
    max: 100,
    step: 1,
  },
  backdrop: {
    type: 'color',
    title: 'Color',
    defaultValue: '#ddd',
  },
}

`

export const renderedGettingStarted = (
  <div>
    <H1>Getting Started</H1>
    <div>
      <P>
        Welcome to the pre-release! A few tips to get you started:
        <ul>
          <li>
            Showing / hiding the code editor: use <PrettyKeys shortcut='⌘⎇.' />
          </li>
          <li>
            Running the code vs editing: use the button in the top right corner, or&nbsp;
            <PrettyKeys shortcut={['⌘', 'Enter']} />
          </li>
          <li>
            To work on, or preview, one or more components: create them via React code as usual,
            then hit the "+" Menu and insert a Scene. In the inspector, select the component you'd
            like to render from a dropdown.
          </li>
        </ul>
        <Callout>
          You don't need an account to get started, but you'll need to sign up to use assets in your
          products, and share them with others.
        </Callout>
      </P>
      <P>
        <H2>Using controls</H2>
        You can add custom controls to your components, and configure them in the inspector. To try
        it, copy and paste this code at the end of your project.
        <SyntaxHighlighter language='jsx' style={syntaxTheme}>
          {codeString}
        </SyntaxHighlighter>
      </P>

      <P>
        <H2>Known Issues</H2>
        Utopia is in developer preview. You should expect your code to be safe, your projects
        recoverable, and code errors being reported well and consistently. However, some features -
        notably visual editing - are still being improved. Their behaviour won't change, but they
        will work better in edge cases.
        <ul>
          <li>
            The inspector does not yet reliably show different layout options, especially where
            elements are configured via css classes
          </li>
          <li>
            The editor does not reliably show you styles applied from css files or class names.
          </li>
          <li>
            Text insertion and manipulation via the visual editor inserts a special
            <code>Text</code> component.
          </li>
          <li>Class-based components, and top-level code mutation, don't yet work reliably.</li>
        </ul>
      </P>
    </div>

    <H1>Change Log - May 2020</H1>
    <div>
      <p style={{ marginTop: 36 }}>
        <EM>Highlights of May</EM>
        <ul style={{ paddingLeft: 30 }}>
          <li>
            <EM>Image handling</EM> We've made it a lot easier to add images to your projects,
            configure them, and swap them out.
          </li>
          <li>
            <EM>Asset support</EM> You can now add other assets to your projects. For now there's an
            upper limit of 5MB. You can also upload code files. Utopia can open and edit any file
            you import that's text or code.
          </li>
          <li>
            <EM>Crash recovery</EM>. The editor now recovers after crashes, and starts in Safe Mode
            so you can fix your loops.
          </li>

          <li>
            <EM>Bug fixes:</EM> We've patched a few editor holes, improved recovery and auto-save,
            improved how and when we show error messages, and added offline detection.
          </li>
        </ul>
      </p>
    </div>

    <H1>Change Log - April 2020</H1>
    <div>
      <p style={{ marginTop: 36 }}>
        <EM>Highlights of April.</EM>Welcome to Utopia, April developer preview :) We shipped a lot
        of things, here are some of the highlights:
        <ul style={{ paddingLeft: 30 }}>
          <li>
            <EM>Better multiselection across scenes</EM>
          </li>
          <li>
            <EM>Inspector clean-ups</EM> - easier reading, more consistent layout
          </li>

          <li>
            <EM>Image insertion works properly now</EM>
          </li>
          <li>
            <EM>Load Speed</EM> - Editor now loads 30-50% faster
          </li>
          <li>
            <EM>Default Project</EM> - Removed junk, made it faster
          </li>
          <li>
            <EM>Speed</EM> - Preview button now starts preview, separate button shows separate
            preview. Top tip: use <PrettyKeys shortcut={'⌘ ⎇ p'} /> to turn the canvas into an
            interactive preview.
          </li>
          <li>
            <EM>Live Canvas</EM> - switch between editing and running your components directly on
            the canvas
          </li>
          <li>
            <EM>Better multifile project support</EM> - errors and warnings are grouped by source
            files
          </li>
          <li>
            <EM>Improved code editing</EM> - JSX tags now close automatically, and we've got
            auto-indent
          </li>

          <li>
            <EM>Open Graph tags</EM> - share a project URL on Twitter, Slack, Discord, Facebook, etc
            See a preview image, title, and creator info
          </li>
          <li>
            <EM>Infinite loop protection</EM> - in the app.js file for now
          </li>
        </ul>
      </p>
      <hr />
      <h3>😎Theme Support 😎</h3>
      <p>
        We now support a lot more themes for the code editor, including some of the most popular
        ones:&nbsp;
        <A href='https://draculatheme.com/visual-studio/'>Dracula</A>,&nbsp;
        <A href='https://marketplace.visualstudio.com/items?itemName=liviuschera.noctis'>Noctis</A>
        ,&nbsp;
        <A href='https://github.com/dempfi/ayu'>Ayu</A>,&nbsp;Horizon, Pale Night, Material, Shades
        of Purple, Glass, OneDarkPro, OneMonokai, Firefox Light, and more. To change themes, go to
        Project Settings and select the theme.
      </p>
    </div>
  </div>
)
