/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'

// this is the expensive fully-loaded Prism. If you read this comment, and it's after the 7th of May, 2020,
// you're welcome to try using the *lighter* experience by following the instructions here:
// https://github.com/conorhastings/react-syntax-highlighter

import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { base16AteliersulphurpoolLight as syntaxTheme } from 'react-syntax-highlighter/dist/cjs/styles/prism'

import React from 'react'
import {
  H1,
  H2,
  PrettyKeys,
  EM,
  CalloutPrimary as Callout,
  A,
  PMT,
} from './documentation-components'
import type { ProjectListing } from '../../common/persistence'
import { fetchProjectMetadata } from '../../common/server'
import { stripNulls } from '../../core/shared/array-utils'
import { projectURLForProject } from '../../core/shared/utils'
import { EllipsisSpinner } from '../common/ellipsis-spinner'
import { FlexRow, FlexColumn } from '../../uuiui'

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

const ProjectCard = React.memo((props: ProjectListing) => {
  const projectURL = projectURLForProject(props.id, props.title)
  const onClick = React.useCallback(() => window.open(projectURL, '_self'), [projectURL])
  return (
    <div
      css={{
        transition: 'all .2s ease-in-out',
        scrollSnapAlign: 'start',
        '&:hover': {
          transform: 'scale(1.02)',
        },
      }}
      style={{
        position: 'relative',
        flex: '0 0 312px',
        overflow: 'hidden',
        cursor: 'pointer',
        borderRadius: 2,
        border: '1px solid lightgrey',
        fontSize: 13,
      }}
      onClick={onClick}
    >
      <div
        style={{
          backgroundImage: `url(${props.thumbnail})`,
          backgroundRepeat: 'no-repeat',
          backgroundSize: 'cover',
          height: 192,
        }}
      />

      <div>
        <FlexRow
          style={{
            marginTop: 8,
            fontWeight: 500,
            fontSize: 13,
            lineHeight: '1.4em',
            paddingLeft: 8,
            paddingRight: 8,
          }}
        >
          <span style={{ overflow: 'hidden', flexGrow: 1, textOverflow: 'ellipsis' }}>
            {props.title}
          </span>
        </FlexRow>
        <div
          style={{
            fontWeight: 400,
            fontSize: 12,
            marginTop: 8,
            height: 33,
            paddingLeft: 8,
            paddingRight: 8,
            color: 'grey',
            lineHeight: '1.4em',
            whiteSpace: 'pre-wrap',
            overflow: 'hidden',
          }}
        >
          {props.description}
        </div>
        <FlexRow
          style={{
            marginTop: 8,
            paddingLeft: 8,
            paddingRight: 8,
            marginBottom: 4,
            color: 'grey',
            fontSize: 11,
          }}
        >
          <div
            style={{
              borderRadius: '50%',
              width: 21,
              height: 21,
              backgroundRepeat: 'no-repeat',
              backgroundSize: 'cover',
              backgroundImage: `url(${props.ownerPicture})`,
            }}
          />

          <span style={{ marginLeft: 8 }}>{props.ownerName ?? ''}</span>
        </FlexRow>
      </div>
    </div>
  )
})
const FixedWidth = styled.div({ width: 900, marginLeft: 'auto', marginRight: 'auto' })

const FeaturedProjectIDs: ReadonlyArray<string> = [
  'd09b3b51-rule-30-ish',
  'ecedc3b5-conways-game-of-life',
  '6c783a57-western-violin',
  '97a93d89-react-spring-example',
  'f4fcb83b-react-spring-cards',
]

const LoadedProjectsRow = React.memo(
  ({ projects }: { projects: ReadonlyArray<ProjectListing> }) => {
    return (
      <FlexRow
        style={{
          position: 'relative',
          height: 360,
          overflowX: 'scroll',
          backgroundColor: '#F9F9F9',
        }}
      >
        <FlexRow
          style={{
            position: 'absolute',
            gap: 30,
            margin: 30,
          }}
        >
          {projects.map((projectListing) => (
            <ProjectCard key={`project-card-${projectListing.id}`} {...projectListing} />
          ))}
        </FlexRow>
      </FlexRow>
    )
  },
)

const FeaturedProjects = React.memo(() => {
  const [featuredProjectList, setFeaturedProjectList] =
    React.useState<ReadonlyArray<ProjectListing> | null>(null)

  React.useEffect(() => {
    const fetchedProjectPromises = FeaturedProjectIDs.map((projectId) =>
      fetchProjectMetadata(projectId),
    )
    void Promise.all(fetchedProjectPromises).then((fetchedProjects) => {
      const withOutNulls = stripNulls(fetchedProjects)
      setFeaturedProjectList(withOutNulls)
    })
  }, [])

  const spinnerRow = (
    <FixedWidth>
      <EllipsisSpinner />
    </FixedWidth>
  )

  return (
    <FlexColumn>
      <FixedWidth>
        <H1>Featured Projects</H1>
      </FixedWidth>
      {featuredProjectList == null ? (
        spinnerRow
      ) : (
        <LoadedProjectsRow projects={featuredProjectList} />
      )}
    </FlexColumn>
  )
})

export const GettingStarted = React.memo(() => {
  return (
    <FixedWidth>
      <H1>Welcome to the Developer Preview</H1>
      <PMT>
        Utopia is an online design and coding environment for React. You can go back and forth
        between design and code, and between design and preview.
      </PMT>
      <PMT>
        Our goal is to give you a creative tool to realize your interface ideas by connecting design
        and code.
      </PMT>
      <PMT>To get started, check out some of our example projects, or start from scratch.</PMT>
      <FeaturedProjects />

      <H1>Keyboard Shortcuts</H1>
      <PMT>
        <ul>
          <li>
            Showing / hiding the code editor: use <PrettyKeys shortcut='⌘⎇.' />
          </li>
          <li>
            Running the code vs editing: use the button in the top right corner, or&nbsp;
            <PrettyKeys shortcut={['⌘', 'Enter']} />
          </li>
        </ul>
        <Callout>
          You don't need an account to get started, but you'll need to sign up to use assets in your
          products, and share them with others.
        </Callout>
      </PMT>
      <PMT>
        <H2>Using controls</H2>
        You can add custom controls to your components, and configure them in the inspector. To try
        it, copy and paste this code at the end of your project.
        <SyntaxHighlighter language='jsx' style={syntaxTheme}>
          {codeString}
        </SyntaxHighlighter>
      </PMT>

      <PMT>
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
      </PMT>

      <H1>Change Log - June 2020</H1>
      <PMT>
        <EM>Highlights of June</EM>
        <ul style={{ paddingLeft: 30 }}></ul>
      </PMT>

      <H1>Change Log - May 2020</H1>
      <PMT>
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
      </PMT>

      <H1>Change Log - April 2020</H1>
      <PMT>
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
      </PMT>
      <hr />
      <h3>😎Theme Support😎</h3>
      <PMT>
        We now support a lot more themes for the code editor, including some of the most popular
        ones:&nbsp;
        <A href='https://draculatheme.com/visual-studio/'>Dracula</A>,&nbsp;
        <A href='https://marketplace.visualstudio.com/items?itemName=liviuschera.noctis'>Noctis</A>
        ,&nbsp;
        <A href='https://github.com/dempfi/ayu'>Ayu</A>,&nbsp;Horizon, Pale Night, Material, Shades
        of Purple, Glass, OneDarkPro, OneMonokai, Firefox Light, and more. To change themes, go to
        Project Settings and select the theme.
      </PMT>
    </FixedWidth>
  )
})
