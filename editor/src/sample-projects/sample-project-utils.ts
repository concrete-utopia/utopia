import { contentsToTree, ProjectContentTreeRoot } from '../components/assets'
import {
  DefaultPackageJson,
  PersistentModel,
  persistentModelForProjectContents,
  StoryboardFilePath,
} from '../components/editor/store/editor-state'
import {
  appJSFile,
  getDefaultUIJsFile,
  getSamplePreviewFile,
  getSamplePreviewHTMLFile,
} from '../core/model/new-project-files'
import { directory } from '../core/model/project-file-utils'
import {
  codeFile,
  ProjectContents,
  RevisionsState,
  TextFile,
  textFile,
  textFileContents,
  unparsed,
} from '../core/shared/project-file-types'

export function simpleDefaultProject(): PersistentModel {
  const projectContents: ProjectContents = {
    '/package.json': textFile(
      textFileContents(
        JSON.stringify(DefaultPackageJson, null, 2),
        unparsed,
        RevisionsState.CodeAhead,
      ),
      null,
      null,
      0,
    ),
    '/src': directory(),
    '/src/app.js': appJSFile(),
    [StoryboardFilePath]: getDefaultUIJsFile(),
    '/assets': directory(),
    '/public': directory(),
    '/src/index.js': getSamplePreviewFile(),
    '/public/index.html': getSamplePreviewHTMLFile(),
  }

  let persistentModel = persistentModelForProjectContents(contentsToTree(projectContents))
  return persistentModel
}

export function createComplexDefaultProjectContents(): ProjectContents {
  function createCodeFile(path: string, contents: string): TextFile {
    return textFile(
      textFileContents(contents, unparsed, RevisionsState.CodeAhead),
      null,
      null,
      Date.now(),
    )
  }

  return {
    '/package.json': textFile(
      textFileContents(
        JSON.stringify(DefaultPackageJson, null, 2),
        unparsed,
        RevisionsState.CodeAhead,
      ),
      null,
      null,
      0,
    ),
    '/src': directory(),
    '/assets': directory(),
    '/public': directory(),
    '/src/index.js': createCodeFile('/src/index.js', getSamplePreviewFile().fileContents.code),
    '/public/index.html': getSamplePreviewHTMLFile(),
    [StoryboardFilePath]: createCodeFile(
      StoryboardFilePath,
      `import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
  registerModule,
} from 'utopia-api'
import { App } from '/src/app.js'

export var SameFileApp = (props) => {
  return <div data-uid='same-file-app-div' style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }} />
}

export var storyboard = (
  <Storyboard data-uid='storyboard-entity'>
    <Scene
      data-label='Imported App'
      data-uid='scene-1-entity'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='app-entity' />
    </Scene>
    <Scene
      data-label='Same File App'
      data-uid='scene-2-entity'
      style={{ position: 'absolute', left: 400, top: 0, width: 375, height: 812 }}
    >
      <SameFileApp data-uid='same-file-app-entity' />
    </Scene>
  </Storyboard>
)`,
    ),
    '/src/app.js': createCodeFile(
      '/src/app.js',
      `import * as React from 'react'
import { Card } from '/src/card.js'
export var App = (props) => {
  return (
    <div
      data-uid='app-outer-div'
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <Card data-uid='card-instance' style={{ position: 'absolute', left: 0, top: 0, width: 200, height: 300}} />
    </div>
  )
}`,
    ),
    '/src/card.js': createCodeFile(
      '/src/card.js',
      `import * as React from 'react'
import { Rectangle } from 'utopia-api'
export var Card = (props) => {
  return <div data-uid='card-outer-div' style={{...props.style}}>
    <div data-uid='card-inner-div' style={{ position: 'absolute', left: 0, top: 0, width: 50, height: 50, backgroundColor: 'red' }} />
    <Rectangle data-uid='card-inner-rectangle' data-testid='rectangle' style={{ position: 'absolute', left: 100, top: 200, width: 50, height: 50, backgroundColor: 'blue' }} />
  </div>
}`,
    ),
  }
}

export function complexDefaultProject(): PersistentModel {
  const projectContents = createComplexDefaultProjectContents()
  const persistentModel = persistentModelForProjectContents(contentsToTree(projectContents))
  return persistentModel
}

function createBeachesProjectContents(): ProjectContents {
  function createCodeFile(path: string, contents: string): TextFile {
    return textFile(
      textFileContents(contents, unparsed, RevisionsState.CodeAhead),
      null,
      null,
      Date.now(),
    )
  }

  return {
    '/package.json': textFile(
      textFileContents(
        JSON.stringify(
          {
            ...DefaultPackageJson,
            dependencies: {
              ...DefaultPackageJson.dependencies,
              'react-spring': '8.0.27',
              '@heroicons/react': '1.0.1',
              '@emotion/react': '11.9.3',
            },
          },
          null,
          2,
        ),
        unparsed,
        RevisionsState.CodeAhead,
      ),
      null,
      null,
      0,
    ),
    '/src': directory(),
    '/assets': directory(),
    '/public': directory(),
    '/src/index.js': createCodeFile('/src/index.js', getSamplePreviewFile().fileContents.code),
    '/public/index.html': getSamplePreviewHTMLFile(),
    '/public/globals.css': codeFile(
      `body, #canvas-container * {
      font-family: San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
  }
  
  @font-face {
      font-family: 'ITC Garamond ';
      font-style: normal;
      font-weight: 400;
      font-display: swap;
      src: local(ITC Garamond ) format('ttf');
    }`,
      null,
    ),
    '/public/data.js': codeFile(
      `export const beaches = [
        {
          name: 'La Digue',
          country: 'Seychelles',
          image:
            'https://source.unsplash.com/jPmurJKSL_0/600x800',
          rating: 5,
        },
        {
          name: 'Isle of Pines',
          country: 'New Caledonia',
          image:
            'https://source.unsplash.com/n7DY58YFg9E/600x800',
          rating: 5,
        },
        {
          name: 'McWay Falls',
          country: 'California',
          image:
            'https://images.unsplash.com/photo-1631699447911-91183ecba384?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=987&q=80',
          rating: 3,
        },
        {
          name: 'Meeru Island',
          country: 'Maldives',
          image:
            'https://source.unsplash.com/8OGJqpNMBGM/600x800',
          rating: 4,
        },
      ]
      `,
      null,
    ),
    [StoryboardFilePath]: createCodeFile(
      StoryboardFilePath,
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { GridCard } from '/src/gridcard.js'
import { Button } from '/src/button.js'

import { SwitchHorizontalIcon } from '@heroicons/react/solid'

export var storyboard = (
  <Storyboard>
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        gap: 100,
      }}
    >
      <Scene
        style={{
          borderRadius: 5,
          height: 'min-content',
          width: 'min-content',
        }}
      >
        <Button
          icon={
            <SwitchHorizontalIcon
              style={{ width: 15, color: 'white' }}
            />
          }
        />
      </Scene>
      <Scene
        style={{
          borderRadius: 10,
          height: 'min-content',
          width: 170,
        }}
      >
        <GridCard
          name='La Digue'
          country='Seychelles'
          image='https://source.unsplash.com/jPmurJKSL_0/600x800'
          rating={5}
        />
      </Scene>
      <Scene
        style={{
          width: 375,
          height: 759,
          display: 'flex',
          flexDirection: 'column',
        }}
        data-label='Beaches'
      >
        <App style={{}} />
      </Scene>
    </div>
  </Storyboard>
)
      `,
    ),
    '/src/app.js': createCodeFile(
      '/src/app.js',
      `import { useState } from 'react'
import '../public/globals.css'
import { Button } from './button.js'
import { GridView } from './gridview.js'
import { ListView } from './listview.js'
import { FlexCol } from './utils'
import { beaches } from '../public/data.js'

import {
  SwitchHorizontalIcon,
  ViewGridIcon,
  ViewListIcon,
} from '@heroicons/react/solid'

export var App = () => {
  const [displayedBeaches, setDisplayedBeaches] = useState(
    beaches,
  )

  const handleShuffle = () => {
    setDisplayedBeaches(
      [...displayedBeaches].sort(() => Math.random() - 0.5),
    )
  }

  const [gridView, setGrid] = useState(true)

  return (
    <FlexCol
      style={{
        width: '100%',
        height: '100%',
        padding: 8,
        gap: 8,
      }}
    >
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          justifyContent: 'space-between',
          alignItems: 'baseline',
          gap: 70,
        }}
      >
        <span
          style={{
            fontSize: '40px',
            fontWeight: 700,
            fontStyle: 'normal',
          }}
        >
          Beaches
        </span>
        <div
          style={{
            display: 'flex',
            flexDirection: 'row',
            gap: 15,
          }}
        >
          <Button
            onClick={handleShuffle}
            icon={
              <SwitchHorizontalIcon
                style={{ width: 15, color: 'white' }}
              />
            }
          />
          <div style={{ display: 'flex', gap: 3 }}>
            <Button
              onClick={() => setGrid(!gridView)}
              icon={
                <ViewGridIcon
                  style={{ width: 15, color: 'white' }}
                />
              }
              clickState={gridView}
              unClickedState={false}
            />
            <Button
              onClick={() => setGrid(!gridView)}
              icon={
                <ViewListIcon
                  style={{ width: 15, color: 'white' }}
                />
              }
              clickState={gridView}
              unClickedState
            />
          </div>
        </div>
      </div>
      {gridView ? (
        <GridView cards={displayedBeaches} />
      ) : (
        <ListView cards={displayedBeaches} />
      )}
    </FlexCol>
  )
}
      `,
    ),
    '/src/button.js': createCodeFile(
      '/src/button.js',
      `/** @jsx jsx */
import { jsx, ThemeProvider } from '@emotion/react'

const theme = {
  colors: {
    primaryBlue: '#93D9FF',
    darkBlue: '#2ab4ff',
  },
}

export var Button = ({
  clickState,
  unClickedState,
  ...buttonProps
}) => {
  return (
    <button
      {...buttonProps}
      css={{
        border: 'none',
        borderRadius: '5px',
        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',
        padding: '5px',
        display: 'flex',
        justifyContent: 'center',
        alignItems: 'center',
        '&:hover': {
          opacity: 0.7,
        },
      }}
      style={{
        backgroundColor:
          clickState === unClickedState
            ? theme.colors.primaryBlue
            : theme.colors.darkBlue,
      }}
    >
      {buttonProps.icon}
    </button>
  )
}
      `,
    ),
    '/src/gridcard.js': createCodeFile(
      '/src/gridcard.js',
      `import { StarIcon } from '@heroicons/react/solid'
import { FlexCol, FlexRow } from './utils'
import { Rectangle } from 'utopia-api'

export function Ratings(props) {
  return (
    <FlexRow style={{ justifyContent: 'flex-start' }}>
      {Array.from({ length: props.rating ?? 1 }).map(
        (_, i) => {
          return (
            <StarIcon
              style={{ width: 20, color: 'orange' }}
            />
          )
        },
      )}
    </FlexRow>
  )
}

export var GridCard = (props) => {
  return (
    <FlexCol
      style={{
        backgroundColor: 'white',
        borderRadius: 10,
        flex: 1,
        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',
        paddingBottom: 5,
        overflow: 'hidden',
        height: 326,
        ...props.style,
      }}
    >
      <div
        style={{
          flex: 1,
          backgroundImage: \`url(\${props.image})\`,
          backgroundSize: 'cover',
          flexBasis: 150,
          marginBottom: 8,
        }}
      />
      <div
        style={{
          position: 'relative',
          paddingLeft: 5,
          fontSize: '18px',
          fontWeight: 700,
          fontStyle: 'normal',
        }}
      >
        {props.name}
      </div>
      <div
        style={{
          position: 'relative',
          paddingLeft: 5,
          color: 'rgb(0, 0, 0, 0.5)',
          fontFamily: 'ITC Garamond Std',
          fontStyle: 'italic',
        }}
      >
        {props.country}
      </div>
      <Ratings rating={props.rating} />
    </FlexCol>
  )
}
    `,
    ),
    '/src/gridview.js': createCodeFile(
      '/src/gridview.js',
      `import * as React from 'react'
import { GridCard } from './gridcard.js'
import { TwoColumnGrid } from './utils'

export var GridView = (props) => {
  return (
    <>
      <span
        style={{
          color: 'rgb(0, 0, 0, 0.5)',
          fontSize: '16px',
        }}
      >
        Featured
      </span>
      <TwoColumnGrid style={{ gap: 8 }}>
        {props.cards.map((beach) => (
          <GridCard
            name={beach.name}
            country={beach.country}
            image={beach.image}
            rating={beach.rating}
          />
        ))}
      </TwoColumnGrid>
    </>
  )
}
    `,
    ),
    '/src/listcard.js': createCodeFile(
      '/src/listcard.js',
      `import { StarIcon } from '@heroicons/react/solid'
import { FlexRow, TwoColumnGrid } from './utils'

export function Ratings(props) {
  return (
    <FlexRow style={{ justifyContent: 'flex-start' }}>
      {Array.from({ length: props.rating ?? 1 }).map(
        (_, i) => {
          return (
            <StarIcon
              style={{ width: 20, color: 'orange' }}
            />
          )
        },
      )}
    </FlexRow>
  )
}

export var ListCard = (props) => {
  return (
    <TwoColumnGrid
      style={{
        borderRadius: 10,
        flex: 1,
        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',
        overflow: 'hidden',
        height: '200px',
      }}
    >
      <div
        style={{
          fontSize: '18px',
          fontWeight: 700,
          padding: 8,
        }}
      >
        {props.name}
        <div
          style={{
            color: 'rgb(0, 0, 0, 0.5)',
            fontFamily: 'ITC Garamond Std',
            fontStyle: 'italic',
          }}
        >
          {props.country}
          <Ratings rating={props.rating} />
        </div>
      </div>
      <div
        style={{
          flex: 1,
          backgroundImage: \`url{\${props.image})\`,
          backgroundSize: 'cover',
          backgroundPosition: 'center 45%',
          backgroundRepeat: 'no-repeat',
        }}
      />
    </TwoColumnGrid>
  )
}
    `,
    ),
    '/src/listview.js': createCodeFile(
      '/src/listview.js',
      `import { ListCard } from './listcard.js'
import { FlexCol } from './utils'

export var ListView = (props) => {
  return (
    <>
      <span
        style={{
          color: 'rgb(0, 0, 0, 0.5)',
          fontSize: '16px',
        }}
      >
        Featured
      </span>
      <FlexCol style={{ gap: 8 }}>
        {props.cards.map((beach) => (
          <ListCard
            name={beach.name}
            country={beach.country}
            image={beach.image}
            rating={beach.rating}
          />
        ))}
      </FlexCol>
    </>
  )
}
    `,
    ),
    '/src/utils.js': createCodeFile(
      '/src/utils.js',
      `import * as React from 'react'

export function FlexRow({ children, style, ...props }) {
  return (
    <div
      {...props}
      style={{
        position: 'relative',
        display: 'flex',
        flexDirection: 'row',
        ...style,
      }}
    >
      {children}
    </div>
  )
}

export function FlexCol({ children, style, ...props }) {
  return (
    <div
      {...props}
      style={{
        position: 'relative',
        display: 'flex',
        flexDirection: 'column',
        ...style,
      }}
    >
      {children}
    </div>
  )
}

export function TwoColumnGrid({
  children,
  style,
  ...props
}) {
  return (
    <div
          {...props}
          style={{
            position: 'relative',
            display: 'grid',
            gridTemplateColumns: 'repeat(2, 1fr)',
            ...style,
          }}
    >
      {children}
    </div>
  )
}
    `,
    ),
  }
}

function beachesDefaultProject(): PersistentModel {
  const projectContents = createBeachesProjectContents()
  const persistentModel = persistentModelForProjectContents(contentsToTree(projectContents))
  return persistentModel
}

export function defaultProject(): PersistentModel {
  if (process.env.NODE_ENV === 'production') {
    return simpleDefaultProject()
  } else {
    return beachesDefaultProject()
  }
}
