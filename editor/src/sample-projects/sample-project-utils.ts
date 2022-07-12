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

function createBeachesProjectContents(): ProjectContentTreeRoot {
  return {
    'package.json': {
      content: {
        fileContents: {
          code: '{\n  "name": "Utopia Project",\n  "version": "0.1.0",\n  "utopia": {\n    "main-ui": "utopia/storyboard.js",\n    "html": "public/index.html",\n    "js": "src/index.js"\n  },\n  "dependencies": {\n    "react": "16.13.1",\n    "react-dom": "16.13.1",\n    "utopia-api": "0.4.1",\n    "react-spring": "8.0.27",\n    "@heroicons/react": "1.0.1",\n    "@emotion/react": "11.9.3"\n  }\n}',
          revisionsState: 'CODE_AHEAD',
          parsed: {
            type: 'UNPARSED',
          },
        },
        lastSavedContents: null,
        lastRevisedTime: 0,
        type: 'TEXT_FILE',
        lastParseSuccess: null,
      },
      type: 'PROJECT_CONTENT_FILE',
      fullPath: '/package.json',
    },
    utopia: {
      children: {
        'storyboard.js': {
          content: {
            fileContents: {
              code: "import * as React from 'react'\nimport { Scene, Storyboard } from 'utopia-api'\nimport { App } from '/src/app.js'\nimport { GridCard } from '/src/gridcard.js'\nimport { Button } from '/src/button.js'\n\nimport { SwitchHorizontalIcon } from '@heroicons/react/solid'\n\nexport var storyboard = (\n  <Storyboard>\n    <div\n      style={{\n        display: 'flex',\n        flexDirection: 'row',\n        gap: 100,\n      }}\n    >\n      <Scene\n        style={{\n          borderRadius: 5,\n          height: 'min-content',\n          width: 'min-content',\n        }}\n      >\n        <Button\n          icon={\n            <SwitchHorizontalIcon\n              style={{ width: 15, color: 'white' }}\n            />\n          }\n        />\n      </Scene>\n      <Scene\n        style={{\n          borderRadius: 10,\n          height: 'min-content',\n          width: 170,\n        }}\n      >\n        <GridCard\n          name='La Digue'\n          country='Seychelles'\n          image='https://source.unsplash.com/jPmurJKSL_0/600x800'\n          rating={5}\n        />\n      </Scene>\n      <Scene\n        style={{\n          width: 375,\n          height: 759,\n          display: 'flex',\n          flexDirection: 'column',\n        }}\n        data-label='Beaches'\n      >\n        <App style={{}} />\n      </Scene>\n    </div>\n  </Storyboard>\n)\n      ",
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 1657644426463,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/utopia/storyboard.js',
        },
      },
      type: 'PROJECT_CONTENT_DIRECTORY',
      directory: {
        type: 'DIRECTORY',
      },
      fullPath: '/utopia',
    },
    src: {
      children: {
        'app.js': {
          content: {
            fileContents: {
              code: "import { useState } from 'react'\nimport '../public/globals.css'\nimport { Button } from './button.js'\nimport { GridView } from './gridview.js'\nimport { ListView } from './listview.js'\nimport { FlexCol } from './utils'\nimport { beaches } from '../public/data.js'\n\nimport {\n  SwitchHorizontalIcon,\n  ViewGridIcon,\n  ViewListIcon,\n} from '@heroicons/react/solid'\n\nexport var App = () => {\n  const [displayedBeaches, setDisplayedBeaches] = useState(\n    beaches,\n  )\n\n  const handleShuffle = () => {\n    setDisplayedBeaches(\n      [...displayedBeaches].sort(() => Math.random() - 0.5),\n    )\n  }\n\n  const [gridView, setGrid] = useState(true)\n\n  return (\n    <FlexCol\n      style={{\n        width: '100%',\n        height: '100%',\n        padding: 8,\n        gap: 8,\n      }}\n    >\n      <div\n        style={{\n          display: 'flex',\n          flexDirection: 'row',\n          justifyContent: 'space-between',\n          alignItems: 'baseline',\n          gap: 70,\n        }}\n      >\n        <span\n          style={{\n            fontSize: '40px',\n            fontWeight: 700,\n            fontStyle: 'normal',\n          }}\n        >\n          Beaches\n        </span>\n        <div\n          style={{\n            display: 'flex',\n            flexDirection: 'row',\n            gap: 15,\n          }}\n        >\n          <Button\n            onClick={handleShuffle}\n            icon={\n              <SwitchHorizontalIcon\n                style={{ width: 15, color: 'white' }}\n              />\n            }\n          />\n          <div style={{ display: 'flex', gap: 3 }}>\n            <Button\n              onClick={() => setGrid(!gridView)}\n              icon={\n                <ViewGridIcon\n                  style={{ width: 15, color: 'white' }}\n                />\n              }\n              clickState={gridView}\n              unClickedState={false}\n            />\n            <Button\n              onClick={() => setGrid(!gridView)}\n              icon={\n                <ViewListIcon\n                  style={{ width: 15, color: 'white' }}\n                />\n              }\n              clickState={gridView}\n              unClickedState\n            />\n          </div>\n        </div>\n      </div>\n      {gridView ? (\n        <GridView cards={displayedBeaches} />\n      ) : (\n        <ListView cards={displayedBeaches} />\n      )}\n    </FlexCol>\n  )\n}\n      ",
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 1657644426463,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/app.js',
        },
        'utils.js': {
          content: {
            fileContents: {
              code: "import * as React from 'react'\n\nexport function FlexRow({ children, style, ...props }) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'row',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function FlexCol({ children, style, ...props }) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'column',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function TwoColumnGrid({\n  children,\n  style,\n  ...props\n}) {\n  return (\n    <div\n          {...props}\n          style={{\n            position: 'relative',\n            display: 'grid',\n            gridTemplateColumns: 'repeat(2, 1fr)',\n            ...style,\n          }}\n    >\n      {children}\n    </div>\n  )\n}\n    ",
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 1657644426463,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/utils.js',
        },
        'listview.js': {
          content: {
            fileContents: {
              code: "import { ListCard } from './listcard.js'\nimport { FlexCol } from './utils'\n\nexport var ListView = (props) => {\n  return (\n    <>\n      <span\n        style={{\n          color: 'rgb(0, 0, 0, 0.5)',\n          fontSize: '16px',\n        }}\n      >\n        Featured\n      </span>\n      <FlexCol style={{ gap: 8 }}>\n        {props.cards.map((beach) => (\n          <ListCard\n            name={beach.name}\n            country={beach.country}\n            image={beach.image}\n            rating={beach.rating}\n          />\n        ))}\n      </FlexCol>\n    </>\n  )\n}\n    ",
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 1657644426463,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/listview.js',
        },
        'listcard.js': {
          content: {
            fileContents: {
              code: "import { StarIcon } from '@heroicons/react/solid'\nimport { FlexRow, TwoColumnGrid } from './utils'\n\nexport function Ratings(props) {\n  return (\n    <FlexRow style={{ justifyContent: 'flex-start' }}>\n      {Array.from({ length: props.rating ?? 1 }).map(\n        (_, i) => {\n          return (\n            <StarIcon\n              style={{ width: 20, color: 'orange' }}\n            />\n          )\n        },\n      )}\n    </FlexRow>\n  )\n}\n\nexport var ListCard = (props) => {\n  return (\n    <TwoColumnGrid\n      style={{\n        borderRadius: 10,\n        flex: 1,\n        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',\n        overflow: 'hidden',\n        height: '200px',\n      }}\n    >\n      <div\n        style={{\n          fontSize: '18px',\n          fontWeight: 700,\n          padding: 8,\n        }}\n      >\n        {props.name}\n        <div\n          style={{\n            color: 'rgb(0, 0, 0, 0.5)',\n            fontFamily: 'ITC Garamond Std',\n            fontStyle: 'italic',\n          }}\n        >\n          {props.country}\n          <Ratings rating={props.rating} />\n        </div>\n      </div>\n      <div\n        style={{\n          flex: 1,\n          backgroundImage: `url{${props.image})`,\n          backgroundSize: 'cover',\n          backgroundPosition: 'center 45%',\n          backgroundRepeat: 'no-repeat',\n        }}\n      />\n    </TwoColumnGrid>\n  )\n}\n    ",
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 1657644426463,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/listcard.js',
        },
        'index.js': {
          content: {
            fileContents: {
              code: 'import * as React from "react";\nimport * as ReactDOM from "react-dom";\nimport { App } from "../src/app";\n\nconst root = document.getElementById("root");\nif (root != null) {\n  ReactDOM.render(<App />, root);\n}',
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 1657644426463,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/index.js',
        },
        'gridview.js': {
          content: {
            fileContents: {
              code: "import * as React from 'react'\nimport { GridCard } from './gridcard.js'\nimport { TwoColumnGrid } from './utils'\n\nexport var GridView = (props) => {\n  return (\n    <>\n      <span\n        style={{\n          color: 'rgb(0, 0, 0, 0.5)',\n          fontSize: '16px',\n        }}\n      >\n        Featured\n      </span>\n      <TwoColumnGrid style={{ gap: 8 }}>\n        {props.cards.map((beach) => (\n          <GridCard\n            name={beach.name}\n            country={beach.country}\n            image={beach.image}\n            rating={beach.rating}\n          />\n        ))}\n      </TwoColumnGrid>\n    </>\n  )\n}\n    ",
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 1657644426463,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/gridview.js',
        },
        'gridcard.js': {
          content: {
            fileContents: {
              code: "import { StarIcon } from '@heroicons/react/solid'\nimport { FlexCol, FlexRow } from './utils'\nimport { Rectangle } from 'utopia-api'\n\nexport function Ratings(props) {\n  return (\n    <FlexRow style={{ justifyContent: 'flex-start' }}>\n      {Array.from({ length: props.rating ?? 1 }).map(\n        (_, i) => {\n          return (\n            <StarIcon\n              style={{ width: 20, color: 'orange' }}\n            />\n          )\n        },\n      )}\n    </FlexRow>\n  )\n}\n\nexport var GridCard = (props) => {\n  return (\n    <FlexCol\n      style={{\n        backgroundColor: 'white',\n        borderRadius: 10,\n        flex: 1,\n        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',\n        paddingBottom: 5,\n        overflow: 'hidden',\n        height: 326,\n        ...props.style,\n      }}\n    >\n      <div\n        style={{\n          flex: 1,\n          backgroundImage: `url(${props.image})`,\n          backgroundSize: 'cover',\n          flexBasis: 150,\n          marginBottom: 8,\n        }}\n      />\n      <div\n        style={{\n          position: 'relative',\n          paddingLeft: 5,\n          fontSize: '18px',\n          fontWeight: 700,\n          fontStyle: 'normal',\n        }}\n      >\n        {props.name}\n      </div>\n      <div\n        style={{\n          position: 'relative',\n          paddingLeft: 5,\n          color: 'rgb(0, 0, 0, 0.5)',\n          fontFamily: 'ITC Garamond Std',\n          fontStyle: 'italic',\n        }}\n      >\n        {props.country}\n      </div>\n      <Ratings rating={props.rating} />\n    </FlexCol>\n  )\n}\n    ",
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 1657644426463,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/gridcard.js',
        },
        'button.js': {
          content: {
            fileContents: {
              code: "/** @jsx jsx */\nimport { jsx, ThemeProvider } from '@emotion/react'\n\nconst theme = {\n  colors: {\n    primaryBlue: '#93D9FF',\n    darkBlue: '#2ab4ff',\n  },\n}\n\nexport var Button = ({\n  clickState,\n  unClickedState,\n  ...buttonProps\n}) => {\n  return (\n    <button\n      {...buttonProps}\n      css={{\n        border: 'none',\n        borderRadius: '5px',\n        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',\n        padding: '5px',\n        display: 'flex',\n        justifyContent: 'center',\n        alignItems: 'center',\n        '&:hover': {\n          opacity: 0.7,\n        },\n      }}\n      style={{\n        backgroundColor:\n          clickState === unClickedState\n            ? theme.colors.primaryBlue\n            : theme.colors.darkBlue,\n      }}\n    >\n      {buttonProps.icon}\n    </button>\n  )\n}\n      ",
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 1657644426463,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/button.js',
        },
      },
      type: 'PROJECT_CONTENT_DIRECTORY',
      directory: {
        type: 'DIRECTORY',
      },
      fullPath: '/src',
    },
    assets: {
      children: {},
      type: 'PROJECT_CONTENT_DIRECTORY',
      directory: {
        type: 'DIRECTORY',
      },
      fullPath: '/assets',
    },
    public: {
      children: {
        'index.html': {
          content: {
            fileContents: {
              code: '<!DOCTYPE html>\n<html lang="en">\n  <head>\n    <meta charset="utf-8">\n    <title>Utopia React App</title>\n    <!-- Begin Generated Utopia External Links -->\n    <!-- End Generated Utopia External Links -->\n  </head>\n  <body>\n    <div id="root"></div>\n  </body>\n</html>',
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 0,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/public/index.html',
        },
        'globals.css': {
          content: {
            fileContents: {
              code: 'body, #canvas-container * {\n      font-family: San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";\n  }\n  \n  @font-face {\n      font-family: \'ITC Garamond \';\n      font-style: normal;\n      font-weight: 400;\n      font-display: swap;\n      src: local(ITC Garamond ) format(\'ttf\');\n    }',
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 0,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/public/globals.css',
        },
        'data.js': {
          content: {
            fileContents: {
              code: "export const beaches = [\n        {\n          name: 'La Digue',\n          country: 'Seychelles',\n          image:\n            'https://source.unsplash.com/jPmurJKSL_0/600x800',\n          rating: 5,\n        },\n        {\n          name: 'Isle of Pines',\n          country: 'New Caledonia',\n          image:\n            'https://source.unsplash.com/n7DY58YFg9E/600x800',\n          rating: 5,\n        },\n        {\n          name: 'McWay Falls',\n          country: 'California',\n          image:\n            'https://images.unsplash.com/photo-1631699447911-91183ecba384?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=987&q=80',\n          rating: 3,\n        },\n        {\n          name: 'Meeru Island',\n          country: 'Maldives',\n          image:\n            'https://source.unsplash.com/8OGJqpNMBGM/600x800',\n          rating: 4,\n        },\n      ]\n      ",
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
            lastSavedContents: null,
            lastRevisedTime: 0,
            type: 'TEXT_FILE',
            lastParseSuccess: null,
          },
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/public/data.js',
        },
      },
      type: 'PROJECT_CONTENT_DIRECTORY',
      directory: {
        type: 'DIRECTORY',
      },
      fullPath: '/public',
    },
  }
}

function beachesDefaultProject(): PersistentModel {
  const projectContents = createBeachesProjectContents()
  const persistentModel = persistentModelForProjectContents(projectContents)
  return persistentModel
}

export function defaultProject(): PersistentModel {
  if (process.env.NODE_ENV === 'production') {
    return simpleDefaultProject()
  } else {
    return beachesDefaultProject()
  }
}
