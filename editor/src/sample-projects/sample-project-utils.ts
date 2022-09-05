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
import { Spring } from 'non-existant-dummy-library'
export var Card = (props) => {
  return <div data-uid='card-outer-div' style={{...props.style}}>
    <div data-uid='card-inner-div' style={{ position: 'absolute', left: 0, top: 0, width: 50, height: 50, backgroundColor: 'red' }} />
    <Spring data-uid='card-inner-spring' data-testid='spring' style={{ position: 'absolute', left: 100, top: 200, width: 50, height: 50, backgroundColor: 'blue' }} />
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
  // to update the default project here, just delete the big object that is returned here, access the new project's contents.json
  // using the link https://utopia.pizza/v1/project/<project-id>/contents.json and paste the contents of projectContents down below

  return {
    src: {
      directory: {
        type: 'DIRECTORY',
      },
      type: 'PROJECT_CONTENT_DIRECTORY',
      children: {
        'button.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/button.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "/** @jsx jsx */\nimport { jsx } from '@emotion/react'\n\nconst theme = {\n  colors: {\n    primaryBlue: '#93D9FF',\n    darkBlue: '#2ab4ff',\n  },\n}\n\nexport var Button = ({ selected, ...buttonProps }) => {\n  return (\n    <button\n      {...buttonProps}\n      css={{\n        border: 'none',\n        borderRadius: '5px',\n        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',\n        padding: '5px',\n        display: 'flex',\n        justifyContent: 'center',\n        alignItems: 'center',\n        '&:hover': { opacity: 0.7 },\n      }}\n      style={{\n        ...buttonProps.style,\n        backgroundColor: selected\n          ? theme.colors.darkBlue\n          : theme.colors.primaryBlue,\n      }}\n    >\n      {buttonProps.icon}\n    </button>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'grid-view.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/grid-view.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import { GridCard } from './grid-card.js'\nimport { TwoColumnGrid } from './utils'\n\nexport var GridView = (props) => {\n  return (\n    <div style={{ overflow: 'scroll', height: '100%' }}>\n      <span\n        style={{\n          color: 'rgb(0, 0, 0, 0.5)',\n          fontSize: '16px',\n        }}\n      >\n        Featured\n      </span>\n      <TwoColumnGrid\n        className='gridcardcontainer'\n        style={{ marginTop: 8, gap: 8 }}\n      >\n        {props.cards.map((beach) => (\n          <GridCard\n            key={beach.name}\n            name={beach.name}\n            country={beach.country}\n            image={beach.image}\n            rating={beach.rating}\n          />\n        ))}\n      </TwoColumnGrid>\n    </div>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'app2.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/app2.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import { useState, useEffect, useCallback } from 'react'\nimport '../public/globals.css'\nimport { Button } from './button.js'\nimport { FlexCol } from './utils'\nimport { ThreeColumnGridView } from '/src/three-column-grid-view.js'\n\nimport { SwitchHorizontalIcon } from '@heroicons/react/solid'\n\nconst PROJECT_ID = 'ez1bx506'\nconst DATASET = 'production'\nconst QUERY = encodeURIComponent('*[_type == \"beach\"]')\n\nconst URL = `https://${PROJECT_ID}.api.sanity.io/v2021-10-21/data/query/${DATASET}?query=${QUERY}`\n\nexport var App2 = () => {\n  const [displayedBeaches, setDisplayedBeaches] = useState(\n    [],\n  )\n\n  useEffect(() => {\n    fetch(URL)\n      .then((res) => res.json())\n      .then(({ result }) => {\n        if (result.length > 0) {\n          result.forEach((beach) => {\n            setDisplayedBeaches((oldArray) => [\n              ...oldArray,\n              beach,\n            ])\n          })\n        }\n      })\n      .catch((err) => console.error(err))\n  }, [])\n\n  const handleShuffle = useCallback(() => {\n    setDisplayedBeaches((beaches) =>\n      [...beaches].sort(() => Math.random() - 0.5),\n    )\n  }, [])\n\n  return (\n    <FlexCol\n      style={{\n        width: '100%',\n        height: '100%',\n        padding: 8,\n        gap: 8,\n        background: 'white',\n      }}\n    >\n      <div\n        className='appheadercontainer'\n        style={{\n          display: 'flex',\n          flexDirection: 'row',\n          justifyContent: 'space-between',\n          alignItems: 'baseline',\n          gap: 70,\n        }}\n      >\n        <h1\n          className='apptitle'\n          style={{\n            fontFamily: 'ITC Garamond Std',\n            fontWeight: 700,\n            margin: 0,\n          }}\n        >\n          Find Your Beach\n        </h1>\n        <div\n          style={{\n            display: 'flex',\n            flexDirection: 'row',\n            gap: 15,\n          }}\n        >\n          <Button\n            onClick={handleShuffle}\n            icon={\n              <SwitchHorizontalIcon\n                style={{ width: 15, color: 'white' }}\n              />\n            }\n          />\n        </div>\n      </div>\n      <ThreeColumnGridView cards={displayedBeaches} />\n    </FlexCol>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'index.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/index.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import * as React from 'react'\nimport * as ReactDOM from 'react-dom'\nimport { App } from '../src/app'\n\nconst root = document.getElementById('root')\nif (root != null) {\n  ReactDOM.render(<App />, root)\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'three-column-grid-view.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/three-column-grid-view.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import { GridCard } from './grid-card.js'\nimport { ThreeColumnGrid } from './utils'\n\nexport var ThreeColumnGridView = (props) => {\n  return (\n    <div style={{ overflow: 'scroll' }}>\n      <ThreeColumnGrid\n        className='gridcardcontainer'\n        style={{ marginTop: 8, gap: 8 }}\n      >\n        {props.cards.map((beach) => (\n          <GridCard\n            key={beach.name}\n            name={beach.name}\n            country={beach.country}\n            image={beach.image}\n            rating={beach.rating}\n          />\n        ))}\n      </ThreeColumnGrid>\n    </div>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'list-view.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/list-view.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import { ListCard } from './list-card.js'\nimport { FlexCol } from './utils'\n\nexport var ListView = (props) => {\n  return (\n    <div style={{ overflow: 'scroll' }}>\n      <span\n        style={{\n          color: 'rgb(0, 0, 0, 0.5)',\n          fontSize: '16px',\n        }}\n      >\n        Featured\n      </span>\n      <FlexCol\n        className='listcardcontainer'\n        style={{ marginTop: 8, gap: 8 }}\n      >\n        {props.cards.map((beach) => (\n          <ListCard\n            key={beach.name}\n            name={beach.name}\n            country={beach.country}\n            image={beach.image}\n            rating={beach.rating}\n          />\n        ))}\n      </FlexCol>\n    </div>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'utils.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/utils.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import * as React from 'react'\n\nexport function FlexRow({ children, style, ...props }) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'row',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function FlexCol({ children, style, ...props }) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'column',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function TwoColumnGrid({\n  children,\n  style,\n  ...props\n}) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'grid',\n        gridTemplateColumns: 'repeat(2, 1fr)',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function ThreeColumnGrid({\n  children,\n  style,\n  ...props\n}) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'grid',\n        gridTemplateColumns: '1fr 1fr 1fr',\n        width: '100%',\n        hieght: '100%',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'themed-card.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/themed-card.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "/** @jsx jsx */\nimport { jsx, ThemeProvider } from '@emotion/react'\nimport * as React from 'react'\nimport { SimpleCard } from '/src/simple-card.js'\n\nexport var ThemedCard = () => {\n  const theme = {\n    colors: {\n      primary: '#685B38',\n      secondary: 'white',\n    },\n    spacing: {\n      primary: '20px',\n    },\n  }\n  return (\n    <>\n      <ThemeProvider theme={theme}>\n        <SimpleCard label='Seychelles' title='La Digue'>\n          <img\n            src='https://source.unsplash.com/jPmurJKSL_0/600x800'\n            style={{ width: '100%' }}\n            alt='beach'\n          />\n          <div\n            css={(theme) => ({\n              color: theme.colors.primary,\n              backgroundColor: theme.colors.secondary,\n              padding: theme.spacing.primary,\n            })}\n            style={{\n              fontFamily: 'ITC Garamond',\n              fontSize: '14pt',\n            }}\n          >\n            La Digue, Seychelles\n          </div>\n        </SimpleCard>\n      </ThemeProvider>\n    </>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'playground.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/playground.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import '../public/globals.css'\nimport { Button } from './button.js'\nimport { GridCard, Ratings } from '/src/grid-card.js'\nimport { ListCard } from '/src/list-card.js'\n\nimport {\n  SwitchHorizontalIcon,\n  ViewGridIcon,\n  ViewListIcon,\n} from '@heroicons/react/solid'\n\nexport var Playground = () => {\n  return (\n    <div\n      style={{\n      width: '100%',\n      height: '100%',\n      position: 'relative',\n    }}\n    >\n      <span\n        style={{\n          fontSize: '40px',\n          fontWeight: 700,\n          fontStyle: 'normal',\n          position: 'absolute',\n          left: 13,\n          top: 5,\n          height: 71,\n        }}\n      >\n        Beaches!\n      </span>\n      <span\n        style={{\n          color: 'rgb(0, 0, 0, 0.5)',\n          fontSize: '16px',\n          position: 'absolute',\n          left: 238,\n          top: 641,\n        }}\n      >\n        Featured\n      </span>\n      <GridCard\n        name='La Digue'\n        country='Seychelles'\n        image='https://source.unsplash.com/jPmurJKSL_0/600x800'\n        rating={5}\n        style={{\n          width: '54%',\n          position: 'absolute',\n          left: 301,\n          top: 262,\n          height: 352,\n        }}\n      />\n      <ListCard\n        name='La Digue'\n        country='Seychelles'\n        image='https://source.unsplash.com/jPmurJKSL_0/600x800'\n        rating={5}\n        style={{\n          position: 'absolute',\n          width: '68.8%',\n          left: 62,\n          top: 121,\n          bottom: 550,\n        }}\n      />\n      <Button\n        style={{ position: 'absolute', left: 35, top: 262 }}\n        icon={\n          <SwitchHorizontalIcon\n            style={{\n              width: '45px',\n              color: 'white',\n            }}\n          />\n        }\n      />\n      <Button\n        style={{ position: 'absolute', left: 10, top: 180 }}\n        icon={\n          <ViewGridIcon\n            style={{ width: 15, color: 'white' }}\n          />\n        }\n      />\n      <Button\n        style={{ position: 'absolute', left: 10, top: 213 }}\n        icon={\n          <ViewListIcon\n            style={{ width: 15, color: 'white' }}\n          />\n        }\n      />\n      <div\n        style={{\n          width: '200px',\n          height: 258,\n          position: 'absolute',\n          backgroundColor: '#FFF8DE',\n          left: 52,\n          top: 362,\n        }}\n      >\n        <img\n          src='https://source.unsplash.com/jPmurJKSL_0/600x800'\n          style={{\n            width: '180px',\n            position: 'absolute',\n            left: -35,\n            top: 102,\n            height: -41,\n            boxShadow: '2px 2px 6px 4px rgb(0, 0, 0, 0.12)',\n          }}\n          alt='beach'\n        />\n        <div\n          style={{\n            position: 'absolute',\n            fontSize: '18px',\n            fontWeight: 700,\n            fontStyle: 'normal',\n            left: 5,\n            top: 8,\n          }}\n        >\n          La Digue\n        </div>\n        <div\n          style={{\n            position: 'absolute',\n            fontStyle: 'italic',\n            left: 5,\n            top: 30,\n          }}\n        >\n          Seychelles\n        </div>\n        <Ratings\n          rating={5}\n          style={{\n            position: 'absolute',\n            left: 120,\n            top: 186,\n            transform: 'rotate(90deg)',\n          }}\n        />\n        <div\n          style={{\n            position: 'absolute',\n            color: 'rgb(0, 0, 0, 0.5)',\n            fontFamily: 'ITC Garamond Std',\n            fontStyle: 'italic',\n            left: 7,\n            top: 76,\n          }}\n        >\n          August 9th, 1924\n        </div>\n      </div>\n      <span\n        style={{\n          fontSize: '40px',\n          fontWeight: 700,\n          fontStyle: 'normal',\n          position: 'absolute',\n          left: 194,\n          top: 41,\n          height: 71,\n          fontFamily: 'ITC Garamond Std',\n        }}\n      >\n        ...Beaches?\n      </span>\n      <span\n        style={{\n          fontSize: '40px',\n          fontWeight: 700,\n          fontStyle: 'normal',\n          position: 'absolute',\n          left: 442,\n          top: 41,\n          height: 71,\n          fontFamily: 'ITC Garamond Std',\n        }}\n      >\n        Find Your Beach\n      </span>\n      <span\n        style={{\n          color: 'rgb(0, 0, 0, 1)',\n          fontSize: '16px',\n          position: 'absolute',\n          left: 238,\n          top: 669,\n        }}\n      >\n        Featured\n      </span>\n      <span\n        style={{\n          color: 'rgb(0, 0, 0, 1)',\n          fontSize: '20px',\n          fontFamily: 'ITC Garamond Std',\n          position: 'absolute',\n          left: 238,\n          top: 695,\n        }}\n      >\n        Featured\n      </span>\n    </div>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'simple-card.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/simple-card.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: 'export var SimpleCard = (props) => {\n  return <>{props.children}</>\n}\n',
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'grid-card.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/grid-card.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import * as React from 'react'\nimport { StarIcon } from '@heroicons/react/solid'\nimport { FlexCol, FlexRow } from './utils'\n\nexport function Ratings(props) {\n  return (\n    <FlexRow\n      style={{\n        justifyContent: 'flex-start',\n        paddingBottom: 5,\n        ...props.style,\n      }}\n    >\n      {Array.from({ length: props.rating ?? 1 }).map(\n        (_, i) => {\n          return (\n            <StarIcon\n              key={i}\n              style={{ width: 20, color: 'orange' }}\n            />\n          )\n        },\n      )}\n    </FlexRow>\n  )\n}\n\nexport var GridCard = (props) => {\n  return (\n    <FlexCol\n      style={{\n        backgroundColor: 'white',\n        borderRadius: 10,\n        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',\n        paddingBottom: 5,\n        overflow: 'hidden',\n        height: '325px',\n        ...props.style,\n      }}\n    >\n      <div\n        style={{\n          flex: 1,\n          backgroundImage: `url(${props.image})`,\n          backgroundSize: 'cover',\n          backgroundPosition: 'center center',\n          flexBasis: 190,\n          marginBottom: 8,\n        }}\n      />\n      <div\n        style={{\n          position: 'relative',\n          paddingLeft: 8,\n          paddingBottom: 4,\n          fontSize: '18px',\n          fontWeight: 700,\n          fontStyle: 'normal',\n        }}\n      >\n        {props.name}\n      </div>\n      <div\n        style={{\n          position: 'relative',\n          paddingLeft: 8,\n          color: 'rgb(0, 0, 0, 0.5)',\n          fontFamily: 'ITC Garamond Std',\n          fontStyle: 'italic',\n        }}\n      >\n        {props.country}\n      </div>\n      <Ratings\n        rating={props.rating}\n        style={{ paddingLeft: 5 }}\n      />\n    </FlexCol>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'app.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/app.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import { useState, useEffect, useCallback } from 'react'\nimport '../public/globals.css'\nimport { Button } from './button.js'\nimport { GridView } from './grid-view.js'\nimport { ListView } from './list-view.js'\nimport { FlexCol } from './utils'\n\nimport {\n  SwitchHorizontalIcon,\n  ViewGridIcon,\n  ViewListIcon,\n} from '@heroicons/react/solid'\n\nconst PROJECT_ID = 'ez1bx506'\nconst DATASET = 'production'\nconst QUERY = encodeURIComponent('*[_type == \"beach\"]')\n\nconst URL = `https://${PROJECT_ID}.api.sanity.io/v2021-10-21/data/query/${DATASET}?query=${QUERY}`\n\nexport var App = () => {\n  const [displayedBeaches, setDisplayedBeaches] = useState(\n    [],\n  )\n\n  useEffect(() => {\n    fetch(URL)\n      .then((res) => res.json())\n      .then(({ result }) => {\n        if (result.length > 0) {\n          result.forEach((beach) => {\n            setDisplayedBeaches((oldArray) => [\n              ...oldArray,\n              beach,\n            ])\n          })\n        }\n      })\n      .catch((err) => console.error(err))\n  }, [])\n\n  const handleShuffle = useCallback(() => {\n    setDisplayedBeaches((beaches) =>\n      [...beaches].sort(() => Math.random() - 0.5),\n    )\n  }, [])\n\n  const [gridView, setGrid] = useState(false)\n  const toggleGrid = useCallback(\n    () => setGrid((grid) => !grid),\n    [],\n  )\n\n  return (\n    <FlexCol\n      style={{\n        width: '100%',\n        height: '100%',\n        padding: 8,\n        gap: 8,\n        background: 'white',\n      }}\n    >\n      <div\n        className='appheadercontainer'\n        style={{\n          display: 'flex',\n          flexDirection: 'row',\n          justifyContent: 'space-between',\n          alignItems: 'baseline',\n          gap: 70,\n        }}\n      >\n        <h1\n          className='apptitle'\n          style={{\n            fontFamily: 'ITC Garamond Std',\n            fontWeight: 700,\n            margin: 0,\n          }}\n        >\n          Find Your Beach\n        </h1>\n        <div\n          style={{\n            display: 'flex',\n            flexDirection: 'row',\n            gap: 15,\n          }}\n        >\n          <Button\n            onClick={handleShuffle}\n            icon={\n              <SwitchHorizontalIcon\n                style={{ width: 15, color: 'white' }}\n              />\n            }\n          />\n          <div style={{ display: 'flex', gap: 3 }}>\n            <Button\n              onClick={toggleGrid}\n              icon={\n                <ViewGridIcon\n                  style={{ width: 15, color: 'white' }}\n                />\n              }\n              selected={gridView}\n              style={{ flexBasis: 0 }}\n            />\n            <Button\n              onClick={toggleGrid}\n              icon={\n                <ViewListIcon\n                  style={{ width: 15, color: 'white' }}\n                />\n              }\n              selected={!gridView}\n              unClickedState\n            />\n          </div>\n        </div>\n      </div>\n      {gridView ? (\n        <GridView cards={displayedBeaches} />\n      ) : (\n        <ListView cards={displayedBeaches} />\n      )}\n    </FlexCol>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'card.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/card.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "export var Card = (props) => {\n  return (\n    <>\n      <img\n        src='https://source.unsplash.com/jPmurJKSL_0/600x800'\n        style={{ width: '100%' }}\n        alt='beach'\n      />\n      <div\n        style={{\n          color: '#685B38',\n          backgroundColor: 'white',\n          padding: 20,\n          fontFamily: 'ITC Garamond',\n          fontSize: '14pt',\n        }}\n      >\n        {props.title}, {props.label}\n        <br />\n        {props.copy}\n      </div>\n    </>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'multiple-providers-card.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/multiple-providers-card.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "/** @jsx jsx */\nimport { jsx, ThemeProvider } from '@emotion/react'\nimport * as React from 'react'\nimport { SimpleCard } from '/src/simple-card.js'\n\nexport var MultipleProvidersCard = () => {\n  const theme = {\n    colors: {\n      primary: '#685B38',\n      secondary: 'white',\n    },\n    spacing: {\n      primary: '10px',\n    },\n  }\n\n  const theme2022 = {\n    spacing: {\n      primary: '20px',\n    },\n  }\n\n  return (\n    <>\n      <ThemeProvider theme={theme}>\n        <ThemeProvider theme={theme2022}>\n          <SimpleCard label='Seychelles' title='La Digue'>\n            <img\n              src='https://source.unsplash.com/jPmurJKSL_0/600x800'\n              style={{ width: '100%' }}\n              alt='beach'\n            />\n            <div\n              css={(theme) => ({\n                color: theme.colors.primary,\n                backgroundColor: theme.colors.secondary,\n                padding: theme.spacing.primary,\n              })}\n              style={{\n                fontFamily: 'ITC Garamond',\n                fontSize: '14pt',\n              }}\n            >\n              La Digue, Seychelles\n            </div>\n          </SimpleCard>\n        </ThemeProvider>\n      </ThemeProvider>\n    </>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'list-card.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/list-card.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import * as React from 'react'\nimport { StarIcon } from '@heroicons/react/solid'\nimport { FlexRow, TwoColumnGrid } from './utils'\n\nexport function Ratings(props) {\n  return (\n    <FlexRow style={{ justifyContent: 'flex-start' }}>\n      {Array.from({ length: props.rating ?? 1 }).map(\n        (_, i) => {\n          return (\n            <StarIcon\n              key={i}\n              style={{ width: 20, color: 'orange' }}\n            />\n          )\n        },\n      )}\n    </FlexRow>\n  )\n}\n\nexport var ListCard = (props) => {\n  return (\n    <TwoColumnGrid\n      className='listcard'\n      style={{\n        backgroundColor: 'white',\n        borderRadius: 10,\n        boxShadow: '0px 2px 4px rgb(0, 0, 0, 0.12)',\n        overflow: 'hidden',\n        ...props.style,\n      }}\n    >\n      <div\n        style={{\n          fontSize: '18px',\n          fontWeight: 700,\n          padding: 8,\n          paddingBottom: 4,\n        }}\n      >\n        {props.name}\n        <div\n          style={{\n            color: 'rgb(0, 0, 0, 0.5)',\n            fontFamily: 'ITC Garamond Std',\n            fontStyle: 'italic',\n          }}\n        >\n          {props.country}\n          <Ratings rating={props.rating} />\n        </div>\n      </div>\n      <div\n        style={{\n          flex: 1,\n          backgroundImage: `url(${props.image})`,\n          backgroundSize: 'cover',\n          backgroundPosition: 'center 45%',\n          backgroundRepeat: 'no-repeat',\n        }}\n      />\n    </TwoColumnGrid>\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
      },
      fullPath: '/src',
    },
    utopia: {
      directory: {
        type: 'DIRECTORY',
      },
      type: 'PROJECT_CONTENT_DIRECTORY',
      children: {
        'storyboard.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/utopia/storyboard.js',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import * as React from 'react'\nimport { Scene, Storyboard } from 'utopia-api'\nimport { App } from '/src/app.js'\nimport { App2 } from '/src/app2.js'\nimport { Card } from '/src/card.js'\nimport { ThemedCard } from '/src/themed-card.js'\nimport { MultipleProvidersCard } from '/src/multiple-providers-card.js'\nimport { SimpleCard } from '/src/simple-card.js'\nimport { Playground } from '/src/playground.js'\n\nexport var storyboard = (\n  <Storyboard>\n    <Scene\n      style={{\n        position: 'absolute',\n        width: 700,\n        height: 759,\n        backgroundColor: 'rgb(255, 255, 255, 0)',\n        border: '1px dashed rgb(0, 0, 0, 1)',\n        left: 208,\n        top: 128,\n      }}\n      data-label='Playground'\n    >\n      <Playground style={{}} />\n    </Scene>\n    <Scene\n      style={{\n        width: 375,\n        height: 759,\n        display: 'flex',\n        flexDirection: 'column',\n        left: 1072,\n        top: 128,\n      }}\n      data-label='Mobile List'\n    >\n      <App style={{}} />\n    </Scene>\n    <Scene\n      style={{\n        width: 744,\n        height: 1133,\n        display: 'flex',\n        flexDirection: 'column',\n        left: 1568,\n        top: 128,\n        position: 'absolute',\n      }}\n      data-label='Tablet List'\n    >\n      <App style={{}} />\n    </Scene>\n    <Scene\n      style={{\n        width: 744,\n        height: 1133,\n        display: 'flex',\n        flexDirection: 'column',\n        left: 2400,\n        top: 128,\n        position: 'absolute',\n      }}\n      data-label='Grid Layout Component in default size'\n    >\n      <App2 />\n    </Scene>\n    <Scene\n      style={{\n        width: 200,\n        display: 'flex',\n        flexDirection: 'column',\n        left: -6,\n        top: 1060,\n        position: 'absolute',\n      }}\n      data-label='Opinionated component with contents and no children prop'\n    >\n      <Card\n        label='Seychelles'\n        title='La Digue'\n        copy={\n          <span style={{ fontSize: '.5em' }}>\n            Most popular beach of 2022\n          </span>\n        }\n        imgURL='https://source.unsplash.com/jPmurJKSL_0/600x800'\n      />\n    </Scene>\n    <Scene\n      style={{\n        width: 200,\n        display: 'flex',\n        flexDirection: 'column',\n        left: 233,\n        top: 1060,\n        position: 'absolute',\n      }}\n      data-label='Opinionated component Prominent support for children'\n    >\n      <SimpleCard label='Seychelles' title='La Digue'>\n        <img\n          src='https://source.unsplash.com/jPmurJKSL_0/600x800'\n          style={{ width: '100%' }}\n          alt='beach'\n        />\n        <div\n          style={{\n            color: '#685B38',\n            backgroundColor: 'white',\n            padding: 20,\n            fontFamily: 'ITC Garamond',\n            fontSize: '14pt',\n          }}\n        >\n          La Digue, Seychelles\n        </div>\n      </SimpleCard>\n    </Scene>\n    <Scene\n      style={{\n        width: 200,\n        display: 'flex',\n        flexDirection: 'column',\n        left: 473,\n        top: 1060,\n        position: 'absolute',\n      }}\n      data-label='Card wrapped in a Theme Provider'\n    >\n      <ThemedCard />\n    </Scene>\n    <Scene\n      style={{\n        width: 200,\n        display: 'flex',\n        flexDirection: 'column',\n        left: 708,\n        top: 1060,\n        position: 'absolute',\n      }}\n      data-label='Card wrapped in multiple Theme Providers'\n    >\n      <MultipleProvidersCard />\n    </Scene>\n  </Storyboard>\n)\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
      },
      fullPath: '/utopia',
    },
    assets: {
      directory: {
        type: 'DIRECTORY',
      },
      type: 'PROJECT_CONTENT_DIRECTORY',
      children: {},
      fullPath: '/assets',
    },
    'package.json': {
      type: 'PROJECT_CONTENT_FILE',
      fullPath: '/package.json',
      content: {
        type: 'TEXT_FILE',
        lastRevisedTime: 0,
        lastParseSuccess: null,
        lastSavedContents: null,
        fileContents: {
          code: '{\n  "name": "Utopia Project",\n  "version": "0.1.0",\n  "utopia": {\n    "main-ui": "utopia/storyboard.js",\n    "html": "public/index.html",\n    "js": "src/index.js"\n  },\n  "dependencies": {\n    "react": "16.13.1",\n    "react-dom": "16.13.1",\n    "utopia-api": "0.4.1",\n    "react-spring": "8.0.27",\n    "@heroicons/react": "1.0.1",\n    "@emotion/react": "11.9.3"\n  }\n}',
          parsed: {
            type: 'UNPARSED',
          },
          revisionsState: 'CODE_AHEAD',
        },
      },
    },
    public: {
      directory: {
        type: 'DIRECTORY',
      },
      type: 'PROJECT_CONTENT_DIRECTORY',
      children: {
        'index.html': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/public/index.html',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: '<!DOCTYPE html>\n<html lang="en">\n  <head>\n    <meta charset="utf-8">\n    <title>Utopia React App</title>\n    <!-- Begin Generated Utopia External Links -->\n    <link href="https://fonts.googleapis.com/css2?family=Alegreya+SC:ital,wght@0,700" rel="stylesheet">\n    <link href="https://fonts.googleapis.com/css2?family=Cormorant+Garamond:ital,wght@0,700" rel="stylesheet">\n    <!-- End Generated Utopia External Links -->\n  </head>\n  <body>\n    <div id="root"></div>\n  </body>\n</html>',
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
        'globals.css': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/public/globals.css',
          content: {
            type: 'TEXT_FILE',
            lastRevisedTime: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: 'body, #canvas-container * {\n    font-family: San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";\n}\n\n@font-face {\n    font-family: \'ITC Garamond \';\n    font-style: normal;\n    font-weight: 400;\n    font-display: swap;\n    src: local(ITC Garamond ) format(\'ttf\');\n  }\n\n\n.appheadercontainer, .listcardcontainer, .gridcardcontainer {\n    container-type: inline-size;\n}\n\n@container (min-width: 700px) {\n    .apptitle {\n        font-size: 3.5em;\n    }\n\n    .listcard {\n        height: 180px\n    }   \n    .gridcard {\n        height: 325px\n    }   \n}\n\n@container (max-width: 700px) {\n    .gridcard {\n        height: 215px\n    }   \n}',
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
          },
        },
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
  return beachesDefaultProject()
}
