import type { ProjectContentTreeRoot } from '../components/assets'
import { contentsToTree } from '../components/assets'
import type { PersistentModel } from '../components/editor/store/editor-state'
import {
  DefaultPackageJson,
  EmptyPackageJson,
  persistentModelForProjectContents,
  StoryboardFilePath,
} from '../components/editor/store/editor-state'
import {
  appJSFile,
  getDefaultUIJsFile,
  getSamplePreviewFile,
  getSamplePreviewHTMLFile,
} from '../core/model/new-project-files'
import type { ProjectContents, TextFile } from '../core/shared/project-file-types'
import { directory } from '../core/shared/project-file-types'
import {
  codeFile,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../core/shared/project-file-types'

export function totallyEmptyDefaultProject(): PersistentModel {
  const projectContents: ProjectContents = {
    '/package.json': textFile(
      textFileContents(
        JSON.stringify(EmptyPackageJson, null, 2),
        unparsed,
        RevisionsState.CodeAhead,
      ),
      null,
      null,
      0,
    ),
  }

  let persistentModel = persistentModelForProjectContents(contentsToTree(projectContents))
  return persistentModel
}

export function simpleDefaultProject({
  storyboardFile = getDefaultUIJsFile(),
  additionalFiles = {},
}: {
  storyboardFile?: TextFile
  additionalFiles?: { [path: string]: TextFile }
} = {}): PersistentModel {
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
    [StoryboardFilePath]: storyboardFile,
    '/assets': directory(),
    '/public': directory(),
    '/src/index.js': getSamplePreviewFile(),
    '/public/index.html': getSamplePreviewHTMLFile(),
    ...additionalFiles,
  }

  let persistentModel = persistentModelForProjectContents(contentsToTree(projectContents))
  return persistentModel
}

export function createComplexDefaultProjectContents(
  dummyComponent: string = 'Spring',
): ProjectContents {
  function createCodeFile(path: string, contents: string): TextFile {
    return textFile(textFileContents(contents, unparsed, RevisionsState.CodeAhead), null, null, 0)
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
import { ${dummyComponent} } from 'non-existant-dummy-library'
export var Card = (props) => {
  return <div data-uid='card-outer-div' style={{...props.style}}>
    <div data-uid='card-inner-div' data-testid='card-inner-div' style={{ position: 'absolute', left: 0, top: 0, width: 50, height: 50, backgroundColor: 'red' }} />
    <${dummyComponent} data-uid='card-inner-spring' data-testid='spring' style={{ position: 'absolute', left: 100, top: 200, width: 50, height: 50, backgroundColor: 'blue' }} />
  </div>
}`,
    ),
  }
}

export function complexDefaultProject(dummyComponent: string = 'Spring'): PersistentModel {
  const projectContents: ProjectContents = createComplexDefaultProjectContents(dummyComponent)
  const persistentModel = persistentModelForProjectContents(contentsToTree(projectContents))
  return persistentModel
}

export function emptyDefaultProject(): PersistentModel {
  return persistentModelForProjectContents(contentsToTree({}))
}

function createBeachesProjectContents(): ProjectContentTreeRoot {
  // to update the default project here, just delete the big object that is returned here, access the new project's contents.json
  // using the link https://utopia.pizza/v1/project/<project-id>/contents.json and paste the contents of projectContents down below

  return {
    assets: {
      children: {},
      directory: {
        type: 'DIRECTORY',
      },
      fullPath: '/assets',
      type: 'PROJECT_CONTENT_DIRECTORY',
    },
    'jsconfig.json': {
      content: {
        fileContents: {
          code: '{\n  "compilerOptions": {\n    "checkJs": true,\n    "jsx": "react-jsx",\n    "target": "ES2022",\n    "module": "ES2022",\n    "moduleResolution": "Bundler",\n    "baseUrl": ".",\n  },\n  "include": ["./**/*.d.ts", "./**/*.js", "./**/*.jsx"]\n}\n',
          parsed: {
            type: 'UNPARSED',
          },
          revisionsState: 'CODE_AHEAD',
        },
        lastParseSuccess: null,
        lastSavedContents: null,
        type: 'TEXT_FILE',
        versionNumber: 0,
      },
      fullPath: '/jsconfig.json',
      type: 'PROJECT_CONTENT_FILE',
    },
    'package.json': {
      content: {
        fileContents: {
          code: '{\n  "name": "utopia-project",\n  "version": "0.1.0",\n  "utopia": {\n    "main-ui": "utopia/storyboard.js",\n    "html": "public/index.html",\n    "js": "src/index.js"\n  },\n  "dependencies": {\n    "react": "16.13.1",\n    "react-dom": "16.13.1",\n    "utopia-api": "0.4.1",\n    "react-spring": "8.0.27",\n    "@heroicons/react": "1.0.1",\n    "@emotion/react": "11.9.3"\n  }\n}',
          parsed: {
            type: 'UNPARSED',
          },
          revisionsState: 'CODE_AHEAD',
        },
        lastParseSuccess: null,
        lastSavedContents: null,
        type: 'TEXT_FILE',
        versionNumber: 0,
      },
      fullPath: '/package.json',
      type: 'PROJECT_CONTENT_FILE',
    },
    public: {
      children: {
        'globals.css': {
          content: {
            fileContents: {
              code: 'body, #canvas-container * {\n    font-family: San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";\n}\n\n@font-face {\n    font-family: \'ITC Garamond \';\n    font-style: normal;\n    font-weight: 400;\n    font-display: swap;\n    src: local(ITC Garamond ) format(\'ttf\');\n  }\n\n\n.appheadercontainer, .listcardcontainer, .gridcardcontainer {\n    container-type: inline-size;\n}\n\n@container (min-width: 700px) {\n    .apptitle {\n        font-size: 3.5em;\n    }\n\n    .listcard {\n        height: 180px\n    }   \n    .gridcard {\n        height: 325px\n    }   \n}\n\n@container (max-width: 700px) {\n    .gridcard {\n        height: 215px\n    }   \n}',
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
            lastParseSuccess: null,
            lastSavedContents: null,
            type: 'TEXT_FILE',
            versionNumber: 0,
          },
          fullPath: '/public/globals.css',
          type: 'PROJECT_CONTENT_FILE',
        },
        'index.html': {
          content: {
            fileContents: {
              code: '\u003C!DOCTYPE html\u003E\n\u003Chtml lang="en"\u003E\n  \u003Chead\u003E\n    \u003Cmeta charset="utf-8"\u003E\n    \u003Ctitle\u003EUtopia React App\u003C/title\u003E\n    \u003C!-- Begin Generated Utopia External Links --\u003E\n    \u003Clink href="https://fonts.googleapis.com/css2?family=Alegreya+SC:ital,wght@0,700" rel="stylesheet"\u003E\n    \u003Clink href="https://fonts.googleapis.com/css2?family=Cormorant+Garamond:ital,wght@0,700" rel="stylesheet"\u003E\n    \u003C!-- End Generated Utopia External Links --\u003E\n  \u003C/head\u003E\n  \u003Cbody\u003E\n    \u003Cdiv id="root"\u003E\u003C/div\u003E\n  \u003C/body\u003E\n\u003C/html\u003E',
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
            lastParseSuccess: null,
            lastSavedContents: null,
            type: 'TEXT_FILE',
            versionNumber: 0,
          },
          fullPath: '/public/index.html',
          type: 'PROJECT_CONTENT_FILE',
        },
      },
      directory: {
        type: 'DIRECTORY',
      },
      fullPath: '/public',
      type: 'PROJECT_CONTENT_DIRECTORY',
    },
    src: {
      children: {
        'app.js': {
          content: {
            fileContents: {
              code: "import * as React from 'react'\nimport '../public/globals.css'\nimport { FlexCol } from './utils'\n\nexport var App = () =\u003E {\n  return (\n    \u003CFlexCol\n      style={{\n        width: '100%',\n        height: '100%',\n        background: 'white',\n\n        justifyContent: 'center',\n        alignItems: 'center',\n      }}\n    \u003E\n      \u003Cimg\n        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'\n        alt='Utopia logo'\n        style={{\n          width: 357,\n          height: 453,\n        }}\n      \u003E\u003C/img\u003E\n    \u003C/FlexCol\u003E\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
            lastParseSuccess: null,
            lastSavedContents: null,
            type: 'TEXT_FILE',
            versionNumber: 0,
          },
          fullPath: '/src/app.js',
          type: 'PROJECT_CONTENT_FILE',
        },
        'index.js': {
          content: {
            fileContents: {
              code: "import * as React from 'react'\nimport * as ReactDOM from 'react-dom'\nimport { App } from '../src/app'\n\nconst root = document.getElementById('root')\nif (root != null) {\n  ReactDOM.render(\u003CApp /\u003E, root)\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
            lastParseSuccess: null,
            lastSavedContents: null,
            type: 'TEXT_FILE',
            versionNumber: 0,
          },
          fullPath: '/src/index.js',
          type: 'PROJECT_CONTENT_FILE',
        },
        'playground.js': {
          content: {
            fileContents: {
              code: "import * as React from 'react'\nimport { View } from 'utopia-api'\nimport '../public/globals.css'\n\nexport var Playground = ({ style }) =\u003E {\n  return (\n    \u003Cdiv\n      style={{\n        height: '100%',\n        width: '100%',\n        contain: 'layout',\n        ...style,\n      }}\n      data-uid='a7b'\n    \u003E\n      \u003Cdiv\n        style={{\n          height: 'max-content',\n          position: 'absolute',\n          left: 163,\n          top: 305,\n          display: 'flex',\n          flexDirection: 'row',\n          width: 'max-content',\n          gap: 10,\n        }}\n        data-uid='b15'\n      \u003E\n        \u003Cimg\n          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'\n          alt='Utopia logo'\n          style={{ width: 118, height: 150 }}\n          data-uid='b0e'\n        /\u003E\n        \u003Cimg\n          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'\n          alt='Utopia logo'\n          style={{ width: 118, height: 150 }}\n          data-uid='aaf'\n        /\u003E\n        \u003Cimg\n          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'\n          alt='Utopia logo'\n          style={{ width: 118, height: 150 }}\n          data-uid='aam'\n        /\u003E\n      \u003C/div\u003E\n    \u003C/div\u003E\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
            lastParseSuccess: null,
            lastSavedContents: null,
            type: 'TEXT_FILE',
            versionNumber: 0,
          },
          fullPath: '/src/playground.js',
          type: 'PROJECT_CONTENT_FILE',
        },
        'utils.js': {
          content: {
            fileContents: {
              code: "import * as React from 'react'\n\nexport function FlexRow({ children, style, ...props }) {\n  return (\n    \u003Cdiv\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'row',\n        ...style,\n      }}\n    \u003E\n      {children}\n    \u003C/div\u003E\n  )\n}\n\nexport function FlexCol({ children, style, ...props }) {\n  return (\n    \u003Cdiv\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'column',\n        ...style,\n      }}\n    \u003E\n      {children}\n    \u003C/div\u003E\n  )\n}\n\nexport function TwoColumnGrid({\n  children,\n  style,\n  ...props\n}) {\n  return (\n    \u003Cdiv\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'grid',\n        gridTemplateColumns: 'repeat(2, 1fr)',\n        ...style,\n      }}\n    \u003E\n      {children}\n    \u003C/div\u003E\n  )\n}\n\nexport function ThreeColumnGrid({\n  children,\n  style,\n  ...props\n}) {\n  return (\n    \u003Cdiv\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'grid',\n        gridTemplateColumns: '1fr 1fr 1fr',\n        width: '100%',\n        hieght: '100%',\n        ...style,\n      }}\n    \u003E\n      {children}\n    \u003C/div\u003E\n  )\n}\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
            lastParseSuccess: null,
            lastSavedContents: null,
            type: 'TEXT_FILE',
            versionNumber: 0,
          },
          fullPath: '/src/utils.js',
          type: 'PROJECT_CONTENT_FILE',
        },
      },
      directory: {
        type: 'DIRECTORY',
      },
      fullPath: '/src',
      type: 'PROJECT_CONTENT_DIRECTORY',
    },
    utopia: {
      children: {
        'storyboard.js': {
          content: {
            fileContents: {
              code: "import * as React from 'react'\nimport { Scene, Storyboard } from 'utopia-api'\nimport { App } from '../src/app.js'\nimport { Playground } from '../src/playground.js'\n\nexport var storyboard = (\n  \u003CStoryboard\u003E\n    \u003CScene\n      id='playground-scene'\n      commentId='playground-scene'\n      style={{\n        width: 700,\n        height: 759,\n        position: 'absolute',\n        left: 212,\n        top: 128,\n      }}\n      data-label='Playground'\n    \u003E\n      \u003CPlayground style={{}} /\u003E\n    \u003C/Scene\u003E\n    \u003CScene\n      id='app-scene'\n      commentId='app-scene'\n      style={{\n        width: 744,\n        height: 1133,\n        position: 'absolute',\n        left: 1036,\n        top: 128,\n      }}\n      data-label='My App'\n    \u003E\n      \u003CApp style={{}} /\u003E\n    \u003C/Scene\u003E\n  \u003C/Storyboard\u003E\n)\n",
              parsed: {
                type: 'UNPARSED',
              },
              revisionsState: 'CODE_AHEAD',
            },
            lastParseSuccess: null,
            lastSavedContents: null,
            type: 'TEXT_FILE',
            versionNumber: 0,
          },
          fullPath: '/utopia/storyboard.js',
          type: 'PROJECT_CONTENT_FILE',
        },
      },
      directory: {
        type: 'DIRECTORY',
      },
      fullPath: '/utopia',
      type: 'PROJECT_CONTENT_DIRECTORY',
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
