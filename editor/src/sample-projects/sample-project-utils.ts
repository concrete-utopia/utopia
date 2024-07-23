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
    src: {
      directory: {
        type: 'DIRECTORY',
      },
      type: 'PROJECT_CONTENT_DIRECTORY',
      children: {
        'index.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/index.js',
          content: {
            type: 'TEXT_FILE',
            versionNumber: 0,
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
        'utils.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/utils.js',
          content: {
            type: 'TEXT_FILE',
            versionNumber: 0,
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
        'playground.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/playground.js',
          content: {
            type: 'TEXT_FILE',
            versionNumber: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import * as React from 'react'\nimport { View } from 'utopia-api'\nimport '../public/globals.css'\n\nexport var Playground = ({ style }) => {\n  return (\n    <div\n      style={{\n        height: '100%',\n        width: '100%',\n        contain: 'layout',\n        ...style,\n      }}\n      data-uid='a7b'\n    >\n      <div\n        style={{\n          height: 'max-content',\n          position: 'absolute',\n          left: 163,\n          top: 305,\n          display: 'flex',\n          flexDirection: 'row',\n          width: 'max-content',\n          gap: 10,\n        }}\n        data-uid='b15'\n      >\n        <img\n          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'\n          alt='Utopia logo'\n          style={{ width: 118, height: 150 }}\n          data-uid='b0e'\n        />\n        <img\n          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'\n          alt='Utopia logo'\n          style={{ width: 118, height: 150 }}\n          data-uid='aaf'\n        />\n        <img\n          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'\n          alt='Utopia logo'\n          style={{ width: 118, height: 150 }}\n          data-uid='aam'\n        />\n      </div>\n    </div>\n  )\n}\n",
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
            versionNumber: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import * as React from 'react'\nimport '../public/globals.css'\nimport { FlexCol } from './utils'\n\nexport var App = () => {\n  return (\n    <FlexCol\n      style={{\n        width: '100%',\n        height: '100%',\n        background: 'white',\n\n        justifyContent: 'center',\n        alignItems: 'center',\n      }}\n    >\n      <img\n        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'\n        alt='Utopia logo'\n        style={{\n          width: 357,\n          height: 453,\n        }}\n      ></img>\n    </FlexCol>\n  )\n}\n",
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
            versionNumber: 0,
            lastParseSuccess: null,
            lastSavedContents: null,
            fileContents: {
              code: "import * as React from 'react'\nimport { Scene, Storyboard } from 'utopia-api'\nimport { App } from '/src/app.js'\nimport { Playground } from '/src/playground.js'\n\nexport var storyboard = (\n  <Storyboard>\n    <Scene\n      id='playground-scene'\n      commentId='playground-scene'\n      style={{\n        width: 700,\n        height: 759,\n        position: 'absolute',\n        left: 212,\n        top: 128,\n      }}\n      data-label='Playground'\n    >\n      <Playground style={{}} />\n    </Scene>\n    <Scene\n      id='app-scene'\n      commentId='app-scene'\n      style={{\n        width: 744,\n        height: 1133,\n        position: 'absolute',\n        left: 1036,\n        top: 128,\n      }}\n      data-label='My App'\n    >\n      <App style={{}} />\n    </Scene>\n  </Storyboard>\n)\n",
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
        versionNumber: 0,
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
            versionNumber: 0,
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
            versionNumber: 0,
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
