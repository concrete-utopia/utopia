// Extracted by querying https://SERVER/v1/project/PROJECT_ID/contents.json/projectContents
// Once prettier formats the JSON to a JavaScript value then enums are replaced.

import type { ProjectContentTreeRoot } from '../components/assets'
import { RevisionsState } from '../core/shared/project-file-types'

export const SmallSingleDivProjectContents: ProjectContentTreeRoot = {
  'package.json': {
    content: {
      fileContents: {
        code: '{\n  "name": "Utopia Project",\n  "version": "0.1.0",\n  "utopia": {\n    "main-ui": "utopia/storyboard.js",\n    "html": "public/index.html",\n    "js": "src/index.js"\n  },\n  "dependencies": {\n    "react": "16.13.1",\n    "react-dom": "16.13.1",\n    "utopia-api": "0.4.1"\n  }\n}',
        revisionsState: RevisionsState.CodeAhead,
        parsed: {
          type: 'UNPARSED',
        },
      },
      lastSavedContents: null,
      versionNumber: 0,
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
            code: "import * as React from 'react'\nimport Utopia, {\n  Scene,\n  Storyboard,\n} from 'utopia-api'\nimport { App } from '/src/app.js'\n\nexport var storyboard = (\n  <Storyboard data-uid='storyboard-entity'>\n    <Scene\n      data-label='Imported App'\n      data-uid='scene-1-entity'\n      style={{\n        position: 'absolute',\n        left: 0,\n        top: 0,\n        width: 375,\n        height: 812,\n      }}\n    >\n      <App data-uid='app-entity' />\n    </Scene>\n  </Storyboard>\n)\n",
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: null,
          versionNumber: 0,
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
      'card.js': {
        content: {
          fileContents: {
            code: "import * as React from 'react'\n\nexport var Card = () => {\n  return (\n    <div\n      data-uid='inner-div'\n      style={{\n        position: 'absolute',\n        left: 50,\n        top: 50,\n        width: 40,\n        height: 40,\n        backgroundColor: 'red',\n      }}\n    />\n  )\n}\n",
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: {
            code: "import * as React from 'react'\n\nexport var Card = () => {\n  return (\n    <div\n      data-uid='inner-div'\n      style={{\n        position: 'absolute',\n        left: 50,\n        top: 50,\n        width: 40,\n        height: 40,\n        backgroundColor: 'red',\n      }}\n    />\n  )\n}\n",
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          versionNumber: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/card.js',
      },
      'app.js': {
        content: {
          fileContents: {
            code: "import * as React from 'react'\nimport { Card } from '/src/card.js'\n\nexport var App = (props) => {\n  return (\n    <div\n      data-uid='app-outer-div'\n      style={{\n        position: 'relative',\n        width: '100%',\n        height: '100%',\n        backgroundColor: '#FFFFFF',\n      }}\n    >\n      <Card />\n    </div>\n  )\n}\n",
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: {
            code: "import * as React from 'react'\nimport { Card } from '/src/card.js'\n\nexport var App = (props) => {\n  return (\n    <div\n      data-uid='app-outer-div'\n      style={{\n        position: 'relative',\n        width: '100%',\n        height: '100%',\n        backgroundColor: '#FFFFFF',\n      }}\n    >\n      <Card />\n    </div>\n  )\n}\n",
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          versionNumber: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/app.js',
      },
      'index.js': {
        content: {
          fileContents: {
            code: "import * as React from 'react'\nimport * as ReactDOM from 'react-dom'\nimport { App } from '../src/app'\n\nconst root = document.getElementById('root')\nif (root != null) {\n  ReactDOM.render(<App />, root)\n}\n",
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: null,
          versionNumber: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/index.js',
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
            revisionsState: RevisionsState.CodeAhead,
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: null,
          versionNumber: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/public/index.html',
      },
    },
    type: 'PROJECT_CONTENT_DIRECTORY',
    directory: {
      type: 'DIRECTORY',
    },
    fullPath: '/public',
  },
}
