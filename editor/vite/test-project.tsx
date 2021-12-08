export const TestProject = {
  hiddenInstances: [],
  projectSettings: {
    minimised: false,
  },
  fileBrowser: {
    minimised: false,
  },
  projectDescription: '',
  projectVersion: 7,
  projectContents: {
    assets: {
      directory: {
        type: 'DIRECTORY',
      },
      children: {},
      type: 'PROJECT_CONTENT_DIRECTORY',
      fullPath: '/assets',
    },
    src: {
      directory: {
        type: 'DIRECTORY',
      },
      children: {
        'index.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/index.js',
          content: {
            type: 'TEXT_FILE',
            lastParseSuccess: null,
            lastSavedContents: null,
            lastRevisedTime: 0,
            fileContents: {
              code:
                'import * as React from "react";\nimport * as ReactDOM from "react-dom";\nimport { App } from "../src/app";\n\nconst root = document.getElementById("root");\nif (root != null) {\n  ReactDOM.render(<App />, root);\n}',
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
          },
        },
        'app.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/src/app.js',
          content: {
            type: 'TEXT_FILE',
            lastParseSuccess: null,
            lastSavedContents: null,
            lastRevisedTime: 0,
            fileContents: {
              code:
                "import * as React from 'react'\nexport var App = (props) => {\n  return (\n    <div\n      style={{\n        width: '100%',\n        height: '100%',\n        backgroundColor: '#FFFFFF',\n        position: 'relative',\n      }}\n    >\n      <div\n        style={{\n          width: 250,\n          height: 400,\n          display: 'flex',\n          border: '1px solid #000',\n        }}\n      >\n        <div\n          style={{\n            backgroundColor: '#E4E41817',\n            width: 100,\n          }}\n        />\n        <div\n          style={{\n            backgroundColor: '#18BBE417',\n            width: 75,\n          }}\n        />\n      </div>\n    </div>\n  )\n}\n",
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
          },
        },
      },
      type: 'PROJECT_CONTENT_DIRECTORY',
      fullPath: '/src',
    },
    utopia: {
      directory: {
        type: 'DIRECTORY',
      },
      children: {
        'storyboard.js': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/utopia/storyboard.js',
          content: {
            type: 'TEXT_FILE',
            lastParseSuccess: null,
            lastSavedContents: null,
            lastRevisedTime: 0,
            fileContents: {
              code:
                "\nimport * as React from 'react'\nimport { Scene, Storyboard } from 'utopia-api'\nimport { App } from '/src/app.js'\nexport var storyboard = (\n  <Storyboard>\n    <Scene\n      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}\n    >\n      <App />\n    </Scene>\n  </Storyboard>\n)\n\n",
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
          },
        },
      },
      type: 'PROJECT_CONTENT_DIRECTORY',
      fullPath: '/utopia',
    },
    'package.json': {
      type: 'PROJECT_CONTENT_FILE',
      fullPath: '/package.json',
      content: {
        type: 'TEXT_FILE',
        lastParseSuccess: null,
        lastSavedContents: null,
        lastRevisedTime: 0,
        fileContents: {
          code:
            '{\n  "name": "Utopia Project",\n  "version": "0.1.0",\n  "utopia": {\n    "main-ui": "utopia/storyboard.js",\n    "html": "public/index.html",\n    "js": "src/index.js"\n  },\n  "dependencies": {\n    "react": "16.13.1",\n    "react-dom": "16.13.1",\n    "utopia-api": "0.4.1"\n  }\n}',
          revisionsState: 'CODE_AHEAD',
          parsed: {
            type: 'UNPARSED',
          },
        },
      },
    },
    public: {
      directory: {
        type: 'DIRECTORY',
      },
      children: {
        'index.html': {
          type: 'PROJECT_CONTENT_FILE',
          fullPath: '/public/index.html',
          content: {
            type: 'TEXT_FILE',
            lastParseSuccess: null,
            lastSavedContents: null,
            lastRevisedTime: 0,
            fileContents: {
              code:
                '<!DOCTYPE html>\n<html lang="en">\n  <head>\n    <meta charset="utf-8">\n    <title>Utopia React App</title>\n    <!-- Begin Generated Utopia External Links -->\n    <!-- End Generated Utopia External Links -->\n  </head>\n  <body>\n    <div id="root"></div>\n  </body>\n</html>',
              revisionsState: 'CODE_AHEAD',
              parsed: {
                type: 'UNPARSED',
              },
            },
          },
        },
      },
      type: 'PROJECT_CONTENT_DIRECTORY',
      fullPath: '/public',
    },
  },
  lastUsedFont: null,
  navigator: {
    minimised: false,
  },
  forkedFromProjectId: null,
  dependencyList: {
    minimised: false,
  },
  appID: null,
  codeEditorErrors: {
    lintErrors: {
      '/src/app.js': [
        {
          message: "'React' is defined but never used. (no-unused-vars)",
          endColumn: 18,
          source: 'eslint',
          severity: 'warning',
          type: 'warning',
          startLine: 1,
          codeSnippet:
            "import * as React from 'react'\nexport var App = (props) => {\n  return (\n    <div",
          errorCode: 'no-unused-vars',
          passTime: 1638976946073,
          endLine: 1,
          fileName: '/src/app.js',
          startColumn: 13,
        },
        {
          message: "'props' is defined but never used. (no-unused-vars)",
          endColumn: 24,
          source: 'eslint',
          severity: 'warning',
          type: 'warning',
          startLine: 2,
          codeSnippet:
            "import * as React from 'react'\nexport var App = (props) => {\n  return (\n    <div\n      style={{",
          errorCode: 'no-unused-vars',
          passTime: 1638976946073,
          endLine: 2,
          fileName: '/src/app.js',
          startColumn: 19,
        },
      ],
    },
    buildErrors: {},
  },
  exportsInfo: [],
}
