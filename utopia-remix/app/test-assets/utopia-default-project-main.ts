import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'

export const TestProjectUtopiaDefaultProjectMain: ProjectContentTreeRoot = {
  'utopia-default-project-main': {
    type: 'PROJECT_CONTENT_DIRECTORY',
    fullPath: '/utopia-default-project-main',
    directory: { type: 'DIRECTORY' },
    children: {
      public: {
        type: 'PROJECT_CONTENT_DIRECTORY',
        fullPath: '/utopia-default-project-main/public',
        directory: { type: 'DIRECTORY' },
        children: {
          'globals.css': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/utopia-default-project-main/public/globals.css',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: 'body, #canvas-container * {\n    font-family: San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";\n}\n\n@font-face {\n    font-family: \'ITC Garamond \';\n    font-style: normal;\n    font-weight: 400;\n    font-display: swap;\n    src: local(ITC Garamond ) format(\'ttf\');\n  }\n\n\n.appheadercontainer, .listcardcontainer, .gridcardcontainer {\n    container-type: inline-size;\n}\n\n@container (min-width: 700px) {\n    .apptitle {\n        font-size: 3.5em;\n    }\n\n    .listcard {\n        height: 180px\n    }   \n    .gridcard {\n        height: 325px\n    }   \n}\n\n@container (max-width: 700px) {\n    .gridcard {\n        height: 215px\n    }   \n}',
                parsed: { type: 'UNPARSED' },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
          'index.html': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/utopia-default-project-main/public/index.html',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: '<!DOCTYPE html>\n<html lang="en">\n  <head>\n    <meta charset="utf-8">\n    <title>Utopia React App</title>\n    <!-- Begin Generated Utopia External Links -->\n    <link href="https://fonts.googleapis.com/css2?family=Alegreya+SC:ital,wght@0,700" rel="stylesheet">\n    <link href="https://fonts.googleapis.com/css2?family=Cormorant+Garamond:ital,wght@0,700" rel="stylesheet">\n    <!-- End Generated Utopia External Links -->\n  </head>\n  <body>\n    <div id="root"></div>\n  </body>\n</html>',
                parsed: { type: 'UNPARSED' },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
        },
      },
      'package.json': {
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/utopia-default-project-main/package.json',
        content: {
          type: 'TEXT_FILE',
          fileContents: {
            code: '{\n  "name": "Utopia Project",\n  "version": "0.1.0",\n  "utopia": {\n    "main-ui": "utopia/storyboard.js",\n    "html": "public/index.html",\n    "js": "src/index.js"\n  },\n  "dependencies": {\n    "react": "16.13.1",\n    "react-dom": "16.13.1",\n    "utopia-api": "0.4.1",\n    "react-spring": "8.0.27",\n    "@heroicons/react": "1.0.1",\n    "@emotion/react": "11.9.3"\n  }\n}',
            parsed: { type: 'UNPARSED' },
            revisionsState: 'CODE_AHEAD',
          },
          lastParseSuccess: null,
          lastSavedContents: null,
          versionNumber: 0,
        },
      },
      src: {
        type: 'PROJECT_CONTENT_DIRECTORY',
        fullPath: '/utopia-default-project-main/src',
        directory: { type: 'DIRECTORY' },
        children: {
          'app.js': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/utopia-default-project-main/src/app.js',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: "import * as React from 'react'\nimport '../public/globals.css'\nimport { FlexCol } from './utils'\n\nexport var App = () => {\n  return (\n    <FlexCol\n      style={{\n        width: '100%',\n        height: '100%',\n        background: 'white',\n\n        justifyContent: 'center',\n        alignItems: 'center',\n      }}\n    >\n      <img\n        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'\n        alt='Utopia logo'\n        style={{\n          width: 357,\n          height: 453,\n        }}\n      ></img>\n    </FlexCol>\n  )\n}\n",
                parsed: { type: 'UNPARSED' },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
          'index.js': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/utopia-default-project-main/src/index.js',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: "import * as React from 'react'\nimport * as ReactDOM from 'react-dom'\nimport { App } from '../src/app'\n\nconst root = document.getElementById('root')\nif (root != null) {\n  ReactDOM.render(<App />, root)\n}\n",
                parsed: { type: 'UNPARSED' },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
          'playground.js': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/utopia-default-project-main/src/playground.js',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: "import * as React from 'react'\nimport { View } from 'utopia-api'\nimport '../public/globals.css'\n\nexport var Playground = ({ style }) => {\n  return (\n    <div\n      style={{\n        height: '100%',\n        width: '100%',\n        contain: 'layout',\n        ...style,\n      }}\n      data-uid='a7b'\n    >\n      <div\n        style={{\n          height: 'max-content',\n          position: 'absolute',\n          left: 163,\n          top: 305,\n          display: 'flex',\n          flexDirection: 'row',\n          width: 'max-content',\n          gap: 10,\n        }}\n        data-uid='b15'\n      >\n        <img\n          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'\n          alt='Utopia logo'\n          style={{ width: 118, height: 150 }}\n          data-uid='b0e'\n        />\n        <img\n          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'\n          alt='Utopia logo'\n          style={{ width: 118, height: 150 }}\n          data-uid='aaf'\n        />\n        <img\n          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'\n          alt='Utopia logo'\n          style={{ width: 118, height: 150 }}\n          data-uid='aam'\n        />\n      </div>\n    </div>\n  )\n}\n",
                parsed: { type: 'UNPARSED' },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
          'utils.js': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/utopia-default-project-main/src/utils.js',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: "import * as React from 'react'\n\nexport function FlexRow({ children, style, ...props }) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'row',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function FlexCol({ children, style, ...props }) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'column',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function TwoColumnGrid({\n  children,\n  style,\n  ...props\n}) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'grid',\n        gridTemplateColumns: 'repeat(2, 1fr)',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function ThreeColumnGrid({\n  children,\n  style,\n  ...props\n}) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'grid',\n        gridTemplateColumns: '1fr 1fr 1fr',\n        width: '100%',\n        hieght: '100%',\n        ...style,\n      }}\n    >\n      {children}\n    </div>\n  )\n}\n",
                parsed: { type: 'UNPARSED' },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
        },
      },
      utopia: {
        type: 'PROJECT_CONTENT_DIRECTORY',
        fullPath: '/utopia-default-project-main/utopia',
        directory: { type: 'DIRECTORY' },
        children: {
          'storyboard.js': {
            type: 'PROJECT_CONTENT_FILE',
            fullPath: '/utopia-default-project-main/utopia/storyboard.js',
            content: {
              type: 'TEXT_FILE',
              fileContents: {
                code: "import * as React from 'react'\nimport { Scene, Storyboard } from 'utopia-api'\nimport { App } from '/src/app.js'\nimport { Playground } from '/src/playground.js'\n\nexport var storyboard = (\n  <Storyboard>\n    <Scene\n      id='playground-scene'\n      commentId='playground-scene'\n      style={{\n        width: 700,\n        height: 759,\n        position: 'absolute',\n        left: 212,\n        top: 128,\n      }}\n      data-label='Playground'\n    >\n      <Playground style={{}} />\n    </Scene>\n    <Scene\n      id='app-scene'\n      commentId='app-scene'\n      style={{\n        width: 744,\n        height: 1133,\n        position: 'absolute',\n        left: 1036,\n        top: 128,\n      }}\n      data-label='My App'\n    >\n      <App style={{}} />\n    </Scene>\n  </Storyboard>\n)\n",
                parsed: { type: 'UNPARSED' },
                revisionsState: 'CODE_AHEAD',
              },
              lastParseSuccess: null,
              lastSavedContents: null,
              versionNumber: 0,
            },
          },
        },
      },
    },
  },
}
