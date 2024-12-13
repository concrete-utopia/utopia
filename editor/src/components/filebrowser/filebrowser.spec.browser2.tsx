import { wait } from '../../utils/utils.test-utils'
import type { ProjectContentTreeRoot } from '../assets'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import { renderTestEditorWithProjectContent } from '../canvas/ui-jsx.test-utils'
import { setLeftMenuTab, setPanelVisibility } from '../editor/actions/action-creators'
import { LeftMenuTab } from '../editor/store/editor-state'

const contents = {
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
            code: "import * as React from 'react'\nimport { Scene, Storyboard } from 'utopia-api'\nimport { App } from '/src/app.js'\nimport { View, Rectangle } from 'utopia-api'\nimport { FlexRow } from 'utopia-api'\n\nexport var storyboard = (\n  <Storyboard data-uid='0cd'>\n    <Scene\n      style={{\n        width: 700,\n        height: 759,\n        position: 'absolute',\n        left: 207,\n        top: 126,\n        paddingLeft: 91,\n      }}\n      data-testid='scene'\n      data-label='Playground'\n      data-uid='3fc'\n    >\n      \n    </Scene>\n  </Storyboard>\n)\n",
            revisionsState: 'CODE_AHEAD',
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: {
            code: "import * as React from 'react'\nimport { Scene, Storyboard } from 'utopia-api'\nimport { App } from '/src/app.js'\nimport { View, Rectangle } from 'utopia-api'\nimport { FlexRow } from 'utopia-api'\n\nexport var storyboard = (\n  <Storyboard data-uid='0cd'>\n    <Scene\n      style={{\n        width: 700,\n        height: 759,\n        position: 'absolute',\n        left: 207,\n        top: 126,\n        paddingLeft: 91,\n      }}\n      data-testid='scene'\n      data-label='Playground'\n      data-uid='3fc'\n    >\n      <View\n        style={{\n          backgroundColor: '#7D94A7AB',\n          display: 'flex',\n          flexDirection: 'column',\n          width: '100%',\n          height: 404,\n          left: 87,\n          top: 281,\n          paddingLeft: 0,\n          paddingTop: 93,\n        }}\n        data-uid='932'\n      >\n        <View\n          style={{\n            backgroundColor: '#B37DB7AB',\n            width: '100%',\n            height: 104,\n          }}\n          data-uid='bb3'\n        >\n          <div\n            data-uid='a75'\n            data-testid='aaa'\n            style={{\n              backgroundColor: '#EB0A0A',\n              position: 'relative',\n              flexBasis: 50,\n              height: 54,\n            }}\n          />\n        </View>\n        <View\n          style={{ backgroundColor: '#7D94B7AB' }}\n          data-uid='fe1'\n        >\n          <Rectangle\n            style={{\n              backgroundColor: '#aaaaaa33',\n              position: 'relative',\n              height: 54,\n              width: 220,\n            }}\n            data-uid='b8b'\n          />\n        </View>\n        <View\n          style={{\n            backgroundColor: '#90B77DAB',\n            height: 104,\n            left: 87,\n            top: 281,\n          }}\n          data-uid='ed3'\n        >\n          <div\n            style={{\n              backgroundColor: '#787EB7',\n              position: 'relative',\n              padding: 10,\n              flex: '0 1 aut0',\n              border: '0px solid rgb(0, 0, 0, 1)',\n              borderRadius: '50px',\n              fontFamily:\n                'San Francisco, SF UI, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\"',\n              fontStyle: 'normal',\n              fontWeight: 400,\n              display: 'flex',\n              justifyContent: 'center',\n              alignItems: 'center',\n            }}\n            data-uid='a9f'\n          >\n            <span\n              style={{\n                position: 'relative',\n                flexBasis: 49,\n                height: 19,\n                color: 'rgb(255, 255, 255, 1)',\n                textAlign: 'justify',\n                fontWeight: 700,\n                fontStyle: 'normal',\n              }}\n              data-uid='613'\n            >\n              Button\n            </span>\n          </div>\n        </View>\n      </View>\n    </Scene>\n  </Storyboard>\n)\n",
            revisionsState: 'CODE_AHEAD',
            parsed: {
              type: 'UNPARSED',
            },
          },
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
      'app.js': {
        content: {
          fileContents: {
            code: "import '../public/globals.css'\nimport { FlexCol } from './utils'\n\nexport var App = () => {\n  return (\n    <FlexCol\n      style={{\n        width: '100%',\n        height: '100%',\n        background: 'white',\n        justifyContent: 'center',\n        alignItems: 'center',\n      }}\n      data-uid='891'\n    >\n      <img\n        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.png?raw=true'\n        alt='Utopia logo'\n        style={{ height: '40%' }}\n        data-uid='b3c'\n      />\n    </FlexCol>\n  )\n}\n",
            revisionsState: 'CODE_AHEAD',
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
        fullPath: '/src/app.js',
      },
      'utils.js': {
        content: {
          fileContents: {
            code: "import * as React from 'react'\n\nexport function FlexRow({ children, style, ...props }) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'row',\n        ...style,\n      }}\n      data-uid='1d0'\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function FlexCol({ children, style, ...props }) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'flex',\n        flexDirection: 'column',\n        ...style,\n      }}\n      data-uid='d36'\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function TwoColumnGrid({\n  children,\n  style,\n  ...props\n}) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'grid',\n        gridTemplateColumns: 'repeat(2, 1fr)',\n        ...style,\n      }}\n      data-uid='e9a'\n    >\n      {children}\n    </div>\n  )\n}\n\nexport function ThreeColumnGrid({\n  children,\n  style,\n  ...props\n}) {\n  return (\n    <div\n      {...props}\n      style={{\n        position: 'relative',\n        display: 'grid',\n        gridTemplateColumns: '1fr 1fr 1fr',\n        width: '100%',\n        hieght: '100%',\n        ...style,\n      }}\n      data-uid='ba2'\n    >\n      {children}\n    </div>\n  )\n}\n",
            revisionsState: 'CODE_AHEAD',
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
        fullPath: '/src/utils.js',
      },
      'index.js': {
        content: {
          fileContents: {
            code: "import * as React from 'react'\nimport * as ReactDOM from 'react-dom'\nimport { App } from '../src/app'\n\nconst root = document.getElementById('root')\nif (root != null) {\n  ReactDOM.render(<App />, root)\n}\n",
            revisionsState: 'CODE_AHEAD',
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
      'playground.js': {
        content: {
          fileContents: {
            code: "import '../public/globals.css'\nimport { View, Rectangle } from 'utopia-api'\n\nexport var Playground = () => {\n  return (\n    <View\n      style={{\n        backgroundColor: '#aaaaaa33',\n        width: '100%',\n        height: '100%',\n        display: 'flex',\n        flexDirection: 'row',\n        justifyContent: 'flex-start',\n        gap: 22,\n        flexWrap: 'wrap',\n      }}\n      data-uid='414'\n    >\n      <Rectangle\n        style={{\n          backgroundColor: '#D000FFAB',\n          position: 'relative',\n          width: 166,\n          height: 160,\n        }}\n        data-uid='527'\n      />\n      <Rectangle\n        style={{\n          backgroundColor: '#647446',\n          position: 'relative',\n          height: 165,\n          width: 179,\n        }}\n        data-uid='ac9'\n      />\n      <div\n        style={{\n          backgroundColor: '#B33F3F',\n          position: 'relative',\n          width: 129,\n          height: 143,\n        }}\n        data-uid='aaa'\n      />\n    </View>\n  )\n}\n",
            revisionsState: 'CODE_AHEAD',
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
        fullPath: '/src/playground.js',
      },
    },
    type: 'PROJECT_CONTENT_DIRECTORY',
    directory: {
      type: 'DIRECTORY',
    },
    fullPath: '/src',
  },
  assets: {
    children: {
      'stuff.png': {
        content: {
          height: 570,
          imageType: 'image/png',
          type: 'IMAGE_FILE',
          hash: 3674089796,
          width: 1830,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/assets/stuff.png',
      },
    },
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
            code: '<!DOCTYPE html>\n<html lang="en">\n  <head>\n    <meta charset="utf-8">\n    <title>Utopia React App</title>\n    <!-- Begin Generated Utopia External Links -->\n    <link href="https://fonts.googleapis.com/css2?family=Alegreya+SC:ital,wght@0,700" rel="stylesheet">\n    <link href="https://fonts.googleapis.com/css2?family=Cormorant+Garamond:ital,wght@0,700" rel="stylesheet">\n    <!-- End Generated Utopia External Links -->\n  </head>\n  <body>\n    <div id="root"></div>\n  </body>\n</html>',
            revisionsState: 'CODE_AHEAD',
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: {
            code: '<!DOCTYPE html>\n<html lang="en">\n  <head>\n    <meta charset="utf-8">\n    <title>Utopia React App</title>\n    <!-- Begin Generated Utopia External Links -->\n    <link href="https://fonts.googleapis.com/css2?family=Alegreya+SC:ital,wght@0,700" rel="stylesheet">\n    <link href="https://fonts.googleapis.com/css2?family=Cormorant+Garamond:ital,wght@0,700" rel="stylesheet">\n    <!-- End Generated Utopia External Links -->\n  </head>\n  <body>\n    <div id="root"></div>\n  </body>\n</html>',
            revisionsState: 'CODE_AHEAD',
            parsed: {
              type: 'UNPARSED',
            },
          },
          versionNumber: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/public/index.html',
      },
      'globals.css': {
        content: {
          fileContents: {
            code: 'body, #canvas-container * {\n    font-family: San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";\n}\n\n@font-face {\n    font-family: \'ITC Garamond \';\n    font-style: normal;\n    font-weight: 400;\n    font-display: swap;\n    src: local(ITC Garamond ) format(\'ttf\');\n  }\n\n\n.appheadercontainer, .listcardcontainer, .gridcardcontainer {\n    container-type: inline-size;\n}\n\n@container (min-width: 700px) {\n    .apptitle {\n        font-size: 3.5em;\n    }\n\n    .listcard {\n        height: 180px\n    }   \n    .gridcard {\n        height: 325px\n    }   \n}\n\n@container (max-width: 700px) {\n    .gridcard {\n        height: 215px\n    }   \n}',
            revisionsState: 'CODE_AHEAD',
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
        fullPath: '/public/globals.css',
      },
    },
    type: 'PROJECT_CONTENT_DIRECTORY',
    directory: {
      type: 'DIRECTORY',
    },
    fullPath: '/public',
  },
} as ProjectContentTreeRoot

describe('File browser tests', () => {
  it('when clicking on a file item, the correct action is dispatched', async () => {
    const editor = await renderTestEditorWithProjectContent(contents, 'await-first-dom-report')
    await editor.dispatch(
      [setPanelVisibility('leftmenu', true), setLeftMenuTab(LeftMenuTab.Project)],
      true,
    )

    const fileName = 'globals.css'

    await editor.getDispatchFollowUpActionsFinished()
    const fileItem = editor.renderedDOM.getByText(fileName)
    const fileItemBounds = fileItem.getBoundingClientRect()
    const fileItemBoundsCenter = {
      x: fileItemBounds.x + 5,
      y: fileItemBounds.y + 5,
    }

    await mouseClickAtPoint(fileItem, fileItemBoundsCenter)
    await editor.getDispatchFollowUpActionsFinished()

    const actions = editor.getRecordedActions()
    const requiredActionPresent = actions.some(
      (a) => a.action === 'OPEN_CODE_EDITOR_FILE' && a.filename.includes(fileName),
    )

    expect(requiredActionPresent).toEqual(true)
  })
})
