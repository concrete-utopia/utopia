import { fireEvent } from '@testing-library/react'
import { ProjectContentTreeRoot } from '../components/assets'
import { CanvasControlsContainerID } from '../components/canvas/controls/new-canvas-controls'
import {
  makeDragEvent,
  mouseDownAtPoint,
  mouseMoveToPoint,
  mouseUpAtPoint,
} from '../components/canvas/event-helpers.test-utils'
import {
  getPrintedUiJsCode,
  renderTestEditorWithProjectContent,
} from '../components/canvas/ui-jsx.test-utils'
import { setLeftMenuTab, setPanelVisibility } from '../components/editor/actions/action-creators'
import { LeftMenuTab } from '../components/editor/store/editor-state'
import {
  FOR_TESTS_setNextGeneratedUid,
  FOR_TESTS_setNextGeneratedUids,
} from '../core/model/element-template-utils.test-utils'
import { slightlyOffsetPointBecauseVeryWeirdIssue, wait } from '../utils/utils.test-utils'

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
            code: "import * as React from 'react'\nimport { Scene, Storyboard } from 'utopia-api'\nimport { App } from '/src/app.js'\nimport { View, Rectangle } from 'utopia-api'\nimport { FlexRow } from 'utopia-api'\n\nexport var storyboard = (\n  <Storyboard data-uid='0cd'>\n    <Scene\n      style={{\n        width: 700,\n        height: 759,\n        position: 'absolute',\n        left: 207,\n        top: 126,\n        paddingLeft: 91,\n      }}\n      data-testid='scene'\n      data-label='Playground'\n      data-uid='3fc'\n    >\n      \n    </Scene>\n  </Storyboard>\n)\n",
            revisionsState: 'CODE_AHEAD',
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastSavedContents: {
            code: "import * as React from 'react'\nimport { Scene, Storyboard } from 'utopia-api'\nimport { App } from '/src/app.js'\nimport { View, Rectangle } from 'utopia-api'\nimport { FlexRow } from 'utopia-api'\n\nexport var storyboard = (\n  <Storyboard data-uid='0cd'>\n    <Scene\n      style={{\n        width: 700,\n        height: 759,\n        position: 'absolute',\n        left: 207,\n        top: 126,\n        paddingLeft: 91,\n      }}\n      data-testid='scene'\n      data-label='Playground'\n      data-uid='3fc'\n    >\n      <View\n        style={{\n          backgroundColor: '#7D94A7AB',\n          display: 'flex',\n          flexDirection: 'column',\n          width: '100%',\n          height: 404,\n          left: 87,\n          top: 281,\n          paddingLeft: 0,\n          paddingTop: 93,\n        }}\n        data-uid='932'\n      >\n        <View\n          style={{\n            backgroundColor: '#B37DB7AB',\n            width: '100%',\n            height: 104,\n          }}\n          data-uid='bb3'\n        >\n          <div\n            data-uid='a75'\n            data-testid='aaa'\n            style={{\n              backgroundColor: '#EB0A0A',\n              position: 'relative',\n              flexBasis: 50,\n              height: 54,\n            }}\n          />\n        </View>\n        <View\n          style={{ backgroundColor: '#7D94B7AB' }}\n          data-uid='fe1'\n        >\n          <Rectangle\n            style={{\n              backgroundColor: '#0091FFAA',\n              position: 'relative',\n              height: 54,\n              width: 220,\n            }}\n            data-uid='b8b'\n          />\n        </View>\n        <View\n          style={{\n            backgroundColor: '#90B77DAB',\n            height: 104,\n            left: 87,\n            top: 281,\n          }}\n          data-uid='ed3'\n        >\n          <div\n            style={{\n              backgroundColor: '#787EB7',\n              position: 'relative',\n              padding: 10,\n              flex: '0 1 aut0',\n              border: '0px solid rgb(0, 0, 0, 1)',\n              borderRadius: '50px',\n              fontFamily:\n                'San Francisco, SF UI, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif, \"Apple Color Emoji\", \"Segoe UI Emoji\", \"Segoe UI Symbol\"',\n              fontStyle: 'normal',\n              fontWeight: 400,\n              display: 'flex',\n              justifyContent: 'center',\n              alignItems: 'center',\n            }}\n            data-uid='a9f'\n          >\n            <span\n              style={{\n                position: 'relative',\n                flexBasis: 49,\n                height: 19,\n                color: 'rgb(255, 255, 255, 1)',\n                textAlign: 'justify',\n                fontWeight: 700,\n                fontStyle: 'normal',\n              }}\n              data-uid='613'\n            >\n              Button\n            </span>\n          </div>\n        </View>\n      </View>\n    </Scene>\n  </Storyboard>\n)\n",
            revisionsState: 'CODE_AHEAD',
            parsed: {
              type: 'UNPARSED',
            },
          },
          lastRevisedTime: 0,
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
            code: "import '../public/globals.css'\nimport { FlexCol } from './utils'\n\nexport var App = () => {\n  return (\n    <FlexCol\n      style={{\n        width: '100%',\n        height: '100%',\n        background: 'white',\n        justifyContent: 'center',\n        alignItems: 'center',\n      }}\n      data-uid='891'\n    >\n      <img\n        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'\n        alt='Utopia logo'\n        style={{ height: '40%' }}\n        data-uid='b3c'\n      />\n    </FlexCol>\n  )\n}\n",
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
          lastRevisedTime: 0,
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
          lastRevisedTime: 0,
          type: 'TEXT_FILE',
          lastParseSuccess: null,
        },
        type: 'PROJECT_CONTENT_FILE',
        fullPath: '/src/index.js',
      },
      'playground.js': {
        content: {
          fileContents: {
            code: "import '../public/globals.css'\nimport { View, Rectangle } from 'utopia-api'\n\nexport var Playground = () => {\n  return (\n    <View\n      style={{\n        backgroundColor: '#0091FFAA',\n        width: '100%',\n        height: '100%',\n        display: 'flex',\n        flexDirection: 'row',\n        justifyContent: 'flex-start',\n        gap: 22,\n        flexWrap: 'wrap',\n      }}\n      data-uid='414'\n    >\n      <Rectangle\n        style={{\n          backgroundColor: '#D000FFAB',\n          position: 'relative',\n          width: 166,\n          height: 160,\n        }}\n        data-uid='527'\n      />\n      <Rectangle\n        style={{\n          backgroundColor: '#647446',\n          position: 'relative',\n          height: 165,\n          width: 179,\n        }}\n        data-uid='ac9'\n      />\n      <div\n        style={{\n          backgroundColor: '#B33F3F',\n          position: 'relative',\n          width: 129,\n          height: 143,\n        }}\n        data-uid='aaa'\n      />\n    </View>\n  )\n}\n",
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
            code: 'body, #canvas-container * {\n    font-family: San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";\n}\n\n@font-face {\n    font-family: \'ITC Garamond \';\n    font-style: normal;\n    font-weight: 400;\n    font-display: swap;\n    src: local(ITC Garamond ) format(\'ttf\');\n  }\n\n\n.appheadercontainer, .listcardcontainer, .gridcardcontainer {\n    container-type: inline-size;\n}\n\n@container (min-width: 700px) {\n    .apptitle {\n        font-size: 3.5em;\n    }\n\n    .listcard {\n        height: 180px\n    }   \n    .gridcard {\n        height: 325px\n    }   \n}\n\n@container (max-width: 700px) {\n    .gridcard {\n        height: 215px\n    }   \n}',
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
    },
    type: 'PROJECT_CONTENT_DIRECTORY',
    directory: {
      type: 'DIRECTORY',
    },
    fullPath: '/public',
  },
} as ProjectContentTreeRoot

describe('image dnd', () => {
  it('dragging from the sidebar works', async () => {
    const newUID = 'imgimgimg'
    FOR_TESTS_setNextGeneratedUid(newUID)

    const editor = await renderTestEditorWithProjectContent(contents, 'await-first-dom-report')
    await editor.dispatch(
      [setPanelVisibility('leftmenu', true), setLeftMenuTab(LeftMenuTab.Contents)],
      true,
    )
    await editor.getDispatchFollowUpActionsFinished()

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

    const imageDragHandle = editor.renderedDOM.getByTestId('file-image-drag-handle')
    const dragHandleBounds = imageDragHandle.getBoundingClientRect()
    const handleCenter = slightlyOffsetPointBecauseVeryWeirdIssue({
      x: dragHandleBounds.x + dragHandleBounds.width / 2,
      y: dragHandleBounds.y + dragHandleBounds.height / 2,
    })

    const target = editor.renderedDOM.getByTestId('scene')
    const targetBounds = target.getBoundingClientRect()

    const endPoint = {
      x: targetBounds.x + targetBounds.width / 2,
      y: targetBounds.y + targetBounds.height / 2,
    }

    mouseMoveToPoint(imageDragHandle, handleCenter)
    mouseDownAtPoint(imageDragHandle, handleCenter)
    mouseMoveToPoint(canvasControlsLayer, endPoint, { eventOptions: { buttons: 1 } })
    await editor.getDispatchFollowUpActionsFinished()

    expect(
      editor.getEditorState().strategyState.sortedApplicableStrategies?.length,
    ).toBeGreaterThan(0)

    mouseUpAtPoint(canvasControlsLayer, endPoint)
    await editor.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { View, Rectangle } from 'utopia-api'
import { FlexRow } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 207,
        top: 126,
        paddingLeft: 91,
      }}
      data-testid='scene'
      data-label='Playground'
      data-uid='3fc'
    />
    <img
      src='./assets/stuff.png'
      style={{
        position: 'absolute',
        width: 200,
        height: 62,
        top: 475,
        left: 3,
      }}
      data-uid='imgimgimg'
    />
  </Storyboard>
)
`)
  })

  it('dragging from the "finder" works', async () => {
    FOR_TESTS_setNextGeneratedUids(['1', '2', '3'])

    const editor = await renderTestEditorWithProjectContent(contents, 'await-first-dom-report')
    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

    const file = await makeImageFile(imgBase64, 'chucknorris.png')

    const target = editor.renderedDOM.getByTestId('scene')
    const targetBounds = target.getBoundingClientRect()

    const endPoint = {
      x: targetBounds.x + targetBounds.width / 2,
      y: targetBounds.y + targetBounds.height / 2,
    }

    fireEvent(
      canvasControlsLayer,
      makeDragEvent('drag', canvasControlsLayer, { x: 5, y: 5 }, [file]),
    )

    fireEvent(canvasControlsLayer, makeDragEvent('drag', canvasControlsLayer, endPoint, [file]))

    fireEvent(canvasControlsLayer, makeDragEvent('drop', canvasControlsLayer, endPoint, [file]))

    await editor.getDispatchFollowUpActionsFinished()

    await wait(250) // read the image

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { View, Rectangle } from 'utopia-api'
import { FlexRow } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 207,
        top: 126,
        paddingLeft: 91,
      }}
      data-testid='scene'
      data-label='Playground'
      data-uid='3fc'
    />
    <img
      alt=''
      src='./assets/chucknorris.png'
      style={{
        position: 'absolute',
        left: 602.5,
        top: 505.5,
        width: 1,
        height: 1,
      }}
      data-uid='1'
      data-aspect-ratio-locked
    />
  </Storyboard>
)
`)
  })

  it('dragging existing filename from the "finder" autoincrements filename', async () => {
    FOR_TESTS_setNextGeneratedUids(['1', '2', '3'])

    const editor = await renderTestEditorWithProjectContent(contents, 'await-first-dom-report')
    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

    const file = await makeImageFile(imgBase64, 'stuff.png')

    const target = editor.renderedDOM.getByTestId('scene')
    const targetBounds = target.getBoundingClientRect()

    const endPoint = {
      x: targetBounds.x + targetBounds.width / 2,
      y: targetBounds.y + targetBounds.height / 2,
    }

    fireEvent(
      canvasControlsLayer,
      makeDragEvent('drag', canvasControlsLayer, { x: 5, y: 5 }, [file]),
    )

    fireEvent(canvasControlsLayer, makeDragEvent('drag', canvasControlsLayer, endPoint, [file]))

    fireEvent(canvasControlsLayer, makeDragEvent('drop', canvasControlsLayer, endPoint, [file]))

    await editor.getDispatchFollowUpActionsFinished()

    await wait(250) // read the image

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { View, Rectangle } from 'utopia-api'
import { FlexRow } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 207,
        top: 126,
        paddingLeft: 91,
      }}
      data-testid='scene'
      data-label='Playground'
      data-uid='3fc'
    />
    <img
      alt=''
      src='./assets/stuff_2.png'
      style={{
        position: 'absolute',
        left: 602.5,
        top: 505.5,
        width: 1,
        height: 1,
      }}
      data-uid='1'
      data-aspect-ratio-locked
    />
  </Storyboard>
)
`)
  })

  it('dragging multiple images from the "finder" works', async () => {
    FOR_TESTS_setNextGeneratedUids(['1', '2', '3'])

    const editor = await renderTestEditorWithProjectContent(contents, 'await-first-dom-report')
    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

    const files = [
      await makeImageFile(imgBase64, 'chucknorris.png'),
      await makeImageFile(imgBase64, 'budspencer.png'),
      await makeImageFile(imgBase64, 'brucelee.png'),
    ]

    const target = editor.renderedDOM.getByTestId('scene')
    const targetBounds = target.getBoundingClientRect()

    const endPoint = {
      x: targetBounds.x + targetBounds.width / 2,
      y: targetBounds.y + targetBounds.height / 2,
    }

    fireEvent(
      canvasControlsLayer,
      makeDragEvent('drag', canvasControlsLayer, { x: 5, y: 5 }, files),
    )

    fireEvent(canvasControlsLayer, makeDragEvent('drag', canvasControlsLayer, endPoint, files))

    fireEvent(canvasControlsLayer, makeDragEvent('drop', canvasControlsLayer, endPoint, files))

    await editor.getDispatchFollowUpActionsFinished()

    await wait(250) // read the image

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { View, Rectangle } from 'utopia-api'
import { FlexRow } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='0cd'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 207,
        top: 126,
        paddingLeft: 91,
      }}
      data-testid='scene'
      data-label='Playground'
      data-uid='3fc'
    />
    <img
      alt=''
      src='./assets/chucknorris.png'
      style={{
        position: 'absolute',
        left: 602.5,
        top: 505.5,
        width: 1,
        height: 1,
      }}
      data-uid='1'
      data-aspect-ratio-locked
    />
    <img
      alt=''
      src='./assets/budspencer.png'
      style={{
        position: 'absolute',
        left: 602.5,
        top: 505.5,
        width: 1,
        height: 1,
      }}
      data-uid='2'
      data-aspect-ratio-locked
    />
    <img
      alt=''
      src='./assets/brucelee.png'
      style={{
        position: 'absolute',
        left: 602.5,
        top: 505.5,
        width: 1,
        height: 1,
      }}
      data-uid='3'
      data-aspect-ratio-locked
    />
  </Storyboard>
)
`)
  })
})

it('dragging multiple images with the same name from the "finder" works', async () => {
  FOR_TESTS_setNextGeneratedUids(['1', '2', '3'])

  const editor = await renderTestEditorWithProjectContent(contents, 'await-first-dom-report')
  const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)

  const files = [
    await makeImageFile(imgBase64, 'chucknorris.png'),
    await makeImageFile(imgBase64, 'chucknorris.png'),
    await makeImageFile(imgBase64, 'brucelee.png'),
  ]

  const target = editor.renderedDOM.getByTestId('scene')
  const targetBounds = target.getBoundingClientRect()

  const endPoint = {
    x: targetBounds.x + targetBounds.width / 2,
    y: targetBounds.y + targetBounds.height / 2,
  }

  fireEvent(canvasControlsLayer, makeDragEvent('drag', canvasControlsLayer, { x: 5, y: 5 }, files))

  fireEvent(canvasControlsLayer, makeDragEvent('drag', canvasControlsLayer, endPoint, files))

  fireEvent(canvasControlsLayer, makeDragEvent('drop', canvasControlsLayer, endPoint, files))

  await editor.getDispatchFollowUpActionsFinished()

  await wait(250) // read the image

  expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { View, Rectangle } from 'utopia-api'
import { FlexRow } from 'utopia-api'

export var storyboard = (
<Storyboard data-uid='0cd'>
  <Scene
    style={{
      width: 700,
      height: 759,
      position: 'absolute',
      left: 207,
      top: 126,
      paddingLeft: 91,
    }}
    data-testid='scene'
    data-label='Playground'
    data-uid='3fc'
  />
  <img
    alt=''
    src='./assets/chucknorris.png'
    style={{
      position: 'absolute',
      left: 602.5,
      top: 505.5,
      width: 1,
      height: 1,
    }}
    data-uid='1'
    data-aspect-ratio-locked
  />
  <img
    alt=''
    src='./assets/chucknorris_2.png'
    style={{
      position: 'absolute',
      left: 602.5,
      top: 505.5,
      width: 1,
      height: 1,
    }}
    data-uid='2'
    data-aspect-ratio-locked
  />
  <img
    alt=''
    src='./assets/brucelee.png'
    style={{
      position: 'absolute',
      left: 602.5,
      top: 505.5,
      width: 1,
      height: 1,
    }}
    data-uid='3'
    data-aspect-ratio-locked
  />
</Storyboard>
)
`)
})

// minimal PNG image
// https://stackoverflow.com/a/36610159
const imgBase64 = `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVQYV2NgYAAAAAMAAWgmWQ0AAAAASUVORK5CYII=`

// https://stackoverflow.com/a/47497249
const makeImageFile = (base64: string, name: string) =>
  fetch(base64)
    .then((res) => res.blob())
    .then((blob) => new File([blob], name, { type: 'image/png' }))
