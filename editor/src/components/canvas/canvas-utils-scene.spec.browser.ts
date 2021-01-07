import {
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestScenePath,
  makeTestProjectCodeWithSnippet,
  getTestParseSuccess,
} from './ui-jsx.test-utils' // IMPORTANT - THIS IMPORT MUST ALWAYS COME FIRST
import { fireEvent } from '@testing-library/react'
import { act } from 'react-dom/test-utils'
import { selectComponents } from '../editor/actions/action-creators'
import * as Prettier from 'prettier'
import * as TP from '../../core/shared/template-path'

import { PrettierConfig } from '../../core/workers/parser-printer/prettier-utils'
import { createFakeMetadataForParseSuccess, wait } from '../../utils/test-utils'
import { determineElementsToOperateOnForDragging } from './controls/select-mode/move-utils'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'

describe('moving a scene/rootview on the canvas', () => {
  it('dragging a dynamic scene’s root view sets the scene position', async () => {
    const testCode = Prettier.format(
      `
      /** @jsx jsx */
        import * as React from 'react'
        import { Scene, Storyboard, View, jsx } from 'utopia-api'
        export var App = (props) => {
          return (
            <View
              style={{ width: 375, height: 812 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='aaa'
              data-testid='aaa'
            >
              <View
                style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
                layout={{ layoutSystem: 'pinSystem' }}
                data-uid='bbb'
              />
            </View>
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='utopia-storyboard-uid'>
              <Scene
                style={{ position: 'absolute' }}
                component={App}
                data-uid='scene-aaa'
                resizeContent
              />
            </Storyboard>
          )
        }`,
      PrettierConfig,
    )
    const renderResult = await renderTestEditorWithCode(testCode)

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId('aaa')

    const areaControlBounds = areaControl.getBoundingClientRect()

    fireEvent(
      areaControl,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
        }),
      )
      await domFinished
      await dispatchDone
    })

    const expectedCode = `/** @jsx jsx */
      import * as React from 'react'
      import { Scene, Storyboard, View, jsx } from 'utopia-api'
      export var App = (props) => {
        return (
          <View
            style={{ width: 375, height: 812 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='aaa'
            data-testid='aaa'
          >
            <View
              style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='bbb'
            />
          </View>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid='utopia-storyboard-uid'>
            <Scene
              style={{ position: 'absolute', top: -30, left: 40 }}
              component={App}
              data-uid='scene-aaa'
              resizeContent
            />
          </Storyboard>
        )
      }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })
  it('dragging a dynamic scene sets the scene position', async () => {
    const testCode = Prettier.format(
      `
      /** @jsx jsx */
        import * as React from 'react'
        import { Scene, Storyboard, View, jsx } from 'utopia-api'
        export var App = (props) => {
          return (
            <View
              style={{ width: 375, height: 812 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='aaa'
            >
              <View
                style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
                layout={{ layoutSystem: 'pinSystem' }}
                data-uid='bbb'
              />
            </View>
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='utopia-storyboard-uid'>
              <Scene
                style={{ position: 'absolute' }}
                component={App}
                data-uid='scene-aaa'
                resizeContent
              />
            </Storyboard>
          )
        }`,
      PrettierConfig,
    )
    const renderResult = await renderTestEditorWithCode(testCode)

    await renderResult.dispatch([selectComponents([TestScenePath], false)], false)

    const areaControl = renderResult.renderedDOM.getByTestId(
      'label-control-utopia-storyboard-uid/scene-aaa',
    )

    const areaControlBounds = areaControl.getBoundingClientRect()

    fireEvent(
      areaControl,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top + 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top + 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top + 25,
        }),
      )
      await domFinished
      await dispatchDone
    })

    const expectedCode = `/** @jsx jsx */
      import * as React from 'react'
      import { Scene, Storyboard, View, jsx } from 'utopia-api'
      export var App = (props) => {
        return (
          <View
            style={{ width: 375, height: 812 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='aaa'
          >
            <View
              style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='bbb'
            />
          </View>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid='utopia-storyboard-uid'>
            <Scene
              style={{ position: 'absolute', top: 20, left: 40 }}
              component={App}
              data-uid='scene-aaa'
              resizeContent
            />
          </Storyboard>
        )
      }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })

  it('dragging a static scene’s root view sets the root view position', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ width: '100%', height: '100%' }} layout={{ layoutSystem: 'pinSystem' }} data-testid='aaa' data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='bbb'
          />
        </View>
      `),
    )

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId('aaa')

    const areaControlBounds = areaControl.getBoundingClientRect()

    fireEvent(
      areaControl,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
        }),
      )
      await domFinished
      await dispatchDone
    })

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <View
          style={{ width: '100%', height: '100%', left: 40, top: -30 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-testid='aaa'
          data-uid='aaa'
        >
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='bbb'
          />
        </View>
      `),
    )
  })
  it('dragging a static scene sets the scene position', async () => {
    const testCode = Prettier.format(
      `/** @jsx jsx */
        import * as React from 'react'
        import { Scene, Storyboard, View, jsx } from 'utopia-api'
        export var App = (props) => {
          return (
            <View
              style={{ width: '100%', height: '100%' }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='aaa'
            >
              <View
                style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
                layout={{ layoutSystem: 'pinSystem' }}
                data-uid='bbb'
              />
            </View>
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='utopia-storyboard-uid'>
              <Scene
                style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
                component={App}
                data-uid='scene-aaa'
              />
            </Storyboard>
          )
        }`,
      PrettierConfig,
    )
    const renderResult = await renderTestEditorWithCode(testCode)

    await renderResult.dispatch([selectComponents([TestScenePath], false)], false)

    const areaControl = renderResult.renderedDOM.getByTestId(
      'label-control-utopia-storyboard-uid/scene-aaa',
    )

    const areaControlBounds = areaControl.getBoundingClientRect()

    fireEvent(
      areaControl,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
        }),
      )
      await domFinished
      await dispatchDone
    })

    const expectedCode = `/** @jsx jsx */
    import * as React from 'react'
    import { Scene, Storyboard, View, jsx } from 'utopia-api'
    export var App = (props) => {
      return (
        <View
          style={{ width: '100%', height: '100%' }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid='aaa'
        >
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='bbb'
          />
        </View>
      )
    }
    export var storyboard = (props) => {
      return (
        <Storyboard data-uid='utopia-storyboard-uid'>
          <Scene
            style={{ position: 'absolute', width: 400, height: 400, top: -30, left: 40 }}
            component={App}
            data-uid='scene-aaa'
          />
        </Storyboard>
      )
    }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })
})

describe('resizing a scene/rootview on the canvas', () => {
  it('resizing a dynamic scene’s root view sets the root view size', async () => {
    const testCode = Prettier.format(
      `/** @jsx jsx */
        import * as React from 'react'
        import { Scene, Storyboard, View, jsx } from 'utopia-api'
        export var App = (props) => {
          return (
            <View
              style={{ width: 200, height: 400 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='aaa'
              data-testid='aaa'
            >
              <View
                style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
                layout={{ layoutSystem: 'pinSystem' }}
                data-uid='bbb'
              />
            </View>
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='utopia-storyboard-uid'>
              <Scene
                style={{ position: 'absolute' }}
                component={App}
                data-uid='scene-aaa'
                resizeContent
              />
            </Storyboard>
          )
        }`,
      PrettierConfig,
    )
    const renderResult = await renderTestEditorWithCode(testCode)

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId(
      'component-resize-control-utopia-storyboard-uid/scene-aaa:aaa-0-1-1',
    )

    const areaControlBounds = areaControl.getBoundingClientRect()

    fireEvent(
      areaControl,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
        }),
      )
      await domFinished
      await dispatchDone
    })

    const expectedCode = `/** @jsx jsx */
      import * as React from 'react'
      import { Scene, Storyboard, View, jsx } from 'utopia-api'
      export var App = (props) => {
        return (
          <View
            style={{ height: 370, width: 240 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='aaa'
            data-testid='aaa'
          >
            <View
              style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='bbb'
            />
          </View>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid='utopia-storyboard-uid'>
            <Scene
              style={{ position: 'absolute' }}
              component={App}
              data-uid='scene-aaa'
              resizeContent
            />
          </Storyboard>
        )
      }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })
  it('resizing a dynamic scene sets the root view size', async () => {
    const testCode = Prettier.format(
      `/** @jsx jsx */
        import * as React from 'react'
        import { Scene, Storyboard, View, jsx } from 'utopia-api'
        export var App = (props) => {
          return (
            <View
              style={{ width: 200, height: 400 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='aaa'
            >
              <View
                style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
                layout={{ layoutSystem: 'pinSystem' }}
                data-uid='bbb'
              />
            </View>
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='utopia-storyboard-uid'>
              <Scene
                style={{ position: 'absolute' }}
                component={App}
                data-uid='scene-aaa'
                resizeContent
              />
            </Storyboard>
          )
        }`,
      PrettierConfig,
    )
    const renderResult = await renderTestEditorWithCode(testCode)

    await renderResult.dispatch([selectComponents([TestScenePath], false)], false)

    const areaControl = renderResult.renderedDOM.getByTestId(
      'component-resize-control-utopia-storyboard-uid/scene-aaa-0-1-1',
    )

    const areaControlBounds = areaControl.getBoundingClientRect()

    fireEvent(
      areaControl,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
        }),
      )
      await domFinished
      await dispatchDone
    })

    const expectedCode = `/** @jsx jsx */
      import * as React from 'react'
      import { Scene, Storyboard, View, jsx } from 'utopia-api'
      export var App = (props) => {
        return (
          <View
            style={{ height: 370, width: 240 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='aaa'
          >
            <View
              style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='bbb'
            />
          </View>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid='utopia-storyboard-uid'>
            <Scene
              style={{ position: 'absolute' }}
              component={App}
              data-uid='scene-aaa'
              resizeContent
            />
          </Storyboard>
        )
      }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })
  it('resizing a (group-like) scene with resizeContent enabled that also has width and height ignores the width and height: resizing sets the root view size', async () => {
    const testCode = Prettier.format(
      `/** @jsx jsx */
        import * as React from 'react'
        import { Scene, Storyboard, View, jsx } from 'utopia-api'
        export var App = (props) => {
          return (
            <View
              style={{ width: 200, height: 400 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='aaa'
            >
              <View
                style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
                layout={{ layoutSystem: 'pinSystem' }}
                data-uid='bbb'
              />
            </View>
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='utopia-storyboard-uid'>
              <Scene
                style={{ position: 'absolute', width: 200, height: 200 }}
                component={App}
                data-uid='scene-aaa'
                resizeContent
              />
            </Storyboard>
          )
        }`,
      PrettierConfig,
    )
    const renderResult = await renderTestEditorWithCode(testCode)

    await renderResult.dispatch([selectComponents([TestScenePath], false)], false)

    const areaControl = renderResult.renderedDOM.getByTestId(
      'component-resize-control-utopia-storyboard-uid/scene-aaa-0-1-1',
    )

    const areaControlBounds = areaControl.getBoundingClientRect()

    fireEvent(
      areaControl,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
        }),
      )
      await domFinished
      await dispatchDone
    })

    const expectedCode = `/** @jsx jsx */
      import * as React from 'react'
      import { Scene, Storyboard, View, jsx } from 'utopia-api'
      export var App = (props) => {
        return (
          <View
            style={{ height: 170, width: 240 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='aaa'
          >
            <View
              style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='bbb'
            />
          </View>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid='utopia-storyboard-uid'>
            <Scene
              style={{ position: 'absolute', width: 200, height: 200 }}
              component={App}
              data-uid='scene-aaa'
              resizeContent
            />
          </Storyboard>
        )
      }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })
  it('resizing a static scene’s root view sets the root view size', async () => {
    const testCode = Prettier.format(
      `/** @jsx jsx */
        import * as React from 'react'
        import { Scene, Storyboard, View, jsx } from 'utopia-api'
        export var App = (props) => {
          return (
            <View
              style={{ width: '100%', height: '100%' }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='aaa'
            >
              <View
                style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
                layout={{ layoutSystem: 'pinSystem' }}
                data-uid='bbb'
              />
            </View>
          )
        }
        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='utopia-storyboard-uid'>
              <Scene
                style={{ position: 'absolute', top: 0, left: 0, width: 200, height: 400 }}
                component={App}
                data-uid='scene-aaa'
              />
            </Storyboard>
          )
        }`,
      PrettierConfig,
    )
    const renderResult = await renderTestEditorWithCode(testCode)

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId(
      'component-resize-control-utopia-storyboard-uid/scene-aaa:aaa-0-1-1',
    )

    const areaControlBounds = areaControl.getBoundingClientRect()

    fireEvent(
      areaControl,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
        }),
      )
      await domFinished
      await dispatchDone
    })

    const expectedCode = `/** @jsx jsx */
    import * as React from 'react'
    import { Scene, Storyboard, View, jsx } from 'utopia-api'
    export var App = (props) => {
      return (
        <View
          style={{ height: '92.5%', width: '120%' }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid='aaa'
        >
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='bbb'
          />
        </View>
      )
    }
    export var storyboard = (props) => {
      return (
        <Storyboard data-uid='utopia-storyboard-uid'>
          <Scene
            style={{ position: 'absolute', top: 0, left: 0, width: 200, height: 400 }}
            component={App}
            data-uid='scene-aaa'
          />
        </Storyboard>
      )
    }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })
  it('resizing a static scene sets the scene size', async () => {
    const testCode = Prettier.format(
      `/** @jsx jsx */
      import * as React from 'react'
      import { Scene, Storyboard, View, jsx } from 'utopia-api'
      export var App = (props) => {
        return (
          <View
            style={{ width: '100%', height: '100%' }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='aaa'
          >
            <View
              style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='bbb'
            />
          </View>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid='utopia-storyboard-uid'>
            <Scene
              style={{ position: 'absolute', top: 0, left: 0, width: 200, height: 400 }}
              component={App}
              data-uid='scene-aaa'
            />
          </Storyboard>
        )
      }`,
      PrettierConfig,
    )
    const renderResult = await renderTestEditorWithCode(testCode)

    await renderResult.dispatch([selectComponents([TestScenePath], false)], false)

    const areaControl = renderResult.renderedDOM.getByTestId(
      'component-resize-control-utopia-storyboard-uid/scene-aaa-0-1-1',
    )

    const areaControlBounds = areaControl.getBoundingClientRect()

    fireEvent(
      areaControl,
      new MouseEvent('mousedown', {
        bubbles: true,
        cancelable: true,
        metaKey: true,
        clientX: areaControlBounds.left + 5,
        clientY: areaControlBounds.top + 5,
        buttons: 1,
      }),
    )

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        areaControl,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        window,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
        }),
      )
      await domFinished
      await dispatchDone
    })

    const expectedCode = `/** @jsx jsx */
      import * as React from 'react'
      import { Scene, Storyboard, View, jsx } from 'utopia-api'
      export var App = (props) => {
        return (
          <View
            style={{ width: '100%', height: '100%' }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid='aaa'
          >
            <View
              style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid='bbb'
            />
          </View>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid='utopia-storyboard-uid'>
            <Scene
              style={{ position: 'absolute', top: 0, left: 0, width: 240, height: 370 }}
              component={App}
              data-uid='scene-aaa'
            />
          </Storyboard>
        )
      }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })
})
