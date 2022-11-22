import {
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
  makeTestProjectCodeWithSnippet,
} from './ui-jsx.test-utils' // IMPORTANT - THIS IMPORT MUST ALWAYS COME FIRST
import { fireEvent, act } from '@testing-library/react'
import { selectComponents } from '../editor/actions/action-creators'
import * as Prettier from 'prettier/standalone'
import * as EP from '../../core/shared/element-path'

import { PrettierConfig } from 'utopia-vscode-common'
import { BakedInStoryboardUID } from '../../core/model/scene-utils'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import { wait } from '../../utils/utils.test-utils'
import { isFeatureEnabled, setFeatureEnabled } from '../../utils/feature-switches'
import { resetMouseStatus } from '../mouse-move'
import { SceneLabelTestID } from './controls/select-mode/scene-label'

describe('moving a scene/rootview on the canvas', () => {
  // TODO Eni and Balazs look into why is this failing under Karma
  xit('dragging a scene child’s root view sets the root view position', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ width: '100%', height: '100%' }} data-testid='aaa' data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
            data-uid='bbb'
          />
        </View>
      `),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId('aaa')

    const areaControlBounds = areaControl.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    fireEvent(
      canvasControlsLayer,
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
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await dispatchDone
    })

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: false,
          clientX: areaControlBounds.left + 45,
          clientY: areaControlBounds.top - 25,
          buttons: 1,
        }),
      )
      await dispatchDone
    })

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
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
      await dispatchDone
    })
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <View
          style={{ width: '100%', height: '100%', left: 40, top: -30 }}
          data-testid='aaa'
          data-uid='aaa'
        >
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
            data-uid='bbb'
          />
        </View>
      `),
    )
  })

  it('dragging a scene sets the scene position', async () => {
    const testCode = Prettier.format(
      `
        import * as React from 'react'
        import { Scene, Storyboard, View } from 'utopia-api'

        export var App = (props) => {
          return (
            <View
              style={{ position: 'relative', width: '100%', height: '100%' }}
              data-uid='aaa'
            >
              <View
                style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
                data-uid='bbb'
              />
            </View>
          )
        }

        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='${BakedInStoryboardUID}'>
              <Scene
                style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
                data-uid='${TestSceneUID}'
              >
                <App data-uid='${TestAppUID}' />
              </Scene>
            </Storyboard>
          )
        }`,
      PrettierConfig,
    )
    const renderResult = await renderTestEditorWithCode(testCode, 'await-first-dom-report')

    const targetPath = EP.elementPath([[BakedInStoryboardUID, TestSceneUID]])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const areaControl = renderResult.renderedDOM.getByTestId(SceneLabelTestID)

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
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
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
      await dispatchDone
    })

    // Ensures that the two mouse moves are handled without the check against
    // didWeHandleMouseMoveForThisFrame not triggering.
    resetMouseStatus()

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
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
      await dispatchDone
    })

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
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
      await dispatchDone
    })

    const expectedCode = `
    import * as React from 'react'
    import { Scene, Storyboard, View } from 'utopia-api'

    export var App = (props) => {
      return (
        <View
          style={{ position: 'relative', width: '100%', height: '100%' }}
          data-uid='aaa'
        >
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
            data-uid='bbb'
          />
        </View>
      )
    }

    export var storyboard = (props) => {
      return (
        <Storyboard data-uid='${BakedInStoryboardUID}'>
          <Scene
            style={{ position: 'absolute', left: 40, top: -30, width: 400, height: 400 }}
            data-uid='${TestSceneUID}'
          >
            <App data-uid='${TestAppUID}' />
          </Scene>
        </Storyboard>
      )
    }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })
})

describe('resizing a scene/rootview on the canvas', () => {
  it('resizing a scene child’s root view sets the root view size', async () => {
    const testCode = Prettier.format(
      `
        import * as React from 'react'
        import { Scene, Storyboard, View } from 'utopia-api'

        export var App = (props) => {
          return (
            <View
              style={{ position: 'relative', width: '100%', height: '100%' }}
              data-uid='aaa'
            >
              <View
                style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
                data-uid='bbb'
              />
            </View>
          )
        }

        export var storyboard = (props) => {
          return (
            <Storyboard data-uid='${BakedInStoryboardUID}'>
              <Scene
                style={{ position: 'absolute', top: 0, left: 0, width: 200, height: 400 }}
                data-uid='${TestSceneUID}'
              >
                <App data-uid='${TestAppUID}' />
              </Scene>
            </Storyboard>
          )
        }`,
      PrettierConfig,
    )
    const renderResult = await renderTestEditorWithCode(testCode, 'await-first-dom-report')

    const targetPath = EP.appendNewElementPath(TestScenePath, ['aaa'])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const areaControl = renderResult.renderedDOM.getByTestId(
      `component-resize-control-${EP.toString(targetPath)}-0-1-1`,
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
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
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
      await dispatchDone
    })

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
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
      await dispatchDone
    })

    const expectedCode = `
    import * as React from 'react'
    import { Scene, Storyboard, View } from 'utopia-api'

    export var App = (props) => {
      return (
        <View
          style={{ position: 'relative', width: '120%', height: '92.5%' }}
          data-uid='aaa'
        >
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
            data-uid='bbb'
          />
        </View>
      )
    }

    export var storyboard = (props) => {
      return (
        <Storyboard data-uid='${BakedInStoryboardUID}'>
          <Scene
            style={{ position: 'absolute', top: 0, left: 0, width: 200, height: 400 }}
            data-uid='${TestSceneUID}'
          >
            <App data-uid='${TestAppUID}' />
          </Scene>
        </Storyboard>
      )
    }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })

  it('resizing a scene sets the scene size', async () => {
    const testCode = Prettier.format(
      `
      import * as React from 'react'
      import { Scene, Storyboard, View } from 'utopia-api'

      export var App = (props) => {
        return (
          <View
            style={{ position: 'relative', width: '100%', height: '100%' }}
            data-uid='aaa'
          >
            <View
              style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
              data-uid='bbb'
            />
          </View>
        )
      }

      export var storyboard = (props) => {
        return (
          <Storyboard data-uid='${BakedInStoryboardUID}'>
            <Scene
              style={{ position: 'absolute', top: 0, left: 0, width: 200, height: 400 }}
              data-uid='${TestSceneUID}'
            >
              <App data-uid='${TestAppUID}' />
            </Scene>
          </Storyboard>
        )
      }`,
      PrettierConfig,
    )
    const renderResult = await renderTestEditorWithCode(testCode, 'await-first-dom-report')

    const targetPath = EP.elementPath([[BakedInStoryboardUID, TestSceneUID]])
    await renderResult.dispatch([selectComponents([targetPath], false)], false)

    const areaControl = renderResult.renderedDOM.getByTestId(
      `component-resize-control-${EP.toString(targetPath)}-0-1-1`,
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
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
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
      await dispatchDone
    })

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
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
      await dispatchDone
    })

    const expectedCode = `
      import * as React from 'react'
      import { Scene, Storyboard, View } from 'utopia-api'

      export var App = (props) => {
        return (
          <View
            style={{ position: 'relative', width: '100%', height: '100%' }}
            data-uid='aaa'
          >
            <View
              style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
              data-uid='bbb'
            />
          </View>
        )
      }
      
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid='${BakedInStoryboardUID}'>
            <Scene
              style={{ position: 'absolute', top: 0, left: 0, width: 240, height: 370 }}
              data-uid='${TestSceneUID}'
            >
              <App data-uid='${TestAppUID}' />
            </Scene>
          </Storyboard>
        )
      }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })
})
