import {
  getPrintedUiJsCode,
  renderTestEditorWithCode,
  TestScenePath,
  makeTestProjectCodeWithSnippetDynamicScene,
  makeTestProjectCodeWithSnippet,
} from './ui-jsx-test-utils' // IMPORTANT - THIS IMPORT MUST ALWAYS COME FIRST
import { fireEvent } from '@testing-library/react'
import { act } from 'react-dom/test-utils'
import { selectComponents } from '../editor/actions/actions'
import * as Prettier from 'prettier'
import * as TP from '../../core/shared/template-path'

import { PrettierConfig } from '../../core/workers/parser-printer/prettier-utils'

describe('moving a scene/rootview on the canvas', () => {
  it('dragging a dynamic scene’s root view sets the scene position', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippetDynamicScene(`
        <View style={{ width: 375, height: 812 }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId(
      'component-area-control-utopia-storyboard-uid/scene-aaa:aaa-0',
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
            style={{ width: 375, height: 812 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'aaa'}
          >
            <View
              style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid={'bbb'}
            />
          </View>
        )
      }
      export var storyboard = (props) => {
        return (
          <Storyboard data-uid={'utopia-storyboard-uid'}>
            <Scene
              style={{ position: 'absolute', top: -30, left: 40 }}
              component={App}
              data-uid={'scene-aaa'}
            />
          </Storyboard>
        )
      }`

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      Prettier.format(expectedCode, PrettierConfig),
    )
  })
  it('dragging a static scene’s root view sets the root view position', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ width: '100%', height: '100%' }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId(
      'component-area-control-utopia-storyboard-uid/scene-aaa:aaa-0',
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

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
      <View
          style={{ width: '100%', height: '100%', left: 40, top: -30 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'aaa'}
        >
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>
      `),
    )
  })
})
