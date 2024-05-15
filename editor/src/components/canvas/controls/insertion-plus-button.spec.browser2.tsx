/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "checkInsertButtonPosition"] }] */
import { act } from '@testing-library/react'
import { selectComponents } from '../../../components/editor/actions/action-creators'
import { getCanvasRectangleFromElement } from '../../../core/shared/dom-utils'
import * as EP from '../../../core/shared/element-path'
import {
  canvasRectangle,
  negate,
  offsetRect,
  windowRectangle,
} from '../../../core/shared/math-utils'
import { CanvasContainerID } from '../canvas-types'
import type { EditorRenderResult } from '../ui-jsx.test-utils'
import { renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { BlueDotSize, InsertionButtonOffset } from './insertion-plus-button'
import { boundingClientRectToCanvasRectangle } from '../../../utils/utils.test-utils'

function getProjectCode(flexDirection: string): string {
  return `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var Button = () => {
  return (
    <div
      data-uid='buttondiv'
      data-testid='buttondiv'
      data-label='buttondiv'
      style={{
        width: 100,
        height: 30,
        backgroundColor: 'pink',
      }}
    >
      BUTTON
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='scene'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 500,
        }}
        data-uid='sceneroot'
      >
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 241,
            top: 142,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: 111,
            width: 140,
            position: 'absolute',
            left: 210,
            top: 346,
          }}
          data-uid='dragme'
          data-testid='dragme'
          data-label='dragme'
        >
          <Button
            data-uid='button'
            data-testid='button'
            data-label='button'
          />
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'grey',
          position: 'absolute',
          display: 'flex',
          flexDirection: '${flexDirection}',
          gap: '50px',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      >
        <div
          style={{
            backgroundColor: 'teal',
            position: 'relative',
            width: 109,
            height: 123,
          }}
          data-uid='firstdiv'
          data-testid='firstdiv'
          data-label='firstdiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            position: 'relative',
            width: 118,
            height: 123,
          }}
          data-uid='thirddiv'
          data-testid='thirddiv'
          data-label='thirddiv'
        />
      </div>
    </Scene>
  </Storyboard>
)
`
}

function getProjectCodeForEmptyFlexContainer(): string {
  return `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var Button = () => {
  return (
    <div
      data-uid='buttondiv'
      data-testid='buttondiv'
      data-label='buttondiv'
      style={{
        width: 100,
        height: 30,
        backgroundColor: 'pink',
      }}
    >
      BUTTON
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      style={{
        backgroundColor: 'white',
        position: 'absolute',
        left: 0,
        top: 0,
        width: 400,
        height: 700,
      }}
      data-uid='scene'
    >
      <div
        style={{
          backgroundColor: 'white',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 400,
          height: 500,
        }}
        data-uid='sceneroot'
      >
        <div
          style={{
            backgroundColor: 'purple',
            position: 'absolute',
            left: 21,
            top: 215.5,
            width: 123,
            height: 100,
          }}
          data-uid='seconddiv'
          data-testid='seconddiv'
          data-label='seconddiv'
        />
        <div
          style={{
            backgroundColor: 'green',
            height: 65,
            width: 66,
            position: 'absolute',
            left: 241,
            top: 142,
          }}
          data-uid='notdrag'
          data-testid='notdrag'
          data-label='notdrag'
        >
          not drag
        </div>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            height: 111,
            width: 140,
            position: 'absolute',
            left: 210,
            top: 346,
          }}
          data-uid='dragme'
          data-testid='dragme'
          data-label='dragme'
        >
          <Button
            data-uid='button'
            data-testid='button'
            data-label='button'
          />
        </div>
      </div>
      <div
        style={{
          backgroundColor: 'grey',
          position: 'absolute',
          display: 'flex',
          gap: '50px',
          left: 0,
          top: 500,
          width: 400,
          height: 200,
        }}
        data-uid='parentsibling'
        data-testid='parentsibling'
        data-label='parentsibling'
      />
    </Scene>
  </Storyboard>
)
`
}

describe('Insertion Plus Button', () => {
  async function checkInsertButtonPosition(
    renderResult: EditorRenderResult,
    buttonTestId: string,
    expectedLeft: number,
    expectedTop: number,
  ): Promise<void> {
    const element = await renderResult.renderedDOM.findByTestId(buttonTestId)
    const bounds = element.getBoundingClientRect()

    const canvasBounds = boundingClientRectToCanvasRectangle(renderResult, bounds)

    expect(canvasBounds.x + BlueDotSize / 2).toEqual(expectedLeft)
    expect(canvasBounds.y + BlueDotSize / 2).toEqual(expectedTop)
  }

  it(`shows the buttons in the correct places for a child of a flex container with a direction of 'row'`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getProjectCode('row'),
      'await-first-dom-report',
    )

    const targetPath = EP.fromString('storyboard/scene/parentsibling/firstdiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-0',
      0,
      500 - InsertionButtonOffset,
    )
    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-1',
      134,
      500 - InsertionButtonOffset,
    )
    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-2',
      327,
      500 - InsertionButtonOffset,
    )
  })

  it(`shows the buttons in the correct places for a child of a flex container with a direction of 'row-reverse'`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getProjectCode('row-reverse'),
      'await-first-dom-report',
    )

    const targetPath = EP.fromString('storyboard/scene/parentsibling/firstdiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-0',
      400,
      500 - InsertionButtonOffset,
    )
    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-1',
      266,
      500 - InsertionButtonOffset,
    )
    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-2',
      73,
      500 - InsertionButtonOffset,
    )
  })

  it(`shows the buttons in the correct places for a child of a flex container with a direction of 'column'`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getProjectCode('column'),
      'await-first-dom-report',
    )

    const targetPath = EP.fromString('storyboard/scene/parentsibling/firstdiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-0',
      0 - InsertionButtonOffset,
      500,
    )
    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-1',
      0 - InsertionButtonOffset,
      600,
    )
    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-2',
      0 - InsertionButtonOffset,
      700,
    )
  })

  it(`shows the buttons in the correct places for a child of a flex container with a direction of 'column-reverse'`, async () => {
    const renderResult = await renderTestEditorWithCode(
      getProjectCode('column-reverse'),
      'await-first-dom-report',
    )

    const targetPath = EP.fromString('storyboard/scene/parentsibling/firstdiv')
    await act(() => renderResult.dispatch([selectComponents([targetPath], false)], false))
    await renderResult.getDispatchFollowUpActionsFinished()

    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-0',
      0 - InsertionButtonOffset,
      700,
    )
    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-1',
      0 - InsertionButtonOffset,
      600,
    )
    await checkInsertButtonPosition(
      renderResult,
      'blue-dot-control-2',
      0 - InsertionButtonOffset,
      500,
    )
  })
})
