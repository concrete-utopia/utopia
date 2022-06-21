import { act } from '@testing-library/react'
import sinon, { SinonFakeTimers } from 'sinon'

import * as EP from '../../../core/shared/element-path'
import { setFeatureEnabled } from '../../../utils/feature-switches'
import { selectComponents } from '../../editor/actions/action-creators'
import { getPrintedUiJsCode, renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { KeyboardInteractionTimeout } from './interaction-state'

describe('Keyboard Absolute Move E2E', () => {
  let clock: SinonFakeTimers
  before(() => {
    viewport.set(2200, 1000)
    setFeatureEnabled('Canvas Strategies', true)
  })
  beforeEach(function () {
    clock = sinon.useFakeTimers()
  })
  afterEach(function () {
    clock.restore()
  })

  it('Pressing right arrow', async () => {
    const initialElementLeft = 40
    const renderResult = await renderTestEditorWithCode(
      TestProjectDeluxeStallion(initialElementLeft),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [selectComponents([EP.fromString('sb/scene/app-instance:aaa/bbb')], false)],
      true,
    )

    act(() => {
      // right arrow key
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
      window.dispatchEvent(
        new KeyboardEvent('keydown', { key: 'ArrowRight', keyCode: 39, shiftKey: true }),
      )
    })

    // tick the clock so useClearKeyboardInteraction is fired
    clock.tick(KeyboardInteractionTimeout)

    await renderResult.getDispatchFollowUpActionsFinished()

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      TestProjectDeluxeStallion(initialElementLeft + 30),
    )
  })
})

const TestProjectDeluxeStallion = (bbbLeft: number) => `import * as React from 'react'
import Utopia, {
  Scene,
  View,
  Storyboard,
  registerModule,
} from 'utopia-api'

export var App = (props) => {
  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#FFFFFF',
        position: 'relative',
      }}
      data-uid='aaa'
    >
      <View
        style={{
          backgroundColor: '#0091FFAA',
          position: 'absolute',
          left: ${bbbLeft},
          top: 100,
          width: 122,
          height: 101,
        }}
        data-uid='bbb'
        data-testid='bbb'
      />
      <View
        style={{
          backgroundColor: '#0091FFAA',
          position: 'absolute',
          left: 40,
          top: 300,
          width: 126,
          height: 96,
        }}
        data-uid='ccc'
      />
    </div>
  )
}

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        position: 'absolute',
        left: 0,
        top: 0,
        width: 375,
        height: 812,
      }}
      data-uid='scene'
    >
      <App data-uid='app-instance' />
    </Scene>
  </Storyboard>
)
`
