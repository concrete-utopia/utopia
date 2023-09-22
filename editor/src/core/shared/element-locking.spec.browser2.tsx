import { renderTestEditorWithCode } from '../../components/canvas/ui-jsx.test-utils'
import * as EP from './element-path'

function testCode(snippet: string) {
  return `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 10,
        top: 10,
      }}
      data-uid='sc'
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
)

export var App = (props) => {
  return (
    <div
      style={{
        height: '100%',
        width: '100%',
        contain: 'layout',
      }}
      data-uid='app-root'
    >
     ${snippet}
    </div>
  )
}`
}

describe('Auto-locking elements', () => {
  it('Root element of component locked when it is not a leaf element', async () => {
    const renderResult = await renderTestEditorWithCode(
      testCode('<div>Hello</div>'),
      'await-first-dom-report',
    )

    expect(renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString)).toEqual(
      ['sb/sc/app:app-root'],
    )
  })

  it('Root element of component is not locked when it is a leaf element', async () => {
    const renderResult = await renderTestEditorWithCode(testCode('Hello'), 'await-first-dom-report')

    expect(renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString)).toEqual(
      [],
    )
  })
})
