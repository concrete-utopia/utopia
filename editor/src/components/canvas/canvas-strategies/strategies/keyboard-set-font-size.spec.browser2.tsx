import { fromString } from '../../../../core/shared/element-path'
import { wait } from '../../../../utils/utils.test-utils'
import { selectComponents } from '../../../editor/actions/action-creators'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { keyDown, mouseClickAtPoint, pressKey } from '../../event-helpers.test-utils'
import { getPrintedUiJsCode, renderTestEditorWithCode } from '../../ui-jsx.test-utils'

describe('adjust font size with the keyboard', () => {
  it('increase font size, no font size specified', async () => {
    const editor = await renderTestEditorWithCode(project, 'await-first-dom-report')
    // const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    // const div = editor.renderedDOM.getByTestId('div')
    // const divBounds = div.getBoundingClientRect()
    // const divCorner = {
    //   x: divBounds.x + 50,
    //   y: divBounds.y + 40,
    // }

    // mouseClickAtPoint(canvasControlsLayer, divCorner)
    await editor.dispatch([selectComponents([fromString('sb/39e')], false)], true)
    await editor.getDispatchFollowUpActionsFinished()

    pressKey('.', { modifiers: { shift: true, cmd: true, alt: false, ctrl: false } })
    await editor.getDispatchFollowUpActionsFinished()

    await wait(10000)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(``)
  })
})

const project = `import * as React from 'react'
import { Storyboard } from 'utopia-api'


export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-testid='div'
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 377,
        top: 271,
        width: 288,
        height: 362,
      }}
      data-uid='39e'
    >
      hello
    </div>
  </Storyboard>
)
`
