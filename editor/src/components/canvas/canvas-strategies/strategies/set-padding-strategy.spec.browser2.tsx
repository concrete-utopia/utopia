import { cmdModifier } from '../../../../utils/modifiers'
import { EdgePiece } from '../../canvas-types'
import { CanvasControlsContainerID } from '../../controls/new-canvas-controls'
import { mouseClickAtPoint } from '../../event-helpers.test-utils'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../../ui-jsx.test-utils'

describe('Padding resize strategy', () => {
  it('Padding resize is present', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
      data-testid={'mydiv'}
      style={{
        backgroundColor: '#0091FFAA',
        position: 'absolute',
        left: 28,
        top: 28,
        width: 612,
        height: 461,
        padding: 50,
        paddingTop: 86,
        paddingLeft: 72,
        paddingRight: 81,
        paddingBottom: 81,
      }}
      data-uid='24a'
    >
      <div
        style={{
          backgroundColor: '#0091FFAA',
          width: '100%',
          height: '100%',
        }}
        data-uid='002'
      />
    </div>`),
      'await-first-dom-report',
    )

    const canvasControlsLayer = editor.renderedDOM.getByTestId(CanvasControlsContainerID)
    const div = editor.renderedDOM.getByTestId('mydiv')
    const divBounds = div.getBoundingClientRect()
    const divCorner = {
      x: divBounds.x + 5,
      y: divBounds.y + 4,
    }

    mouseClickAtPoint(canvasControlsLayer, divCorner, { modifiers: cmdModifier })

    const edgePieces: Array<EdgePiece> = ['top', 'bottom', 'left', 'right']
    edgePieces.forEach((edge) => {
      const paddingControlLeft = editor.renderedDOM.getByTestId(`absolute-resizepadding-${edge}`)
      expect(paddingControlLeft).toBeTruthy()
    })
  })
})
