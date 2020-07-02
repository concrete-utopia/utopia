import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from './ui-jsx-test-utils' // IMPORTANT - THIS IMPORT MUST ALWAYS COME FIRST
import { fireEvent } from '@testing-library/react'
import { act } from 'react-dom/test-utils'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { canvasRectangle, CanvasVector } from '../../core/shared/math-utils'
import { selectComponents, setCanvasFrames, wrapInView } from '../editor/actions/actions'
import { reparentComponents } from '../navigator/actions'
import * as TP from '../../core/shared/template-path'
import { pinFrameChange, pinMoveChange, pinSizeChange } from './canvas-types'

const NewUID = 'catdog'

describe('updateFramesOfScenesAndComponents - pinFrameChange -', () => {
  it('a simple TLWH pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    const pinChange = pinFrameChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', width: 100, height: 100, left: 20, top: 20 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('a simple TLWH pin change works with old CanvasMetadata format as well', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    const pinChange = pinFrameChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', width: 100, height: 100, left: 20, top: 20 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLWH, but W and H are percentage works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: '50%', height: '20%'  }}
          layout={{ layoutSystem: 'pinSystem'}}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    const pinChange = pinFrameChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', width: '25%', height: '25%', left: 20, top: 20 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLW, missing H pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    const pinChange = pinFrameChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 20, top: 20, width: 100, height: 100 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLWHBR, too many frame points work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202, bottom: 137, right: 93 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    const pinChange = pinFrameChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 20, top: 20, width: 100, height: 100 }}
          ${/** notice how the extraneous pins were removed automatically */ ''}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLRB pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, right: 50, bottom: 20 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )

    const pinChange = pinFrameChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 20, top: 20, right: 280, bottom: 280 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLCxCy pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61 }}
          layout={{ layoutSystem: 'pinSystem', centerX: 100, centerY: 100 }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )

    const pinChange = pinFrameChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 20, top: 20 }}
          layout={{ layoutSystem: 'pinSystem', centerX: -130, centerY: -130 }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('no layout prop on child', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA' }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    const pinChange = pinFrameChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          ${/** pins are magically created */ ''}
          style={{ backgroundColor: '#0091FFAA', left: 20, top: 20, width: 100, height: 100 }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })
})

describe('updateFramesOfScenesAndComponents - pinMoveChange -', () => {
  it('only TL pins work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 52, top: 61 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 20, top: 20 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )
  })

  it('only TL pins work with old CanvasMetadata as well', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 52, top: 61 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 20, top: 20 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )
  })

  it('only RB pins work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', right: 50, bottom: 50 }}
            layout={{ layoutSystem: 'pinSystem'  }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: 20,
      y: 20,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', right: 30, bottom: 30 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )
  })

  it('just R pin gets turned into T,R', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', right: 50 }}
            layout={{ layoutSystem: 'pinSystem'  }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: 20,
      y: 20,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', right: 30, top: 20 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )
  })

  it('just B pin gets turned into L,B', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', bottom: 50 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: 20,
      y: 20,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', bottom: 30, left: 20 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )
  })

  it('just B pin doesn`t turn into L,B with deltaX=0', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', bottom: 50 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: 0,
      y: 20,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', bottom: 30 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )
  })

  it('TLWH, but W and H are left alone', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', width: '50%', height: 25, left: 52, top: 61 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', width: '50%', height: 25, left: 20, top: 20  }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLWH, but W and H are left alone, T, L are % values', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', width: '50%', height: 25, left: '10%', top: '5%' }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: 45,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', width: '50%', height: 25, left: '2%', top: '16.3%' }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLRB pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, right: 50, bottom: 20 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 20, top: 20, right: 82, bottom: 61 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLRB pin change works, with % values', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: '10%', top: '15%', right: '10%', bottom: '25%' }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: 20,
      y: 65,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{
            backgroundColor: '#0091FFAA',
            left: '15%',
            right: '5%',
            top: '31.3%',
            bottom: '8.8%',
          }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLCxCy pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61 }}
          layout={{ layoutSystem: 'pinSystem', centerX: 100, centerY: 100 }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 20, top: 20 }}
          layout={{ layoutSystem: 'pinSystem', centerX: 68, centerY: 59 }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('RBCxCy pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', right: 52, bottom: 61 }}
          layout={{ layoutSystem: 'pinSystem', centerX: 100, centerY: 100 }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', right: 84, bottom: 102 }}
          layout={{ layoutSystem: 'pinSystem', centerX: 68, centerY: 59 }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })

  it('TLWHBR, too many frame points work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202, bottom: 137, right: 93 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            ${
              /**
               * we correctly update left, top, bottom, and right, but we do nothing to fix the fact that there are 6 pins competing for priorities here
               * if we later revise the behavior and change how PIN_MOVE works when there is too many pins, don't be surprised if this test here breaks
               *  */ ''
            }
            style={{
              backgroundColor: '#0091FFAA',
              width: 256,
              height: 202,
              left: 20,
              top: 20,
              right: 125,
              bottom: 178,
            }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
      </View>`,
      ),
    )
  })

  it('TLR, no B pin change?', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, right: 50 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )

    const pinChange = pinMoveChange(TP.instancePath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 20, top: 20, right: 82 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>`,
      ),
    )
  })
})

describe('updateFramesOfScenesAndComponents - pinSizeChange -', () => {
  it('only TL pins work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 52, top: 61 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )

    const pinChange = pinSizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
      null,
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 20, width: 100, top: 20, height: 100 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )
  })

  it('only TW pins work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 52, width: 100 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )

    const pinChange = pinSizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
      null,
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    // THIS IS THE IMPORTANT TEST, IT POINTS OUT THAT WE DO NOT ADD AN UNNECESSARY TOP PROPERTY, ONLY HEIGHT

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', width: 100, left: 20, height: 100 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )
  })

  it('TLRB pins work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, right: 150, bottom: 150 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )

    const pinChange = pinSizeChange(
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
      null,
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 20, right: 280, top: 20, bottom: 280 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>`,
      ),
    )
  })
})

describe('moveTemplate', () => {
  it('wraps in 1 element', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
      </View>
      `),
    )

    const targets = [TP.instancePath(TestScenePath, ['aaa', 'bbb'])]
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)

    await renderResult.dispatch([wrapInView(targets)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
        <View
          layout={{ layoutSystem: 'pinSystem' }}
          style={{ left: 52, top: 61, width: 256, height: 202 }}
          data-uid={'${NewUID}'}
        >
          <View
            style={{ backgroundColor: '#0091FFAA', left: 0, top: 0, width: 256, height: 202 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          />
        </View>
      </View>
      `),
    )
  })
  it('wraps multiselected elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        >
          <View
            style={{ left: 10, top: 10, width: 100, height: 100 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'ccc'}
          >
            <View data-uid={'ddd'} />
          </View>
        </View>
        <View data-uid={'eee'}/>
        <View
          style={{ left: 10, top: 10, width: 256, height: 150 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'fff'}
        >
            <View
              style={{ left: 5, top: 0, width: 246, height: 150 }}
              data-uid={'ggg'}
            />
          </View>
        <View data-uid={'hhh'}/>
      </View>
      `),
    )

    const targets = [
      TP.instancePath(TestScenePath, ['aaa', 'bbb', 'ccc']),
      TP.instancePath(TestScenePath, ['aaa', 'fff', 'ggg']),
    ]
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)

    await renderResult.dispatch([wrapInView(targets)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        />
        <View data-uid={'eee'} />
        <View
          style={{ left: 10, top: 10, width: 256, height: 150 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'fff'}
        />
        <View data-uid={'hhh'} />
        <View
          layout={{ layoutSystem: 'pinSystem' }}
          style={{  left: 15, top: 71, width: 246, height: 291  }}
          data-uid={'${NewUID}'}
        >
          <View
            style={{ left: 47, top: 0, width: 100, height: 100 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'ccc'}
          >
            <View data-uid={'ddd'} />
          </View>
          <View
            style={{ left: 0, top: 141, width: 246, height: 150 }}
            data-uid={'ggg'}
          />
        </View>
      </View>
      `),
    )
  })
  it('wraps in multiselected element + children, moves only the element, keeps children', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
          layout={{ layoutSystem: 'pinSystem' }}
          data-uid={'bbb'}
        >
          <View data-uid={'ccc'}>
            <View data-uid={'ddd'} />
          </View>
        </View>
        <View data-uid={'eee'}/>
      </View>
      `),
    )

    const targets = [
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      TP.instancePath(TestScenePath, ['aaa', 'bbb', 'ccc']),
    ]
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)

    await renderResult.dispatch([wrapInView(targets)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
        <View data-uid={'eee'} />
        <View
          layout={{ layoutSystem: 'pinSystem' }}
          style={{ left: 52, top: 61, width: 256, height: 202 }}
          data-uid={'${NewUID}'}
        >
          <View
            style={{ backgroundColor: '#0091FFAA', left: 0, top: 0, width: 256, height: 202 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          >
            <View data-uid={'ccc'}>
              <View data-uid={'ddd'} />
            </View>
          </View>
        </View>
      </View>
      `),
    )
  })
  it('reparents multiselected elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
            layout={{ layoutSystem: 'pinSystem'}}
            data-uid={'bbb'}
          >
            <View data-uid={'ccc'}>
              <View data-uid={'ddd'} />
            </View>
          </View>
          <View data-uid={'eee'}/>
          <View data-uid={'fff'}>
              <View data-uid={'ggg'} />
            </View>
          <View data-uid={'hhh'}/>
        </View>
      `),
    )

    const targets = [
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      TP.instancePath(TestScenePath, ['aaa', 'hhh']),
      TP.instancePath(TestScenePath, ['aaa', 'fff', 'ggg']),
    ]

    await renderResult.dispatch(
      [reparentComponents(targets, TP.instancePath(TestScenePath, ['aaa', 'eee']))],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View data-uid={'eee'}>
            <View
              style={{ backgroundColor: '#0091FFAA', left: 52, width: 256, height: 202, top: -141 }}
              layout={{ layoutSystem: 'pinSystem'  }}
              data-uid={'bbb'}
            >
              <View data-uid={'ccc'}>
                <View data-uid={'ddd'} />
              </View>
            </View>
            <View data-uid={'ggg'} />
            <View data-uid={'hhh'} style={{ top: 0 }} />
          </View>
          <View data-uid={'fff'} />
        </View>
      `),
    )
  })
  it('reparents multiselected element + children, moves only the element, keeps children', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          >
            <View data-uid={'ccc'}>
              <View data-uid={'ddd'} />
            </View>
          </View>
          <View data-uid={'eee'}/>
        </View>
      `),
    )

    const targets = [
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      TP.instancePath(TestScenePath, ['aaa', 'bbb', 'ccc']),
    ]

    await renderResult.dispatch(
      [reparentComponents(targets, TP.instancePath(TestScenePath, ['aaa', 'eee']))],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View data-uid={'eee'}>
            <View
              style={{ backgroundColor: '#0091FFAA', left: 52, width: 256, height: 202, top: -141 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid={'bbb'}
            >
              <View data-uid={'ccc'}>
                <View data-uid={'ddd'} />
              </View>
            </View>
          </View>
        </View>
      `),
    )
  })
  it('reparents multiselected element + descendant (not direct children), moves both of the elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
            layout={{ layoutSystem: 'pinSystem' }}
            data-uid={'bbb'}
          >
            <View data-uid={'ccc'}>
              <View data-uid={'ddd'} />
            </View>
          </View>
          <View data-uid={'eee'}/>
        </View>
      `),
    )

    const targets = [
      TP.instancePath(TestScenePath, ['aaa', 'bbb']),
      TP.instancePath(TestScenePath, ['aaa', 'bbb', 'ccc', 'ddd']),
    ]

    await renderResult.dispatch(
      [reparentComponents(targets, TP.instancePath(TestScenePath, ['aaa', 'eee']))],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View data-uid={'eee'}>
            <View data-uid={'ddd'} style={{ left: 52, top: -141 }} />
            <View
              style={{ backgroundColor: '#0091FFAA', left: 52, width: 256, height: 202, top: -141 }}
              layout={{ layoutSystem: 'pinSystem' }}
              data-uid={'bbb'}
            >
              <View data-uid={'ccc'} />
            </View>
          </View>
        </View>
      `,
      ),
    )
  })

  it('reparents a pinned element to flex', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200, display: 'flex' }}
            data-uid={'bbb'}
          >
            <View data-uid={'ccc'} layout={{ layoutSystem: 'pinSystem', flexBasis: 70, crossBasis: 50 }} />
          </View>
          <View data-uid={'eee'} layout={{ layoutSystem: 'pinSystem' }} style={{ left: 50, top: 175, width: 80, height: 80 }}/>
        </View>
      `),
    )

    await renderResult.dispatch(
      [
        reparentComponents(
          [TP.instancePath(TestScenePath, ['aaa', 'eee'])],
          TP.instancePath(TestScenePath, ['aaa', 'bbb']),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200, display: 'flex' }}
            data-uid={'bbb'}
          >
            <View data-uid={'ccc'} layout={{ layoutSystem: 'pinSystem', flexBasis: 70, crossBasis: 50 }} />
            <View data-uid={'eee'} layout={{ layoutSystem: 'pinSystem', flexBasis: 80, crossBasis: 80 }} style={{}} />
          </View>
        </View>
      `),
    )
  })

  it('reparents a pinned element to flex using magic?', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200, display: 'flex' }}
            data-uid={'bbb'}
          >
            <View data-uid={'ccc'} style={{ backgroundColor: '#ff00ff' }} layout={{ layoutSystem: 'pinSystem', flexBasis: 70, crossBasis: 50 }} />
          </View>
          <View data-uid={'eee'} style={{ backgroundColor: '#00ff00', left: 150, top: 250, width: 80, height: 80 }} layout={{ layoutSystem: 'pinSystem' }}/>
        </View>
      `),
    )

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'eee'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId(
      'component-area-control-utopia-storyboard-uid/scene-aaa:aaa/eee-2',
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
          metaKey: true,
          clientX: areaControlBounds.left + 5,
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
          metaKey: true,
          clientX: areaControlBounds.left + 5,
          clientY: areaControlBounds.top - 25,
        }),
      )
      await domFinished
      await dispatchDone
    })

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200, display: 'flex' }}
            data-uid={'bbb'}
          >
            <View data-uid={'ccc'} style={{ backgroundColor: '#ff00ff' }} layout={{ layoutSystem: 'pinSystem', flexBasis: 70, crossBasis: 50 }} />
            <View data-uid={'eee'} style={{ backgroundColor: '#00ff00' }} layout={{ layoutSystem: 'pinSystem', flexBasis: 80, crossBasis: 80 }} />
          </View>
        </View>
      `),
    )
  })

  it('inserting a new element', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200, display: 'flex' }}
            data-uid={'bbb'}
          >
            <View data-uid={'ccc'} style={{ backgroundColor: '#ff00ff' }} layout={{ layoutSystem: 'pinSystem', flexBasis: 20, crossBasis: 20 }} />
          </View>
        </View>
      `),
    )
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)

    await renderResult.dispatch(
      [selectComponents([TP.instancePath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId(
      'component-area-control-utopia-storyboard-uid/scene-aaa:aaa/bbb-1',
    )
    const areaControlBounds = areaControl.getBoundingClientRect()

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent.keyDown(areaControl, { key: 'v', keyCode: 86 })
      await dispatchDone
    })

    const insertModeMouseCatcher = renderResult.renderedDOM.getByTestId(
      'insert-target-utopia-storyboard-uid/scene-aaa:aaa/bbb',
    )

    await act(async () => {
      fireEvent(
        insertModeMouseCatcher,
        new MouseEvent('mouseover', {
          bubbles: true,
          cancelable: true,
          clientX: areaControlBounds.left + 25,
          clientY: areaControlBounds.top + 25,
        }),
      )
    })

    await act(async () => {
      fireEvent(
        insertModeMouseCatcher,
        new MouseEvent('mousedown', {
          bubbles: true,
          cancelable: true,
          clientX: areaControlBounds.left + 25,
          clientY: areaControlBounds.top + 25,
          buttons: 1,
        }),
      )
    })

    await act(async () => {
      fireEvent(
        insertModeMouseCatcher,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          clientX: areaControlBounds.left + 100,
          clientY: areaControlBounds.top + 100,
          buttons: 1,
        }),
      )
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        insertModeMouseCatcher,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          clientX: areaControlBounds.left + 100,
          clientY: areaControlBounds.top + 100,
        }),
      )
      await domFinished
      await dispatchDone
    })

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toMatch(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} layout={{ layoutSystem: 'pinSystem' }} data-uid={'aaa'}>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200, display: 'flex' }}
            data-uid={'bbb'}
          >
            <View data-uid={'ccc'} style={{ backgroundColor: '#ff00ff' }} layout={{ layoutSystem: 'pinSystem', flexBasis: 20, crossBasis: 20 }} />
            <View
              style={{ backgroundColor: '#0091FFAA' }}
              data-uid={'${NewUID}'}
              layout={{ layoutSystem: 'pinSystem', flexBasis: 75, crossBasis: 75 }}
            />
          </View>
        </View>
      `),
    )
  })
})
