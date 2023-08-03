import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from './ui-jsx.test-utils' // IMPORTANT - THIS IMPORT MUST ALWAYS COME FIRST
import type { CanvasVector } from '../../core/shared/math-utils'
import { canvasRectangle } from '../../core/shared/math-utils'
import { setCanvasFrames } from '../editor/actions/action-creators'
import * as EP from '../../core/shared/element-path'
import type { EdgePosition } from './canvas-types'
import { pinFrameChange, pinMoveChange, pinSizeChange, singleResizeChange } from './canvas-types'

describe('updateFramesOfScenesAndComponents - pinFrameChange -', () => {
  it('a simple TLWH pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    const pinChange = pinFrameChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20, width: 100, height: 100 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('a simple TLWH pin change works with old CanvasMetadata format as well', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    const pinChange = pinFrameChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20, width: 100, height: 100 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('TLWH, but W and H are percentage works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, width: '50%', height: '20%'  }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    const pinChange = pinFrameChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20, width: '25%', height: '25%' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('TLW, missing H pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, width: 256 }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    const pinChange = pinFrameChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20, width: 100, height: 100 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('TLWHBR, too many frame points work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, width: 256, height: 202, bottom: 137, right: 93 }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    const pinChange = pinFrameChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20, width: 100, height: 100 }}
          ${/** notice how the extraneous pins were removed automatically */ ''}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('TLRB pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, right: 50, bottom: 20 }}
          data-uid='bbb'
        />
      </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinFrameChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20, right: 280, bottom: 280 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('no layout prop on child', async () => {
    const renderResult = await renderTestEditorWithCode(
      // FIXME This should be fine with {{ ...props.style }} but that causes the uids to switch in tests only
      makeTestProjectCodeWithSnippet(`
      <View style={ props.style } data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33' }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    const pinChange = pinFrameChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={ props.style } data-uid='aaa'>
        <View
          ${/** pins are magically created */ ''}
          style={{ backgroundColor: '#aaaaaa33', left: 20, top: 20, width: 100, height: 100 }}
          data-uid='bbb'
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
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61 }}
            data-uid='bbb'
          />
        </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20 }}
            data-uid='bbb'
          />
        </View>`,
      ),
    )
  })

  it('only TL pins work with old CanvasMetadata as well', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61 }}
            data-uid='bbb'
          />
        </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20 }}
            data-uid='bbb'
          />
        </View>`,
      ),
    )
  })

  it('only RB pins work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', right: 50, bottom: 50 }}
            data-uid='bbb'
          />
        </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: 20,
      y: 20,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', right: 30, bottom: 30 }}
            data-uid='bbb'
          />
        </View>`,
      ),
    )
  })

  it('just R pin gets turned into T,R', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', right: 50 }}
            data-uid='bbb'
          />
        </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: 20,
      y: 20,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', right: 30, top: 20 }}
            data-uid='bbb'
          />
        </View>`,
      ),
    )
  })

  it('just B pin gets turned into L,B', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', bottom: 50 }}
            data-uid='bbb'
          />
        </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: 20,
      y: 20,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', bottom: 30, left: 20 }}
            data-uid='bbb'
          />
        </View>`,
      ),
    )
  })

  it('just B pin doesn`t turn into L,B with deltaX=0', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', bottom: 50 }}
            data-uid='bbb'
          />
        </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: 0,
      y: 20,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', bottom: 30 }}
            data-uid='bbb'
          />
        </View>`,
      ),
    )
  })

  it('TLWH, but W and H are left alone', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', width: '50%', height: 25, left: 52, top: 61 }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', width: '50%', height: 25, left: 20, top: 20  }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('TLWH, but W and H are left alone, T, L are % values', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', width: '50%', height: 25, left: '10%', top: '5%' }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: 45,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', width: '50%', height: 25, left: '2%', top: '16.3%' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('TLRB pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, right: 50, bottom: 20 }}
          data-uid='bbb'
        />
      </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20, right: 82, bottom: 61 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('TLRB pin change works, with % values', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '10%', top: '15%', right: '10%', bottom: '25%' }}
          data-uid='bbb'
        />
      </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: 20,
      y: 65,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: '15%',
            top: '31.3%',
            right: '5%',
            bottom: '8.8%',
          }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('TLWHBR, too many frame points work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, width: 256, height: 202, bottom: 137, right: 93 }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            ${
              /**
               * we correctly update left, top, bottom, and right, but we do nothing to fix the fact that there are 6 pins competing for priorities here
               * if we later revise the behavior and change how PIN_MOVE works when there is too many pins, don't be surprised if this test here breaks
               *  */ ''
            }
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 20,
              top: 20,
              width: 256,
              height: 202,
              bottom: 178,
              right: 125,
            }}
            data-uid='bbb'
          />
      </View>`,
      ),
    )
  })

  it('TLR, no B pin change?', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, right: 50 }}
          data-uid='bbb'
        />
      </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinMoveChange(EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']), {
      x: -32,
      y: -41,
    } as CanvasVector)

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20, right: 82 }}
          data-uid='bbb'
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
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61 }}
            data-uid='bbb'
          />
        </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinSizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
      null,
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20, width: 100, height: 100 }}
            data-uid='bbb'
          />
        </View>`,
      ),
    )
  })

  it('only TW pins work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, width: 100 }}
            data-uid='bbb'
          />
        </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinSizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
      null,
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    // THIS IS THE IMPORTANT TEST, IT POINTS OUT THAT WE DO NOT ADD AN UNNECESSARY TOP PROPERTY, ONLY HEIGHT

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, width: 100, height: 100 }}
            data-uid='bbb'
          />
        </View>`,
      ),
    )
  })

  it('TLRB pins work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, right: 150, bottom: 150 }}
            data-uid='bbb'
          />
        </View>`,
      ),
      'await-first-dom-report',
    )

    const pinChange = pinSizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 20, y: 20, width: 100, height: 100 }),
      null,
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 20, top: 20, right: 280, bottom: 280 }}
            data-uid='bbb'
          />
        </View>`,
      ),
    )
  })
})

describe('updateFramesOfScenesAndComponents - singleResizeChange -', () => {
  it('TLWH, but W and H are percentage works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 40, width: '50%', height: '20%'  }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    const pinChange = singleResizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: -20, y: -10 } as CanvasVector,
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 40, top: 40, width: '45%', height: '17.5%' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('no layout prop on child', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33' }}
          data-uid='bbb'
        />
      </View>
      `),
      'await-first-dom-report',
    )

    const pinChange = singleResizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      { x: 0, y: 0 } as EdgePosition,
      { x: 50, y: 60 } as CanvasVector,
    )

    await renderResult.dispatch([setCanvasFrames([pinChange], false)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          ${/** pins are magically created */ ''}
          style={{ backgroundColor: '#aaaaaa33', top: -60, height: 60, left: -50, width: 50 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})
