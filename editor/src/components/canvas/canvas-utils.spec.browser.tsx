import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithProjectContent,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from './ui-jsx.test-utils' // IMPORTANT - THIS IMPORT MUST ALWAYS COME FIRST
import { fireEvent, act } from '@testing-library/react'
import { generateUidWithExistingComponents } from '../../core/model/element-template-utils'
import { canvasRectangle, CanvasVector } from '../../core/shared/math-utils'
import {
  selectComponents,
  setCanvasFrames,
  unwrapGroupOrView,
  wrapInView,
} from '../editor/actions/action-creators'
import { reparentComponents } from '../navigator/actions'
import * as EP from '../../core/shared/element-path'
import {
  pinFrameChange,
  pinMoveChange,
  pinSizeChange,
  singleResizeChange,
  EdgePosition,
} from './canvas-types'
import { wait } from '../../utils/utils.test-utils'
import { CanvasControlsContainerID } from './controls/new-canvas-controls'
import { PrettierConfig } from 'utopia-vscode-common'
import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import * as Prettier from 'prettier'
import { setElectronWindow } from '../../core/shared/test-setup.test-utils'
import { contentsToTree } from '../assets'
import { createCodeFile } from '../custom-code/code-file.test-utils'
import { DefaultPackageJson, StoryboardFilePath } from '../editor/store/editor-state'
import { directory } from '../../core/model/project-file-utils'
import {
  ProjectContents,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../../core/shared/project-file-types'

const NewUID = 'catdog'

describe('updateFramesOfScenesAndComponents - pinFrameChange -', () => {
  beforeAll(setElectronWindow)
  it('a simple TLWH pin change works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
      </View>
      `),
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20, width: 100, height: 100 }}
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
      </View>
      `),
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20, width: 100, height: 100 }}
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61, width: '50%', height: '20%'  }}
          data-uid='bbb'
        />
      </View>
      `),
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20, width: '25%', height: '25%' }}
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61, width: 256 }}
          data-uid='bbb'
        />
      </View>
      `),
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20, width: 100, height: 100 }}
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61, width: 256, height: 202, bottom: 137, right: 93 }}
          data-uid='bbb'
        />
      </View>
      `),
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20, width: 100, height: 100 }}
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61, right: 50, bottom: 20 }}
          data-uid='bbb'
        />
      </View>`,
      ),
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20, right: 280, bottom: 280 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('no layout prop on child', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA' }}
          data-uid='bbb'
        />
      </View>
      `),
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
          ${/** pins are magically created */ ''}
          style={{ backgroundColor: '#0091FFAA', left: 20, top: 20, width: 100, height: 100 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})

describe('updateFramesOfScenesAndComponents - pinMoveChange -', () => {
  beforeAll(setElectronWindow)
  it('only TL pins work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61 }}
            data-uid='bbb'
          />
        </View>`,
      ),
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20 }}
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61 }}
            data-uid='bbb'
          />
        </View>`,
      ),
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20 }}
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', right: 50, bottom: 50 }}
            data-uid='bbb'
          />
        </View>`,
      ),
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', right: 30, bottom: 30 }}
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', right: 50 }}
            data-uid='bbb'
          />
        </View>`,
      ),
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', right: 30, top: 20 }}
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', bottom: 50 }}
            data-uid='bbb'
          />
        </View>`,
      ),
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', bottom: 30, left: 20 }}
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', bottom: 50 }}
            data-uid='bbb'
          />
        </View>`,
      ),
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', bottom: 30 }}
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', width: '50%', height: 25, left: 52, top: 61 }}
          data-uid='bbb'
        />
      </View>
      `),
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', width: '50%', height: 25, left: 20, top: 20  }}
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', width: '50%', height: 25, left: '10%', top: '5%' }}
          data-uid='bbb'
        />
      </View>
      `),
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', width: '50%', height: 25, left: '2%', top: '16.3%' }}
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61, right: 50, bottom: 20 }}
          data-uid='bbb'
        />
      </View>`,
      ),
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20, right: 82, bottom: 61 }}
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '10%', top: '15%', right: '10%', bottom: '25%' }}
          data-uid='bbb'
        />
      </View>`,
      ),
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
            backgroundColor: '#0091FFAA',
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61, width: 256, height: 202, bottom: 137, right: 93 }}
          data-uid='bbb'
        />
      </View>
      `),
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
              backgroundColor: '#0091FFAA',
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61, right: 50 }}
          data-uid='bbb'
        />
      </View>`,
      ),
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20, right: 82 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})

describe('updateFramesOfScenesAndComponents - pinSizeChange -', () => {
  beforeAll(setElectronWindow)
  it('only TL pins work', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61 }}
            data-uid='bbb'
          />
        </View>`,
      ),
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20, width: 100, height: 100 }}
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, width: 100 }}
            data-uid='bbb'
          />
        </View>`,
      ),
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, width: 100, height: 100 }}
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 52, top: 61, right: 150, bottom: 150 }}
            data-uid='bbb'
          />
        </View>`,
      ),
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
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 20, top: 20, right: 280, bottom: 280 }}
            data-uid='bbb'
          />
        </View>`,
      ),
    )
  })
})

describe('updateFramesOfScenesAndComponents - singleResizeChange -', () => {
  beforeAll(setElectronWindow)
  it('TLWH, but W and H are percentage works', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 40, width: '50%', height: '20%'  }}
          data-uid='bbb'
        />
      </View>
      `),
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
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 40, top: 40, width: '45%', height: '17.5%' }}
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
          style={{ backgroundColor: '#0091FFAA' }}
          data-uid='bbb'
        />
      </View>
      `),
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
          style={{ backgroundColor: '#0091FFAA', top: -60, height: 60, left: -50, width: 50 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})

describe('moveTemplate', () => {
  beforeAll(setElectronWindow)
  it('wraps in 1 element', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ position: 'absolute', backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        />
      </View>
      `),
    )

    const targets = [EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])]
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)

    await renderResult.dispatch([wrapInView(targets, 'default-empty-View')], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
          data-uid='${NewUID}'
        >
          <View
            style={{ position: 'absolute', backgroundColor: '#0091FFAA', left: 0, top: 0, width: 256, height: 202 }}
            data-uid='bbb'
          />
        </View>
      </View>
      `),
    )
  })
  it('wraps multiselected elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style, width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202, position: 'absolute' }}
          data-uid='bbb'
        >
          <View
            style={{ left: 10, top: 10, width: 100, height: 100, position: 'absolute' }}
            data-uid='ccc'
          >
            <View data-uid='ddd' />
          </View>
        </View>
        <View data-uid='eee'/>
        <View
          style={{ left: 10, top: 10, width: 256, height: 150, position: 'absolute' }}
          data-uid='fff'
        >
            <View
              style={{ left: 5, top: 0, width: 246, height: 150, position: 'absolute'  }}
              data-uid='ggg'
            />
          </View>
        <View data-uid='hhh'/>
      </View>
      `),
    )

    const targets = [
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc']),
      EP.appendNewElementPath(TestScenePath, ['aaa', 'fff', 'ggg']),
    ]
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)

    await renderResult.dispatch([wrapInView(targets, 'default-empty-View')], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style, width: '100%', height: '100%', position: 'relative' }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202, position: 'absolute' }}
          data-uid='bbb'
        />
        <View data-uid='eee' />
        <View
          style={{ left: 10, top: 10, width: 256, height: 150, position: 'absolute' }}
          data-uid='fff'
        />
        <View data-uid='hhh' />
        <View
          style={{ position: 'absolute', left: 15, top: 10, width: 246, height: 161 }}
          data-uid='${NewUID}'
        >
          <View
            style={{ left: 47, top: 61, width: 100, height: 100, position: 'absolute' }}
            data-uid='ccc'
          >
            <View data-uid='ddd' />
          </View>
          <View
            style={{ left: 0, top: 0, width: 246, height: 150, position: 'absolute' }}
            data-uid='ggg'
          />
        </View>
      </View>
      `),
    )
  })
  it('wraps in multiselected element and children, moves only the element, keeps children', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View
          style={{ position: 'absolute', backgroundColor: '#0091FFAA', left: 52, top: 61, width: 256, height: 202 }}
          data-uid='bbb'
        >
          <View data-uid='ccc'>
            <View data-uid='ddd' />
          </View>
        </View>
        <View data-uid='eee'/>
      </View>
      `),
    )

    const targets = [
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc']),
    ]
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)

    await renderResult.dispatch([wrapInView(targets, 'default-empty-View')], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
        <View data-uid='eee' />
        <View
          style={{ position: 'absolute', left: 52, top: 61, width: 256, height: 202 }}
          data-uid='${NewUID}'
        >
          <View
            style={{ position: 'absolute', backgroundColor: '#0091FFAA', left: 0, top: 0, width: 256, height: 202 }}
            data-uid='bbb'
          >
            <View data-uid='ccc'>
              <View data-uid='ddd' />
            </View>
          </View>
        </View>
      </View>
      `),
    )
  })
  it('wraps in 1 non-storyboard element', async () => {
    const appFilePath = '/src/app.js'
    let projectContents: ProjectContents = {
      '/package.json': textFile(
        textFileContents(
          JSON.stringify(DefaultPackageJson, null, 2),
          unparsed,
          RevisionsState.BothMatch,
        ),
        null,
        0,
      ),
      '/src': directory(),
      '/utopia': directory(),
      [StoryboardFilePath]: createCodeFile(
        StoryboardFilePath,
        `
  import * as React from 'react'
  import { Scene, Storyboard } from 'utopia-api'
  import { App } from '/src/app.js'

  export var storyboard = (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        data-uid='${TestSceneUID}'
        style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
      >
        <App data-uid='${TestAppUID}' />
      </Scene>
    </Storyboard>
  )`,
      ),
      [appFilePath]: createCodeFile(
        appFilePath,
        `
  import * as React from 'react'
  export var App = (props) => {
    return <div data-uid='app-outer-div' style={{position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF'}}>
      <div data-uid='app-inner-div' style={{ width: 50, height: 100 }}><span data-uid='app-inner-span'>hello</span></div>
    </div>
  }`,
      ),
    }
    const renderResult = await renderTestEditorWithProjectContent(contentsToTree(projectContents))
    const targetPath = EP.appendNewElementPath(TestScenePath, ['app-outer-div', 'app-inner-div'])

    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)

    await renderResult.dispatch([wrapInView([targetPath], 'default-empty-View')], true)
    expect(getPrintedUiJsCode(renderResult.getEditorState(), appFilePath)).toEqual(
      Prettier.format(
        `
    import * as React from 'react'
    import { View } from 'utopia-api'
    export var App = (props) => {
      return (
        <div
          data-uid='app-outer-div'
          style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
        >
          <View
            style={{ position: 'absolute', left: 0, top: 0, width: 50, height: 100 }}
            data-uid='${NewUID}'
          >
            <div
              data-uid='app-inner-div'
              style={{ width: 50, height: 100, left: 0, top: 0, position: 'absolute' }}
            >
              <span data-uid='app-inner-span'>hello</span>
            </div>
          </View>
        </div>
      )
    }`,
        PrettierConfig,
      ),
    )
  })
  it('unwraps 1 non-storyboard element', async () => {
    const appFilePath = '/src/app.js'
    let projectContents: ProjectContents = {
      '/package.json': textFile(
        textFileContents(
          JSON.stringify(DefaultPackageJson, null, 2),
          unparsed,
          RevisionsState.BothMatch,
        ),
        null,
        0,
      ),
      '/src': directory(),
      '/utopia': directory(),
      [StoryboardFilePath]: createCodeFile(
        StoryboardFilePath,
        `
  import * as React from 'react'
  import { Scene, Storyboard } from 'utopia-api'
  import { App } from '/src/app.js'

  export var storyboard = (
    <Storyboard data-uid='${BakedInStoryboardUID}'>
      <Scene
        data-uid='${TestSceneUID}'
        style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
      >
        <App data-uid='${TestAppUID}' />
      </Scene>
    </Storyboard>
  )`,
      ),
      [appFilePath]: createCodeFile(
        appFilePath,
        `
  import * as React from 'react'
  import { View } from 'utopia-api'
  export var App = (props) => {
    return <div data-uid='app-outer-div' style={{position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF'}}>
      <View data-uid='app-wrapper-view'>
        <div data-uid='app-inner-div' style={{ width: 50, height: 100 }}><span data-uid='app-inner-span'>hello</span></div>
      </View>
    </div>
  }`,
      ),
    }
    const renderResult = await renderTestEditorWithProjectContent(contentsToTree(projectContents))
    const targetPath = EP.appendNewElementPath(TestScenePath, ['app-outer-div', 'app-wrapper-view'])
    const selectionAfterUnwrap = EP.appendNewElementPath(TestScenePath, [
      'app-outer-div',
      'app-inner-div',
    ])

    await renderResult.dispatch([unwrapGroupOrView(targetPath)], true)
    expect(getPrintedUiJsCode(renderResult.getEditorState(), appFilePath)).toEqual(
      Prettier.format(
        `
    import * as React from 'react'
    import { View } from 'utopia-api'
    export var App = (props) => {
      return (
        <div
          data-uid='app-outer-div'
          style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
        >
          <div
            data-uid='app-inner-div'
            style={{ width: 50, height: 100 }}
          >
            <span data-uid='app-inner-span'>hello</span>
          </div>
        </div>
      )
    }`,
        PrettierConfig,
      ),
    )
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([selectionAfterUnwrap])
  })
  it('reparents multiselected elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'relative', left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
          >
            <View data-uid='ccc'>
              <View data-uid='ddd' />
            </View>
          </View>
          <View data-uid='eee'/>
          <View data-uid='fff'>
              <View data-uid='ggg' />
            </View>
          <View data-uid='hhh'/>
        </View>
      `),
    )

    const targets = [
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      EP.appendNewElementPath(TestScenePath, ['aaa', 'hhh']),
      EP.appendNewElementPath(TestScenePath, ['aaa', 'fff', 'ggg']),
    ]

    await renderResult.dispatch(
      [reparentComponents(targets, EP.appendNewElementPath(TestScenePath, ['aaa', 'eee']))],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View data-uid='eee'>
            <View
              style={{ backgroundColor: '#0091FFAA', position: 'relative', left: 52, top: -141, width: 256, height: 202 }}
              data-uid='bbb'
            >
              <View data-uid='ccc'>
                <View data-uid='ddd' />
              </View>
            </View>
            <View data-uid='ggg' />
            <View data-uid='hhh' style={{ top: 0 }} />
          </View>
          <View data-uid='fff' />
        </View>
      `),
    )
  })
  it('reparents multiselected element and children, moves only the element, keeps children', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'relative', left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
          >
            <View data-uid='ccc'>
              <View data-uid='ddd' />
            </View>
          </View>
          <View data-uid='eee'/>
        </View>
      `),
    )

    const targets = [
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc']),
    ]

    await renderResult.dispatch(
      [reparentComponents(targets, EP.appendNewElementPath(TestScenePath, ['aaa', 'eee']))],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View data-uid='eee'>
            <View
              style={{ backgroundColor: '#0091FFAA', position: 'relative', left: 52, top: -141, width: 256, height: 202}}
              data-uid='bbb'
            >
              <View data-uid='ccc'>
                <View data-uid='ddd' />
              </View>
            </View>
          </View>
        </View>
      `),
    )
  })
  it('reparents multiselected element and descendant which are not direct children, moves both of the elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'relative', left: 52, top: 61, width: 256, height: 202 }}
            data-uid='bbb'
          >
            <View data-uid='ccc'>
              <View data-uid='ddd' />
            </View>
          </View>
          <View data-uid='eee'/>
        </View>
      `),
    )

    const targets = [
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', 'ccc', 'ddd']),
    ]

    await renderResult.dispatch(
      [reparentComponents(targets, EP.appendNewElementPath(TestScenePath, ['aaa', 'eee']))],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...props.style }} data-uid='aaa'>
          <View data-uid='eee'>
            <View
              style={{ backgroundColor: '#0091FFAA', position: 'relative', left: 52, top: -141, width: 256, height: 202 }}
              data-uid='bbb'
            >
              <View data-uid='ccc' />
            </View>
            <View data-uid='ddd' style={{ left: 52, top: -141 }} />
          </View>
        </View>
      `,
      ),
    )
  })

  it('reparents a pinned element to flex', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200, display: 'flex' }}
            data-uid='bbb'
          >
            <View data-uid='ccc' style={{ flexBasis: 70, height: 50 }} />
          </View>
          <View data-uid='eee' style={{ position: 'absolute', left: 50, top: 175, width: 80, height: 80 }}/>
        </View>
      `),
    )

    await renderResult.dispatch(
      [
        reparentComponents(
          [EP.appendNewElementPath(TestScenePath, ['aaa', 'eee'])],
          EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
        ),
      ],
      true,
    )

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200, display: 'flex' }}
            data-uid='bbb'
          >
            <View data-uid='ccc' style={{ flexBasis: 70, height: 50 }} />
            <View data-uid='eee' style={{ position: 'relative', flexBasis: 80, height: 80 }} />
          </View>
        </View>
      `),
    )
  })

  it('reparents a pinned element to flex using magic?', async () => {
    //const currentWindow = require('electron').remote.getCurrentWindow()
    //currentWindow.show()
    //currentWindow.setPosition(500, 200)
    //currentWindow.setSize(2200, 1000)
    //currentWindow.openDevTools()
    // This is necessary because the test code races against the Electron process
    // opening the window it would appear.
    //await wait(10000)
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200, display: 'flex' }}
            data-uid='bbb'
          >
            <View data-uid='ccc' style={{ backgroundColor: '#ff00ff', flexBasis: 70, height: 50 }} />
          </View>
          <View data-testid='eee' data-uid='eee' style={{ position: 'relative', backgroundColor: '#00ff00', left: 150, top: 250, width: 80, height: 80 }}/>
        </View>
      `),
    )

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'eee'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId('eee')

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
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        canvasControlsLayer,
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
        canvasControlsLayer,
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

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200, display: 'flex' }}
            data-uid='bbb'
          >
            <View data-uid='ccc' style={{ backgroundColor: '#ff00ff', flexBasis: 70, height: 50 }} />
            <View data-testid='eee'  data-uid='eee' style={{ backgroundColor: '#00ff00', position: 'relative', flexBasis: 80, height: 80 }} />
          </View>
        </View>
      `),
    )
  })

  it('reparents an orphan from the canvas', async () => {
    const renderResult = await renderTestEditorWithCode(
      Prettier.format(
        `
      import * as React from 'react'
      import { Scene, Storyboard, View } from 'utopia-api'
    
      export var App = (props) => {
        return (
          <div style={{ position: 'relative', width: '100%', height: '100%'}} data-uid='aaa' />
        )
      }
    
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid='${BakedInStoryboardUID}'>
            <Scene
              style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
              data-uid='${TestSceneUID}'
            >
              <App data-uid='${TestAppUID}' />
            </Scene>
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                width: 100,
                height: 100,
                left: 350,
                top: 0,
              }}
              data-uid='orphan-bbb'
              data-testid='orphan-bbb'
            />
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
    )

    await renderResult.dispatch(
      [selectComponents([EP.elementPath([[BakedInStoryboardUID, 'orphan-bbb']])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId('orphan-bbb')

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
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: areaControlBounds.left - 5,
          clientY: areaControlBounds.top + 5,
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
        canvasControlsLayer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          metaKey: true,
          clientX: areaControlBounds.left - 5,
          clientY: areaControlBounds.top + 5,
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
          clientX: areaControlBounds.left - 5,
          clientY: areaControlBounds.top + 5,
        }),
      )
      await domFinished
      await dispatchDone
    })

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
      import * as React from 'react'
      import { Scene, Storyboard, View } from 'utopia-api'
    
      export var App = (props) => {
        return (
          <div style={{ position: 'relative', width: '100%', height: '100%'}} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                width: 100,
                height: 100,
                left: 340,
                top: 0,
              }}
              data-uid='orphan-bbb'
              data-testid='orphan-bbb'
            />
          </div>
        )
      }
    
      export var ${BakedInStoryboardVariableName} = (props) => {
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
      ),
    )
  })

  it('reparenting to the canvas creates an orphan', async () => {
    const renderResult = await renderTestEditorWithCode(
      Prettier.format(
        `
      import * as React from 'react'
      import { Scene, Storyboard, View } from 'utopia-api'
    
      export var App = (props) => {
        return (
          <div style={{ position: 'relative', width: '100%', height: '100%'}} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                width: 100,
                height: 100,
                left: 0,
                top: 0,
              }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </div>
        )
      }
    
      export var ${BakedInStoryboardVariableName} = (props) => {
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
      ),
    )

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId('bbb')

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
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        canvasControlsLayer,
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
        canvasControlsLayer,
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

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
      import * as React from 'react'
      import { Scene, Storyboard, View } from 'utopia-api'
    
      export var App = (props) => {
        return (
          <div style={{ position: 'relative', width: '100%', height: '100%'}} data-uid='aaa' />
        )
      }
    
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid='${BakedInStoryboardUID}'>
            <Scene
              style={{ position: 'absolute', left: 0, top: 0, width: 400, height: 400 }}
              data-uid='${TestSceneUID}'
            >
              <App data-uid='${TestAppUID}' />
            </Scene>
            <div
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                width: 100,
                height: 100,
                left: 0,
                top: -30,
              }}
              data-uid='bbb'
              data-testid='bbb'
            />
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
    )
  })

  it('inserting a new element', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ position: 'relative', width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200, display: 'flex', position: 'absolute' }}
            data-uid='bbb'
            data-testid='bbb'
          >
            <div data-uid='ccc' style={{ backgroundColor: '#ff00ff', flexBasis: 20, height: 20 }} />
          </div>
        </div>
      `),
    )
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)
    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent.keyDown(canvasRoot, { key: 'v', keyCode: 86 })
      await dispatchDone
    })

    const canvasControlContainer = renderResult.renderedDOM.getByTestId(
      'new-canvas-controls-container',
    )

    const insertionArea = renderResult.renderedDOM.getByTestId('bbb')
    const areaControlBounds = insertionArea.getBoundingClientRect()

    await act(async () => {
      fireEvent(
        canvasControlContainer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          clientX: areaControlBounds.left + 25,
          clientY: areaControlBounds.top + 25,
        }),
      )
    })

    await act(async () => {
      fireEvent(
        canvasControlContainer,
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
        canvasControlContainer,
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
        canvasControlContainer,
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

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ position: 'relative', width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200, display: 'flex', position: 'absolute' }}
            data-uid='bbb'
            data-testid='bbb'
          >
            <div data-uid='ccc' style={{ backgroundColor: '#ff00ff', flexBasis: 20, height: 20 }} />
            <View
              style={{ backgroundColor: '#0091FFAA', position: 'relative', height: 74, flexBasis: 74 }}
              data-uid='${NewUID}'
            />
          </div>
        </div>
      `),
    )
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb', NewUID]),
    ])
  })

  it('inserting a new element as an orphan', async () => {
    const renderResult = await renderTestEditorWithCode(
      Prettier.format(
        `
      import * as React from 'react'
      import { Scene, Storyboard, View } from 'utopia-api'
    
      export var App = (props) => {
        return (
          <div style={{ position: 'relative', width: '100%', height: '100%'}} data-uid='aaa' />
        )
      }
    
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid='storyboard'>
            <Scene
              style={{ position: 'absolute', left: 0, top: 0, width: 100, height: 100 }}
              data-uid='${TestSceneUID}'
            >
              <App data-uid='${TestAppUID}' />
            </Scene>
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
    )
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)

    const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent.keyDown(canvasRoot, { key: 'v', keyCode: 86 })
      await dispatchDone
    })

    const canvasControlContainer = renderResult.renderedDOM.getByTestId(
      'new-canvas-controls-container',
    )
    const areaControlBounds = canvasControlContainer.getBoundingClientRect()

    await act(async () => {
      fireEvent(
        canvasControlContainer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          clientX: areaControlBounds.left + 120,
          clientY: areaControlBounds.top + 0,
        }),
      )
    })

    await act(async () => {
      fireEvent(
        canvasControlContainer,
        new MouseEvent('mousedown', {
          bubbles: true,
          cancelable: true,
          clientX: areaControlBounds.left + 120,
          clientY: areaControlBounds.top + 0,
          buttons: 1,
        }),
      )
    })

    await act(async () => {
      fireEvent(
        canvasControlContainer,
        new MouseEvent('mousemove', {
          bubbles: true,
          cancelable: true,
          clientX: areaControlBounds.left + 180,
          clientY: areaControlBounds.top + 50,
          buttons: 1,
        }),
      )
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        canvasControlContainer,
        new MouseEvent('mouseup', {
          bubbles: true,
          cancelable: true,
          clientX: areaControlBounds.left + 180,
          clientY: areaControlBounds.top + 50,
        }),
      )
      await domFinished
      await dispatchDone
    })

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `
      import * as React from 'react'
      import { Scene, Storyboard, View } from 'utopia-api'
    
      export var App = (props) => {
        return (
          <div style={{ position: 'relative', width: '100%', height: '100%'}} data-uid='aaa' />
        )
      }
    
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid='storyboard'>
            <Scene
              style={{ position: 'absolute', left: 0, top: 0, width: 100, height: 100 }}
              data-uid='${TestSceneUID}'
            >
              <App data-uid='${TestAppUID}' />
            </Scene>
            <View
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                left: -160,
                top: -60,
                width: 60,
                height: 50,
              }}
              data-uid='${NewUID}'
            />
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
    )
    expect(renderResult.getEditorState().editor.selectedViews).toEqual([
      EP.elementPath([['storyboard', NewUID]]),
    ])
  })

  it('reparents an element while dragging', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ ...props.style }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
            data-uid='bbb'
          >
            <View data-uid='ccc' style={{ backgroundColor: '#ff00ff', position: 'absolute', top: 10, left: 15, width: 50, height: 60 }} />
          </View>
          <View data-testid='eee' data-uid='eee' style={{ backgroundColor: '#00ff00', position: 'absolute', left: 150, top: 250, width: 80, height: 80 }}/>
        </View>
      `),
    )

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'eee'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId('eee')

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
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
      fireEvent(
        window,
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

    //await wait(600000)
    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <View style={{ ...props.style }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 200, height: 200 }}
        data-uid='bbb'
      >
        <View
          data-uid='ccc'
          style={{ backgroundColor: '#ff00ff', position: 'absolute', top: 10, left: 15, width: 50, height: 60 }}
        />
        <View
          data-testid='eee'
          data-uid='eee'
          style={{ backgroundColor: '#00ff00',  position: 'absolute', left: 100, top: 170, width: 80, height: 80 }}
        />
      </View>
    </View>
      `),
    )
  })
  it('canvas select a sibling and drag immediately', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <View style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200, }}
            data-uid='bbb'
          />
          <View
            style={{ backgroundColor: '#0091FFAA', left: 55, top: 275, width: 200, height: 105 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </View>
      `),
    )
    ;(generateUidWithExistingComponents as any) = jest.fn().mockReturnValue(NewUID)

    await renderResult.dispatch(
      [selectComponents([EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb'])], false)],
      false,
    )

    const areaControl = renderResult.renderedDOM.getByTestId('ccc')
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
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
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
      await domFinished
      await dispatchDone
    })

    await act(async () => {
      const domFinished = renderResult.getDomReportDispatched()
      const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
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
          style={{ width: '100%', height: '100%' }}
          data-uid='aaa'
        >
          <View
            style={{ backgroundColor: '#0091FFAA', left: 50, top: 50, width: 200, height: 200 }}
            data-uid='bbb'
          />
          <View
            style={{ backgroundColor: '#0091FFAA', left: 95, top: 245, width: 200, height: 105 }}
            data-uid='ccc'
            data-testid='ccc'
          />
        </View>
      `),
    )
  })
})
