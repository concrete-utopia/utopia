import { appendNewElementPath } from '../../../core/shared/element-path'
import { isFeatureEnabled, setFeatureEnabled } from '../../../utils/feature-switches'
import { expectSingleUndoStep, wait } from '../../../utils/utils.test-utils'
import { mouseClickAtPoint } from '../../canvas/event-helpers.test-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../editor/actions/action-creators'
import { AddRemoveLayouSystemControlTestId } from '../../inspector/add-remove-layout-system-control'
import { FlexDirection } from '../../inspector/common/css-utils'
import { FlexAlignment, FlexJustifyContent, MaxContent } from '../../inspector/inspector-common'

type LTWH = [left: number, top: number, width: number | string, height: number | string]
type Size = [width: number, height: number]
type FlexProps = {
  left: number
  top: number
  padding?: string
  gap?: number
  alignItems?: FlexAlignment
  justifyContent?: FlexJustifyContent
  display: 'flex'
  flexDirection?: FlexDirection
  width: string | number
  height: string | number
}

describe('Smart Convert To Flex', () => {
  let originalFSValue: boolean = false
  before(() => {
    originalFSValue = isFeatureEnabled('Nine block control')
    setFeatureEnabled('Nine block control', true)
  })

  after(() => {
    setFeatureEnabled('Nine block control', originalFSValue)
  })

  it('handles zero children well', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 500, 150],
      children: [],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='a'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 50,
          top: 50,
          width: 500,
          height: 150,
          display: 'flex',
        }}
        data-uid='parent'
      />
    </div>
  `),
    )
  })

  it('converts a horizontal layout with zero padding and a gap of 15', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 500, 150],
      children: [
        [0, 0, 50, 50],
        [65, 0, 50, 50],
        [130, 0, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectWith({
        parent: {
          left: 50,
          top: 50,
          width: MaxContent,
          height: MaxContent,
          display: 'flex',
          flexDirection: 'row',
          gap: 15,
        },
        children: [
          [50, 50],
          [50, 50],
          [50, 50],
        ],
      }),
    )
  })

  it('converts a horizontal parent but with clearly vertical children as a vertical layout', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 500, 150],
      children: [
        [0, 0, 50, 50],
        [0, 60, 50, 50],
        [0, 120, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectWith({
        parent: {
          left: 50,
          top: 50,
          width: MaxContent,
          height: MaxContent,
          display: 'flex',
          flexDirection: 'column',
          gap: 10,
        },
        children: [
          [50, 50],
          [50, 50],
          [50, 50],
        ],
      }),
    )
  })

  it('converts a vertical parent with ambiguous children as a vertical layout', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 200, 300],
      children: [
        [0, 0, 50, 50],
        [60, 60, 50, 50],
        [120, 120, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectWith({
        parent: {
          left: 50,
          top: 50,
          width: MaxContent,
          height: MaxContent,
          display: 'flex',
          flexDirection: 'column',
          gap: 10,
        },
        children: [
          [50, 50],
          [50, 50],
          [50, 50],
        ],
      }),
    )
  })

  it('converts horizontal layout with symmetric 25px padding and a gap of 15', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 230, 150],
      children: [
        [25, 0, 50, 50],
        [90, 0, 50, 50],
        [155, 0, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectWith({
        parent: {
          left: 50,
          top: 50,
          width: MaxContent,
          height: MaxContent,
          display: 'flex',
          flexDirection: 'row',
          gap: 15,
          padding: '0 25px',
        },
        children: [
          [50, 50],
          [50, 50],
          [50, 50],
        ],
      }),
    )
  })

  it('converts horizontal layout with horizontal 25px padding, and vertical 20px', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 230, 150],
      children: [
        [25, 20, 50, 50],
        [90, 20, 50, 50],
        [155, 20, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectWith({
        parent: {
          left: 50,
          top: 50,
          width: MaxContent,
          height: MaxContent,
          display: 'flex',
          flexDirection: 'row',
          gap: 15,
          padding: '20px 25px',
        },
        children: [
          [50, 50],
          [50, 50],
          [50, 50],
        ],
      }),
    )
  })

  it('converts horizontal layout with symmetric 25px padding and no gap', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 230, 150],
      children: [
        [25, 0, 50, 50],
        [75, 0, 50, 50],
        [125, 0, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectWith({
        parent: {
          left: 50,
          top: 50,
          width: MaxContent,
          height: MaxContent,
          display: 'flex',
          flexDirection: 'row',
          padding: '0 25px',
        },
        children: [
          [50, 50],
          [50, 50],
          [50, 50],
        ],
      }),
    )
  })

  it('converts horizontal layout with single child 100% wide', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 230, 150],
      children: [[0, 0, '100%', 50]],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='a'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 50,
          top: 50,
          width: 230,
          height: 'max-content',
          display: 'flex',
        }}
        data-uid='parent'
      >
        <div 
          data-uid='child-0'
          style={{
            backgroundColor: '#aaaaaa33', 
            height: 50, 
            contain: 'layout',
            flexGrow: 1 
          }} 
        />
      </div>
    </div>
  `),
    )
  })

  it('converts horizontal layout with single child 100% height', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 230, 150],
      children: [[0, 0, 50, '100%']],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='a'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 50,
          top: 50,
          width: 'max-content',
          height: 150,
          display: 'flex',
        }}
        data-uid='parent'
      >
        <div 
          data-uid='child-0'
          style={{
            backgroundColor: '#aaaaaa33', 
            width: 50,
            height: '100%', 
            contain: 'layout',
          }} 
        />
      </div>
    </div>
  `),
    )
  })

  it('converts vertical layout with single child 100% wide', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 100, 300],
      children: [[0, 0, '100%', 50]],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='a'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 50,
          top: 50,
          width: 100,
          height: 'max-content',
          display: 'flex',
        }}
        data-uid='parent'
      >
        <div 
          data-uid='child-0'
          style={{
            backgroundColor: '#aaaaaa33', 
            width: '100%',
            height: 50, 
            contain: 'layout',
          }} 
        />
      </div>
    </div>
  `),
    )
  })
})

function renderProjectWith(input: { parent: LTWH; children: Array<LTWH> }) {
  const [parentL, parentT, parentW, parentH] = input.parent
  return renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(`
    <div style={{ ...props.style }} data-uid='a'>
      <div
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${parentL}, top: ${parentT}, width: ${parentW}, height: ${parentH}  }}
        data-uid='parent'
      >
        ${input.children
          .map((child, i) => {
            const [childL, childT, childW, childH] = child
            return `<div 
                data-uid='child-${i}' 
                style={{ 
                  backgroundColor: '#aaaaaa33', 
                  position: 'absolute', 
                  left: ${JSON.stringify(childL)}, 
                  top: ${JSON.stringify(childT)},
                  width: ${JSON.stringify(childW)},
                  height: ${JSON.stringify(childH)} }}
              />`
          })
          .join('\n')}
      </div>
    </div>
    `),
    'await-first-dom-report',
  )
}

function makeReferenceProjectWith(input: { parent: FlexProps; children: Array<Size> }): string {
  return makeTestProjectCodeWithSnippet(`
  <div style={{ ...props.style }} data-uid='a'>
    <div
      style={
        ${JSON.stringify({
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          ...input.parent,
        })}
      }
      data-uid='parent'
    >
      ${input.children
        .map((child, i) => {
          const [childW, childH] = child
          return `<div 
                  data-uid='child-${i}'
                  style={{ 
                    backgroundColor: '#aaaaaa33',
                    width: ${JSON.stringify(childW)}, 
                    height: ${JSON.stringify(childH)}, 
                    contain: 'layout' 
                  }}
                />`
        })
        .join('\n')}
    </div>
  </div>
  `)
}

async function convertParentToFlex(editor: EditorRenderResult) {
  const targetPath = appendNewElementPath(TestScenePath, ['a', 'parent'])
  await editor.dispatch([selectComponents([targetPath], false)], true)

  await expectSingleUndoStep(editor, () => clickOnPlusButton(editor))
}

async function clickOnPlusButton(editor: EditorRenderResult) {
  const plusButton = editor.renderedDOM.getByTestId(AddRemoveLayouSystemControlTestId())

  mouseClickAtPoint(plusButton, { x: 2, y: 2 })
}
