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
import { FlexAlignment, FlexJustifyContent, MaxContent } from '../../inspector/inspector-common'

type LTWH = [left: number, top: number, width: number, height: number]
type Size = [width: number, height: number]
type FlexProps = {
  left: number
  top: number
  padding: number
  gap: number
  alignItems: FlexAlignment
  justifyContent: FlexJustifyContent
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

  it('converts an align-start layout with zero padding and a gap of 15', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 500, 150],
      children: [
        [0, 0, 50, 50],
        [65, 0, 50, 50],
        [130, 0, 50, 50],
      ],
    })

    const targetPath = appendNewElementPath(TestScenePath, ['a', 'parent'])
    await editor.dispatch([selectComponents([targetPath], false)], true)

    await expectSingleUndoStep(editor, () => clickOnPlusButton(editor))

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectWith({
        parent: {
          left: 50,
          top: 50,
          padding: 0,
          gap: 15,
          alignItems: 'flex-start',
          justifyContent: 'center',
        },
        children: [
          [50, 50],
          [50, 50],
          [50, 50],
        ],
      }),
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
            return `<div data-uid='child-${i}' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${childL}, top: ${childT}, width: ${childW}, height: ${childH} }} />`
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
          width: MaxContent,
          height: MaxContent,
          display: 'flex',
        })}
      }
      data-uid='parent'
    >
      ${input.children
        .map((child, i) => {
          const [childW, childH] = child
          return `<div data-uid='child-${i}' style={{ backgroundColor: '#aaaaaa33', width: ${childW}, height: ${childH}, contain: 'layout' }} />`
        })
        .join('\n')}
    </div>
  </div>
  `)
}

async function clickOnPlusButton(editor: EditorRenderResult) {
  const plusButton = editor.renderedDOM.getByTestId(AddRemoveLayouSystemControlTestId())

  mouseClickAtPoint(plusButton, { x: 2, y: 2 })
}
