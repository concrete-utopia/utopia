import * as EP from '../../core/shared/element-path'
import { selectComponentsForTest, wait } from '../../utils/utils.test-utils'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { groupSectionOption, WrapperType } from '../inspector/group-section'

const projectWithSizedDiv = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='group'
      style={{
        position: 'absolute',
        top: 106,
        left: 135.5,
        width: 299,
        height: 343,
      }}
    >
      <div
        style={{
          backgroundColor: '#267f99',
          position: 'absolute',
          left: 0,
          top: 0,
          width: 196,
          height: 148,
        }}
        data-uid='6c3'
      />
      <div
        style={{
          backgroundColor: '#1a1aa8',
          position: 'absolute',
          left: 235,
          top: 95,
          width: 64,
          height: 248,
        }}
        data-uid='15d'
      />
    </div>
  </Storyboard>
)
`

const projectWithSizelessDiv = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div data-uid='group'>
      <div
        style={{
          backgroundColor: '#267f99',
          position: 'absolute',
          left: 135.5,
          top: 106,
          width: 196,
          height: 148,
        }}
        data-uid='6c3'
      />
      <div
        style={{
          backgroundColor: '#1a1aa8',
          position: 'absolute',
          left: 370.5,
          top: 201,
          width: 64,
          height: 248,
        }}
        data-uid='15d'
      />
    </div>
  </Storyboard>
)
`

describe('Group section', () => {
  it('toggle from a sized div to a sizeless div', async () => {
    const editor = await renderTestEditorWithCode(projectWithSizedDiv, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/group')])

    await chooseWrapperType(editor, 'div', 'sizeless-div')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(projectWithSizelessDiv)
  })

  it('toggle from a sizeless div to a sized div', async () => {
    const editor = await renderTestEditorWithCode(projectWithSizelessDiv, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/group')])

    await chooseWrapperType(editor, 'sizeless-div', 'div')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(projectWithSizedDiv)
  })
})

async function chooseWrapperType(
  editor: EditorRenderResult,
  fromWrapperType: WrapperType,
  toWrapperType: WrapperType,
) {
  const divLabel = groupSectionOption(fromWrapperType).label!
  const groupDropDown = editor.renderedDOM.getAllByText(divLabel).at(-1)!
  await mouseClickAtPoint(groupDropDown, { x: 2, y: 2 })

  const wrapperLabel = groupSectionOption(toWrapperType).label!
  const optionElement = editor.renderedDOM.getAllByText(wrapperLabel).at(-1)!
  await mouseClickAtPoint(optionElement, { x: 2, y: 2 })
}
