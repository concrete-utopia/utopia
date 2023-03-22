import * as EP from '../../core/shared/element-path'
import { assertNever } from '../../core/shared/utils'
import { selectComponentsForTest, setFeatureForBrowserTests } from '../../utils/utils.test-utils'
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
        width: 299,
        height: 343,
        position: 'absolute',
        top: 106,
        left: 136,
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
          left: 136,
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
          left: 371,
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
  setFeatureForBrowserTests('Fragment support', true)

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

  it('toggle from a sized div to a sizeless div, nested in a fragment', async () => {
    const editor = await renderTestEditorWithCode(
      nestedGroupsWithWrapperType('fragment', 'div'),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [EP.fromString('sb/outer-group/group')])

    await chooseWrapperType(editor, 'div', 'sizeless-div')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <React.Fragment>
      <div data-uid='group'>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 111,
            top: 11,
            width: 157,
            height: 112,
          }}
          data-uid='f64'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 318,
            top: 52,
            width: 139,
            height: 138,
          }}
          data-uid='978'
        />
      </div>
    </React.Fragment>
  </Storyboard>
)
`)
  })

  it('toggle from a sized div to a fragment, nested in a fragment', async () => {
    const editor = await renderTestEditorWithCode(
      nestedGroupsWithWrapperType('fragment', 'div'),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [EP.fromString('sb/outer-group/group')])

    await chooseWrapperType(editor, 'div', 'fragment')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <React.Fragment>
      <React.Fragment>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 111,
            top: 11,
            width: 157,
            height: 112,
          }}
          data-uid='f64'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 318,
            top: 52,
            width: 139,
            height: 138,
          }}
          data-uid='978'
        />
      </React.Fragment>
    </React.Fragment>
  </Storyboard>
)
`)
  })

  it('toggle from a fragment to a sized div, nested in a sized div', async () => {
    const editor = await renderTestEditorWithCode(
      nestedGroupsWithWrapperType('div', 'fragment'),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [EP.fromString('sb/outer-group/group')])

    expect(editor.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'sb/outer-group/group',
    ])

    await chooseWrapperType(editor, 'fragment', 'div')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='outer-group'
      style={{
        width: 346,
        height: 179,
        position: 'absolute',
        top: 11,
        left: 111,
      }}
    >
      <div
        data-uid='group'
        style={{ width: 346, height: 179 }}
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 0,
            top: 0,
            width: 157,
            height: 112,
          }}
          data-uid='f64'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 207,
            top: 41,
            width: 139,
            height: 138,
          }}
          data-uid='978'
        />
      </div>
    </div>
  </Storyboard>
)
`)
  })

  it('toggle from a group into a sized div in a flow context', async () => {
    const editor = await renderTestEditorWithCode(projectWithGroupInFlow, 'await-first-dom-report')

    await selectComponentsForTest(editor, [EP.fromString('sb/flow/group')])

    await chooseWrapperType(editor, 'sizeless-div', 'div')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 230,
        top: 448,
        width: 373,
        height: 442,
      }}
      data-uid='flow'
    >
      <div
        data-uid='group'
        style={{ width: 78, height: 225 }}
      >
        <div
          style={{
            backgroundColor: '#007aff',
            width: 78,
            height: 134,
            contain: 'layout',
          }}
          data-uid='767'
        />
        <div
          style={{
            backgroundColor: '#679bd2',
            width: 68,
            height: 91,
            contain: 'layout',
          }}
          data-uid='118'
        />
      </div>
      <div
        style={{
          backgroundColor: '#ff00ff',
          width: 66,
          height: 88,
          contain: 'layout',
        }}
        data-uid='7f2'
      />
    </div>
  </Storyboard>
)
`)
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

function nestedGroupsWithWrapperType(outerWrapperType: WrapperType, innerWrapperType: WrapperType) {
  const openingTag = (wrapperType: WrapperType, uid: string) => {
    switch (wrapperType) {
      case 'div':
        return `<div
          data-uid='${uid}'
          style={{
            width: 346,
            height: 179,
            position: 'absolute',
            top: 11,
            left: 111,
          }}
      >`
      case 'fragment':
        return `<React.Fragment data-uid='${uid}'>`
      case 'sizeless-div':
        return `<div data-uid='${uid}'>`
      default:
        assertNever(wrapperType)
    }
  }

  const closingTag = (wrapperType: WrapperType) => {
    switch (wrapperType) {
      case 'div':
      case 'sizeless-div':
        return '</div>'
      case 'fragment':
        return '</React.Fragment>'
      default:
        assertNever(wrapperType)
    }
  }

  return `import * as React from 'react'
  import { Storyboard } from 'utopia-api'
  
  export var storyboard = (
    <Storyboard data-uid='sb'>
      ${openingTag(outerWrapperType, 'outer-group')}
        ${openingTag(innerWrapperType, 'group')}
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 0,
              top: 0,
              width: 157,
              height: 112,
            }}
            data-uid='f64'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 207,
              top: 41,
              width: 139,
              height: 138,
            }}
            data-uid='978'
          />
        ${closingTag(innerWrapperType)}
      ${closingTag(outerWrapperType)}
    </Storyboard>
  )
  `
}

const projectWithGroupInFlow = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 230,
        top: 448,
        width: 373,
        height: 442,
      }}
      data-uid='flow'
    >
      <div data-uid='group'>
        <div
          style={{
            backgroundColor: '#007aff',
            width: 78,
            height: 134,
            contain: 'layout',
          }}
          data-uid='767'
        />
        <div
          style={{
            backgroundColor: '#679bd2',
            width: 68,
            height: 91,
            contain: 'layout',
          }}
          data-uid='118'
        />
      </div>
      <div
        style={{
          backgroundColor: '#ff00ff',
          width: 66,
          height: 88,
          contain: 'layout',
        }}
        data-uid='7f2'
      />
    </div>
  </Storyboard>
)
`
