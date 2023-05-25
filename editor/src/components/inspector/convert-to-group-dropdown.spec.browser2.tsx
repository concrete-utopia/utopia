import * as EP from '../../core/shared/element-path'
import { assertNever } from '../../core/shared/utils'
import { selectComponentsForTest } from '../../utils/utils.test-utils'
import { EditorContract } from '../canvas/canvas-strategies/strategies/contracts/contract-helpers'
import { mouseClickAtPoint } from '../canvas/event-helpers.test-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  renderTestEditorWithCode,
} from '../canvas/ui-jsx.test-utils'
import { groupSectionOption } from './convert-to-group-dropdown'

const projectWithSizedDiv = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='group'
      style={{
        position: 'absolute',
        top: 106,
        left: 136,
        width: 299,
        height: 343,
      }}
    >
      <div
        style={{
          backgroundColor: '#267f99',
          position: 'absolute',
          top: 0,
          left: 0,
          width: 196,
          height: 148,
        }}
        data-uid='6c3'
      />
      <div
        style={{
          backgroundColor: '#1a1aa8',
          position: 'absolute',
          top: 95,
          left: 235,
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
          top: 106,
          left: 136,
          width: 196,
          height: 148,
        }}
        data-uid='6c3'
      />
      <div
        style={{
          backgroundColor: '#1a1aa8',
          position: 'absolute',
          top: 201,
          left: 371,
          width: 64,
          height: 248,
        }}
        data-uid='15d'
      />
    </div>
  </Storyboard>
)
`

const projectWithFragment = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <React.Fragment>
      <div
        style={{
          backgroundColor: '#267f99',
          position: 'absolute',
          top: 106,
          left: 136,
          width: 196,
          height: 148,
        }}
        data-uid='6c3'
      />
      <div
        style={{
          backgroundColor: '#1a1aa8',
          position: 'absolute',
          top: 201,
          left: 371,
          width: 64,
          height: 248,
        }}
        data-uid='15d'
      />
    </React.Fragment>
  </Storyboard>
)
`

describe('Group section', () => {
  it('toggle from a sized div to a fragment, nested in a fragment', async () => {
    const editor = await renderTestEditorWithCode(
      nestedGroupsWithWrapperType('fragment', 'frame'),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [EP.fromString('sb/outer-group/group')])

    await chooseWrapperType(editor, 'frame', 'fragment')
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
            top: 11,
            left: 111,
            width: 157,
            height: 112,
          }}
          data-uid='f64'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            top: 52,
            left: 318,
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

  it('toggle from a sizeless div to a fragment', async () => {
    const editor = await renderTestEditorWithCode(projectWithSizelessDiv, 'await-first-dom-report')
    await selectComponentsForTest(editor, [EP.fromString('sb/group')])

    await chooseWrapperType(editor, 'frame', 'fragment')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(projectWithFragment)

    // then convert to a sized div!
    await chooseWrapperType(editor, 'fragment', 'frame')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(projectWithSizedDiv)
  })

  it('toggle from a fragment to a sized div, nested in a fragment', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
      import { Storyboard } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <React.Fragment data-uid='outer-group'>
            <React.Fragment data-uid='group'>
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  top: 423,
                  left: 591,
                  width: 157,
                  height: 112,
                }}
                data-uid='f64'
              />
              <div
                style={{
                  backgroundColor: '#aaaaaa33',
                  position: 'absolute',
                  top: 464,
                  left: 798,
                  width: 139,
                  height: 138,
                }}
                data-uid='978'
              />
            </React.Fragment>
          </React.Fragment>
        </Storyboard>
      )
      `,
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [EP.fromString('sb/outer-group/group')])

    await chooseWrapperType(editor, 'fragment', 'frame')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <React.Fragment>
      <div
        data-uid='group'
        style={{
          position: 'absolute',
          top: 423,
          left: 591,
          width: 346,
          height: 179,
        }}
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            top: 0,
            left: 0,
            width: 157,
            height: 112,
          }}
          data-uid='f64'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            top: 41,
            left: 207,
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

  it('toggle from a fragment to a sized div, nested in a sized div', async () => {
    const editor = await renderTestEditorWithCode(
      nestedGroupsWithWrapperType('frame', 'fragment'),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [EP.fromString('sb/outer-group/group')])

    expect(editor.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'sb/outer-group/group',
    ])

    await chooseWrapperType(editor, 'fragment', 'frame')
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(`import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='outer-group'
      style={{
        position: 'absolute',
        top: 11,
        left: 111,
        width: 346,
        height: 179,
      }}
    >
      <div
        data-uid='group'
        style={{
          position: 'absolute',
          top: 0,
          left: 0,
          width: 346,
          height: 179,
        }}
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            top: 0,
            left: 0,
            width: 157,
            height: 112,
          }}
          data-uid='f64'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            top: 41,
            left: 207,
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

  it("toggle from fragment to frame doesn't work if the fragment has a static child", async () => {
    const startingCode = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <React.Fragment data-uid='outer-group'>
      <React.Fragment data-uid='group'>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            // position: 'absolute', // <--------- notice that this child is not position: absolute!
            top: 423,
            left: 591,
            width: 157,
            height: 112,
          }}
          data-uid='f64'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            top: 464,
            left: 798,
            width: 139,
            height: 138,
          }}
          data-uid='978'
        />
      </React.Fragment>
    </React.Fragment>
  </Storyboard>
)
`

    const editor = await renderTestEditorWithCode(startingCode, 'await-first-dom-report')

    await selectComponentsForTest(editor, [EP.fromString('sb/outer-group/group')])

    await chooseWrapperType(editor, 'fragment', 'frame')

    // nothing happened!
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(startingCode)
  })

  it("toggle from frame to fragment doesn't work if the frame has a static child", async () => {
    const startingCode = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <React.Fragment data-uid='outer-group'>
      <div
        data-uid='group'
        style={{
          position: 'absolute',
          top: 106,
          left: 136,
          width: 299,
          height: 343,
        }}
      >
        <div
          style={{
            backgroundColor: '#267f99',
            // position: 'absolute', // <--------- notice that this child is not position: absolute!
            top: 0,
            left: 0,
            width: 196,
            height: 148,
          }}
          data-uid='6c3'
        />
        <div
          style={{
            backgroundColor: '#1a1aa8',
            position: 'absolute',
            top: 95,
            left: 235,
            width: 64,
            height: 248,
          }}
          data-uid='15d'
        />
      </div>
    </React.Fragment>
  </Storyboard>
)
`

    const editor = await renderTestEditorWithCode(startingCode, 'await-first-dom-report')

    await selectComponentsForTest(editor, [EP.fromString('sb/outer-group/group')])

    await chooseWrapperType(editor, 'frame', 'fragment')

    // nothing happened!
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(startingCode)
  })
})

async function chooseWrapperType(
  editor: EditorRenderResult,
  fromWrapperType: EditorContract,
  toWrapperType: EditorContract,
) {
  const divLabel = groupSectionOption(fromWrapperType).label!
  const groupDropDown = editor.renderedDOM.getAllByText(divLabel).at(-1)!
  await mouseClickAtPoint(groupDropDown, { x: 2, y: 2 })

  const wrapperLabel = groupSectionOption(toWrapperType).label!
  const optionElement = editor.renderedDOM.getAllByText(wrapperLabel).at(-1)!
  await mouseClickAtPoint(optionElement, { x: 2, y: 2 })
}

function nestedGroupsWithWrapperType(
  outerWrapperType: EditorContract,
  innerWrapperType: EditorContract,
) {
  const openingTag = (wrapperType: EditorContract, uid: string) => {
    switch (wrapperType) {
      case 'frame':
        return `<div
          data-uid='${uid}'
          style={{
            position: 'absolute',
            top: 11,
            left: 111,
            width: 346,
            height: 179,
          }}
      >`
      case 'fragment':
        return `<React.Fragment data-uid='${uid}'>`
      default:
        assertNever(wrapperType)
    }
  }

  const closingTag = (wrapperType: EditorContract) => {
    switch (wrapperType) {
      case 'frame':
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
              top: 0,
              left: 0,
              width: 157,
              height: 112,
            }}
            data-uid='f64'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              top: 41,
              left: 207,
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
