import { navigatorEntryToKey } from '../../../components/editor/store/editor-state'
import * as EP from '../../../core/shared/element-path'
import {
  expectNoAction,
  expectSingleUndo2Saves,
  selectComponentsForTest,
  setFeatureForBrowserTests,
} from '../../../utils/utils.test-utils'
import { mouseClickAtPoint } from '../../canvas/event-helpers.test-utils'
import {
  EditorRenderResult,
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../editor/actions/action-creators'
import { AddRemoveLayouSystemControlTestId } from '../../inspector/add-remove-layout-system-control'
import { FlexDirection } from '../../inspector/common/css-utils'
import { FlexAlignment, FlexJustifyContent, MaxContent } from '../../inspector/inspector-common'

type LTWH = [
  left: number,
  top: number,
  width: number | string,
  height: number | string,
  uid?: string,
]
type Size = [width: number, height: number, uid?: string]
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
  setFeatureForBrowserTests('Nine block control', true)

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
          flexDirection: 'row',
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
          flexDirection: 'row',
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
          flexDirection: 'column',
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

  it('single overflowing child does not make negative padding', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 100, 100],
      children: [[0, 0, 150, 50]],
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
          height: 'max-content',
          display: 'flex',
          flexDirection: 'column',
        }}
        data-uid='parent'
      >
        <div 
          data-uid='child-0'
          style={{
            backgroundColor: '#aaaaaa33', 
            width: 150, 
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

describe('Smart Convert to Flex Reordering Children if Needed', () => {
  setFeatureForBrowserTests('Nine block control', true)

  it('converts a horizontal layout with children out of order', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 500, 150],
      children: [
        [130, 0, 50, 50, 'c'],
        [65, 0, 50, 50, 'b'],
        [0, 0, 50, 50, 'a'],
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
          [50, 50, 'a'],
          [50, 50, 'b'],
          [50, 50, 'c'],
        ],
      }),
    )
  })

  it('reordering is disabled if non-jsx elements are among the children', async () => {
    const editor = await renderTestEditorWithCode(projectWithTextChild, 'await-first-dom-report')

    await selectComponentsForTest(editor, [EP.fromString('sb/parent')])

    const originalElementOrder = [
      'regular-sb/parent',
      'regular-sb/parent/first',
      'regular-sb/parent/second',
    ]

    expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      originalElementOrder,
    )

    await expectSingleUndo2Saves(editor, () => clickOnPlusButton(editor))

    expect(editor.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      originalElementOrder,
    )
  })
})

describe('Smart Convert to Flex alignItems', () => {
  setFeatureForBrowserTests('Nine block control', true)

  it('all elements aligned at the start become alignItems flex-start, but we omit that for simplicity', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 500, 150],
      children: [
        [0, 0, 50, 60],
        [65, 0, 50, 30],
        [130, 0, 50, 60],
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
          [50, 60],
          [50, 30],
          [50, 60],
        ],
      }),
    )
  })

  it('elements aligned at their center become alignItems center', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 500, 150],
      children: [
        [0, 0, 50, 60],
        [65, 15, 50, 30],
        [130, 0, 50, 60],
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
          alignItems: 'center',
        },
        children: [
          [50, 60],
          [50, 30],
          [50, 60],
        ],
      }),
    )
  })

  it('elements aligned at their bottom become alignItems flex-end', async () => {
    const editor = await renderProjectWith({
      parent: [50, 50, 500, 150],
      children: [
        [0, 0, 50, 60],
        [65, 30, 50, 30],
        [130, 0, 50, 60],
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
          alignItems: 'flex-end',
        },
        children: [
          [50, 60],
          [50, 30],
          [50, 60],
        ],
      }),
    )
  })
})

describe('Smart Convert to Flex Fragment Parents', () => {
  setFeatureForBrowserTests('Nine block control', true)

  it('converts a horizontal layout with zero padding and a gap of 15', async () => {
    const editor = await renderProjectWithFragmentParent({
      children: [
        [50, 50, 50, 50],
        [115, 50, 50, 50],
        [180, 50, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectAfterFragment({
        parent: {
          top: 50,
          left: 50,
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
    const editor = await renderProjectWithFragmentParent({
      children: [
        [50, 50, 50, 50],
        [50, 110, 50, 50],
        [50, 170, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectAfterFragment({
        parent: {
          top: 50,
          left: 50,
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
    const editor = await renderProjectWithFragmentParent({
      children: [
        [50, 50, 50, 50],
        [110, 110, 50, 50],
        [170, 170, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectAfterFragment({
        parent: {
          top: 50,
          left: 50,
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

  it('converts horizontal layout with gap of 15', async () => {
    const editor = await renderProjectWithFragmentParent({
      children: [
        [75, 50, 50, 50],
        [140, 50, 50, 50],
        [205, 50, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectAfterFragment({
        parent: {
          top: 50,
          left: 75,
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

  it('converts horizontal layout with 15px gap', async () => {
    const editor = await renderProjectWithFragmentParent({
      children: [
        [75, 70, 50, 50],
        [140, 70, 50, 50],
        [205, 70, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectAfterFragment({
        parent: {
          top: 70,
          left: 75,
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

  it('converts horizontal layout 2', async () => {
    const editor = await renderProjectWithFragmentParent({
      children: [
        [75, 50, 50, 50],
        [125, 50, 50, 50],
        [175, 50, 50, 50],
      ],
    })

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeReferenceProjectAfterFragment({
        parent: {
          top: 50,
          left: 75,
          width: MaxContent,
          height: MaxContent,
          display: 'flex',
          flexDirection: 'row',
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

describe('Smart Convert to Flex Fragment In Existing Flex', () => {
  setFeatureForBrowserTests('Nine block control', true)

  it('converts a fragment inside a flex layout to a flex child that is also a flex parent', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ ...props.style }} data-uid='a'>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 50,
            top: 200,
            width: 'max-content',
            height: 'max-content',
            display: 'flex',
            flexDirection: 'row',
            gap: 40,
            padding: '80px 30px',
          }}
          data-uid='parent'
        >
          <React.Fragment data-uid='fragment'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 47,
                height: 37,
                contain: 'layout',
              }}
              data-uid='aaa'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 43,
                height: 35,
                contain: 'layout',
              }}
              data-uid='bbb'
            />
          </React.Fragment>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 140,
              height: 120,
              contain: 'layout',
            }}
            data-uid='ccc'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 94,
              height: 110,
              contain: 'layout',
            }}
            data-uid='ddd'
          />
        </div>
      </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['a', 'parent', 'fragment'])
    await editor.dispatch([selectComponents([targetPath], false)], true)

    await expectSingleUndo2Saves(editor, () => clickOnPlusButton(editor))

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
    <div style={{ ...props.style }} data-uid='a'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 50,
          top: 200,
          width: 'max-content',
          height: 'max-content',
          display: 'flex',
          flexDirection: 'row',
          gap: 40,
          padding: '80px 30px',
        }}
        data-uid='parent'
      >
        <div
          data-uid='fragment'
          style={{
            contain: 'layout',
            width: 'max-content',
            height: 'max-content',
            display: 'flex',
            flexDirection: 'row',
            gap: 40,
          }}
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 47,
              height: 37,
              contain: 'layout',
            }}
            data-uid='aaa'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 43,
              height: 35,
              contain: 'layout',
            }}
            data-uid='bbb'
          />
        </div>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 140,
            height: 120,
            contain: 'layout',
          }}
          data-uid='ccc'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 94,
            height: 110,
            contain: 'layout',
          }}
          data-uid='ddd'
        />
      </div>
    </div>
    `),
    )
  })
})

describe('Smart Convert To Flex if Fragment Children', () => {
  it('for now, just refuse to convert if there are fragment children present.', async () => {
    const testProjectWithBadFragment = makeTestProjectCodeWithSnippet(`
    <div style={{ ...props.style }} data-uid='a'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 50,
          top: 200,
          width: 'max-content',
          height: 'max-content',
          display: 'flex',
          flexDirection: 'row',
          gap: 40,
          padding: '80px 30px',
        }}
        data-uid='parent'
      >
        <React.Fragment data-uid='fragment'>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 47,
              height: 37,
              contain: 'layout',
            }}
            data-uid='aaa'
          />
          <React.Fragment data-uid='bad-fragment'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 43,
                height: 35,
                contain: 'layout',
              }}
              data-uid='bbb'
            />
          </React.Fragment>
        </React.Fragment>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 140,
            height: 120,
            contain: 'layout',
          }}
          data-uid='ccc'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 94,
            height: 110,
            contain: 'layout',
          }}
          data-uid='ddd'
        />
      </div>
    </div>
    `)

    const editor = await renderTestEditorWithCode(
      testProjectWithBadFragment,
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['a', 'parent', 'fragment'])
    await editor.dispatch([selectComponents([targetPath], false)], true)

    await expectNoAction(editor, () => clickOnPlusButton(editor))

    // Expect that nothing changed
    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(testProjectWithBadFragment)
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
            const [childL, childT, childW, childH, maybeUid] = child
            const uid = `child-${maybeUid ?? i}`
            return `<div 
                data-uid='${uid}' 
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

function renderProjectWithFragmentParent(input: { children: Array<LTWH> }) {
  return renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(`
    <div style={{ ...props.style }} data-uid='a'>
      <React.Fragment data-uid='parent'>
        ${input.children
          .map((child, i) => {
            const [childL, childT, childW, childH, maybeUid] = child
            const uid = `child-${maybeUid ?? i}`
            return `<div 
                data-uid='${uid}' 
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
      </React.Fragment>
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
          const [childW, childH, maybeUid] = child
          const uid = `child-${maybeUid ?? i}`
          return `<div 
                  data-uid='${uid}'
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

function makeReferenceProjectAfterFragment(input: {
  parent: FlexProps
  children: Array<Size>
}): string {
  return makeTestProjectCodeWithSnippet(`
  <div style={{ ...props.style }} data-uid='a'>
    <div
      data-uid='parent'
      style={
        ${JSON.stringify({
          position: 'absolute',
          ...input.parent,
        })}
      }
    >
      ${input.children
        .map((child, i) => {
          const [childW, childH, maybeUid] = child
          const uid = `child-${maybeUid ?? i}`
          return `<div 
                  data-uid='${uid}'
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

const projectWithTextChild = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: -562,
        top: 159,
        width: 330,
        height: 530,
      }}
      data-uid='parent'
    >
      <div
        style={{
          backgroundColor: '#61ffe9',
          width: 144,
          height: 174,
          contain: 'layout',
          left: 6,
          top: 246,
          position: 'absolute',
        }}
        data-uid='first'
      >
        first
      </div>
      hello
      <div
        style={{
          backgroundColor: '#d089cc',
          width: 150,
          height: 186,
          contain: 'layout',
          left: 0,
          top: 44,
          position: 'absolute',
        }}
        data-uid='second'
      >
        second
      </div>
    </div>
  </Storyboard>
)
`

async function convertParentToFlex(editor: EditorRenderResult) {
  const targetPath = EP.appendNewElementPath(TestScenePath, ['a', 'parent'])
  await editor.dispatch([selectComponents([targetPath], false)], true)

  await expectSingleUndo2Saves(editor, () => clickOnPlusButton(editor))
}

async function clickOnPlusButton(editor: EditorRenderResult) {
  const plusButton = editor.renderedDOM.getByTestId(AddRemoveLayouSystemControlTestId())

  await mouseClickAtPoint(plusButton, { x: 2, y: 2 })
}
