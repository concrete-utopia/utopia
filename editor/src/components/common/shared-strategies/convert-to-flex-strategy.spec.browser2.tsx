import { navigatorEntryToKey } from '../../../components/editor/store/editor-state'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import { shiftModifier } from '../../../utils/modifiers'
import {
  expectSingleUndo2Saves,
  selectComponentsForTest,
  setFeatureForBrowserTests,
} from '../../../utils/utils.test-utils'
import { getRegularNavigatorTargets } from '../../canvas/canvas-strategies/strategies/fragment-like-helpers.test-utils'
import { mouseClickAtPoint, pressKey } from '../../canvas/event-helpers.test-utils'
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

    await expectSingleUndo2Saves(editor, () => pressShiftA(editor))

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

    await expectSingleUndo2Saves(editor, () => pressShiftA(editor))

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
  it('adding flex layout to a container with fragment children present.', async () => {
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

    await expectSingleUndo2Saves(editor, () => pressShiftA(editor))

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div style={{ ...props.style }} data-uid='a'>
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
          <React.Fragment>
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
    </div>`),
    )
  })

  it('adding flex layout to a container with non-dom element children', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 30,
        top: 528,
        width: 624,
        height: 298,
      }}
      data-uid='container'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 52,
          top: 80,
          width: 87,
          height: 180,
        }}
        data-uid='91d'
      />
      <React.Fragment>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 181,
            top: 80,
            width: 63,
            height: 169,
          }}
          data-uid='21a'
        />
        <React.Fragment>
          <React.Fragment>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 303,
                top: 106,
                width: 36,
                height: 154,
              }}
              data-uid='d17'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 378,
                top: 125,
                width: 47,
                height: 106,
              }}
              data-uid='c9a'
            />
          </React.Fragment>
        </React.Fragment>
      </React.Fragment>
      {true ? (
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 449,
            top: 122,
            width: 121,
            height: 88,
          }}
          data-uid='9c4'
        />
      ) : null}
    </div>`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    await expectSingleUndo2Saves(editor, async () => {
      await pressKey('a', { modifiers: shiftModifier })
    })

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 30,
        top: 528,
        width: 'max-content',
        height: 'max-content',
        display: 'flex',
        flexDirection: 'row',
        gap: 41,
        padding: '80px 52px',
      }}
      data-uid='container'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 87,
          height: 180,
          contain: 'layout',
        }}
        data-uid='91d'
      />
      <React.Fragment>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 63,
            height: 169,
            contain: 'layout',
          }}
          data-uid='21a'
        />
        <React.Fragment>
          <React.Fragment>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 36,
                height: 154,
                contain: 'layout',
              }}
              data-uid='d17'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 47,
                height: 106,
                contain: 'layout',
              }}
              data-uid='c9a'
            />
          </React.Fragment>
        </React.Fragment>
      </React.Fragment>
      {true ? (
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 121,
            height: 88,
            contain: 'layout',
          }}
          data-uid='9c4'
        />
      ) : null}
      </div>`),
    )
  })

  it('reordering elements in the same group is allowed', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: -321,
        top: 354,
        width: 807,
        height: 283,
      }}
      data-uid='container'
    >
      <div
        style={{
          backgroundColor: '#ff0000',
          position: 'absolute',
          left: 381,
          top: 45,
          width: 45,
          height: 180,
        }}
        data-uid='first-child'
      />
      <div
        style={{
          backgroundColor: '#4285f4',
          position: 'absolute',
          left: 701,
          top: 61,
          width: 45,
          height: 180,
        }}
        data-uid='second-child'
      />
      <React.Fragment data-uid="fragment-1">
        <div
          style={{
            backgroundColor: '#fbcbb6',
            position: 'absolute',
            left: 203,
            top: 87,
            width: 65,
            height: 96,
          }}
          data-uid='fragment-1-child-1'
        />
        <div
          style={{
            backgroundColor: '#63ffeb',
            position: 'absolute',
            left: 102,
            top: 87,
            width: 40,
            height: 87,
          }}
          data-uid='fragment-1-child-2'
        />
      </React.Fragment>
      <React.Fragment data-uid="fragment-2">
        <div
          style={{
            backgroundColor: '#c393cd',
            position: 'absolute',
            left: 598,
            top: 104.5,
            width: 83,
            height: 97,
          }}
          data-uid='fragment-2-child-1'
        />
        <div
          style={{
            backgroundColor: '#91bbd3',
            position: 'absolute',
            left: 491.5,
            top: 82,
            width: 38,
            height: 100,
          }}
          data-uid='fragment-2-child-2'
        />
      </React.Fragment>
    </div>`),
      'await-first-dom-report',
    )

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    await expectSingleUndo2Saves(editor, async () => {
      await pressKey('a', { modifiers: shiftModifier })
    })

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: -321,
        top: 354,
        width: 'max-content',
        height: 'max-content',
        display: 'flex',
        flexDirection: 'row',
        gap: 65.6,
        padding: '42px 61px',
      }}
      data-uid='container'
    >
      <React.Fragment>
        <div
          style={{
            backgroundColor: '#fbcbb6',
            width: 65,
            height: 96,
            contain: 'layout',
          }}
          data-uid='fragment-1-child-1'
        />
        <div
          style={{
            backgroundColor: '#63ffeb',
            width: 40,
            height: 87,
            contain: 'layout',
          }}
          data-uid='fragment-1-child-2'
        />
      </React.Fragment>
      <div
        style={{
          backgroundColor: '#ff0000',
          width: 45,
          height: 180,
          contain: 'layout',
        }}
        data-uid='first-child'
      />
      <React.Fragment>
        <div
          style={{
            backgroundColor: '#c393cd',
            width: 83,
            height: 97,
            contain: 'layout',
          }}
          data-uid='fragment-2-child-1'
        />
        <div
          style={{
            backgroundColor: '#91bbd3',
            width: 38,
            height: 100,
            contain: 'layout',
          }}
          data-uid='fragment-2-child-2'
        />
      </React.Fragment>
      <div
        style={{
          backgroundColor: '#4285f4',
          width: 45,
          height: 180,
          contain: 'layout',
        }}
        data-uid='second-child'
      />
      </div>
  `),
    )

    expect(getRegularNavigatorTargets(editor)).toEqual([
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-1',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-1/fragment-1-child-1',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-1/fragment-1-child-2',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/first-child', // <- reordered here from the 1st place
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-2',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-2/fragment-2-child-1',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-2/fragment-2-child-2',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/second-child', // <- reordered here from the 2nd place
    ])
  })

  it('reordering elements with overlapping groups is not allowed', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: -321,
        top: 354,
        width: 477,
        height: 283,
      }}
      data-uid='container'
    >
      <React.Fragment data-uid='fragment-1'>
        <div
          style={{
            backgroundColor: '#ed72c7',
            position: 'absolute',
            left: 26,
            top: 56,
            width: 56,
            height: 171,
          }}
          data-uid='fragment-1-child-1'
        />
        <div
          style={{
            backgroundColor: '#c095ce',
            position: 'absolute',
            left: 297,
            top: 64,
            width: 30,
            height: 155,
          }}
          data-uid='fragment-1-child-2'
        />
      </React.Fragment>
      <React.Fragment data-uid='fragment-2'>
        <div
          style={{
            backgroundColor: '#8bc0d4',
            position: 'absolute',
            left: 132,
            top: 57.5,
            width: 44,
            height: 158,
          }}
          data-uid='fragment-2-child-1'
        />
        <div
          style={{
            backgroundColor: '#75d0d6',
            position: 'absolute',
            left: 395,
            top: 74.5,
            width: 58,
            height: 146,
          }}
          data-uid='fragment-2-child-2'
        />
      </React.Fragment>
      <div
        style={{
          backgroundColor: '#b9fdd8',
          position: 'absolute',
          left: 225.5,
          top: 25.5,
          width: 13,
          height: 232,
        }}
        data-uid='element'
      />
    </div>`),
      'await-first-dom-report',
    )

    const originalOrder: string[] = [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-1',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-1/fragment-1-child-1',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-1/fragment-1-child-2',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-2',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-2/fragment-2-child-1',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/fragment-2/fragment-2-child-2',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/element',
    ]

    expect(getRegularNavigatorTargets(editor)).toEqual(originalOrder)

    await selectComponentsForTest(editor, [
      EP.fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:container`),
    ])

    await expectSingleUndo2Saves(editor, async () => {
      await pressKey('a', { modifiers: shiftModifier })
    })

    expect(getRegularNavigatorTargets(editor)).toEqual(originalOrder)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: -321,
        top: 354,
        width: 'max-content',
        height: 'max-content',
        display: 'flex',
        flexDirection: 'row',
        gap: 56.5,
        padding: '56px 24px',
      }}
      data-uid='container'
    >
      <React.Fragment>
        <div
          style={{
            backgroundColor: '#ed72c7',
            width: 56,
            height: 171,
            contain: 'layout',
          }}
          data-uid='fragment-1-child-1'
        />
        <div
          style={{
            backgroundColor: '#c095ce',
            width: 30,
            height: 155,
            contain: 'layout',
          }}
          data-uid='fragment-1-child-2'
        />
      </React.Fragment>
      <React.Fragment>
        <div
          style={{
            backgroundColor: '#8bc0d4',
            width: 44,
            height: 158,
            contain: 'layout',
          }}
          data-uid='fragment-2-child-1'
        />
        <div
          style={{
            backgroundColor: '#75d0d6',
            width: 58,
            height: 146,
            contain: 'layout',
          }}
          data-uid='fragment-2-child-2'
        />
      </React.Fragment>
      <div
        style={{
          backgroundColor: '#b9fdd8',
          width: 13,
          height: 232,
          contain: 'layout',
        }}
        data-uid='element'
      />
    </div>`),
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

  await expectSingleUndo2Saves(editor, () => pressShiftA(editor))
}

async function pressShiftA(editor: EditorRenderResult) {
  await pressKey('A', { modifiers: shiftModifier })
}
