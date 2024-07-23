import { navigatorEntryToKey } from '../../../components/editor/store/editor-state'
import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import { shiftModifier } from '../../../utils/modifiers'
import { expectSingleUndo2Saves, selectComponentsForTest } from '../../../utils/utils.test-utils'
import { getRegularNavigatorTargets } from '../../canvas/canvas-strategies/strategies/fragment-like-helpers.test-utils'
import { pressKey } from '../../canvas/event-helpers.test-utils'
import type { EditorRenderResult } from '../../canvas/ui-jsx.test-utils'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../canvas/ui-jsx.test-utils'
import { selectComponents } from '../../editor/actions/action-creators'
import type { FlexDirection } from '../../inspector/common/css-utils'
import type { FlexAlignment, FlexJustifyContent } from '../../inspector/inspector-common'
import { MaxContent } from '../../inspector/inspector-common'
import { getNavigatorTargetsFromEditorState } from '../../navigator/navigator-utils'

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

  it('can convert zero-sized element with absolute children into a flex layout', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      height: '100%',
      width: '100%',
      contain: 'layout',
    }}
    data-uid='root'
  >
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
      }}
      data-uid='zero-sized'
    >
      <img
        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
        alt='Utopia logo'
        style={{
          width: 51,
          height: 65,
          position: 'absolute',
          left: 15,
          top: 16,
        }}
        data-testid='first-child'
        data-uid='first-child'
      />
      <img
        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
        alt='Utopia logo'
        style={{
          width: 51,
          height: 65,
          position: 'absolute',
          left: 79,
          top: 16,
        }}
        data-uid='second-child'
      />
      <img
        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
        alt='Utopia logo'
        style={{
          width: 51,
          height: 65,
          position: 'absolute',
          left: 143,
          top: 16,
        }}
        data-uid='third-child'
      />
    </div>
  </div>`),
      'await-first-dom-report',
    )

    const { top: firstChildTopBeforeFlexConversion, left: firstChildLeftBeforeFlexConversion } =
      renderResult.renderedDOM.getByTestId('first-child').style

    const containerPath = EP.appendNewElementPath(TestScenePath, ['root', 'zero-sized'])
    await selectComponentsForTest(renderResult, [containerPath])
    await expectSingleUndo2Saves(renderResult, () => pressShiftA(renderResult))

    const { top: containerTop, left: containerLeft } =
      renderResult.getEditorState().editor.allElementProps[EP.toString(containerPath)]['style']

    expect({
      top: firstChildTopBeforeFlexConversion,
      left: firstChildLeftBeforeFlexConversion,
    }).toEqual({
      top: `${containerTop}px`,
      left: `${containerLeft}px`,
    })

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div
    style={{
      height: '100%',
      width: '100%',
      contain: 'layout',
    }}
    data-uid='root'
  >
    <div
      data-uid='zero-sized'
      style={{
        position: 'absolute',
        top: 16,
        left: 15,
        width: 'max-content',
        height: 'max-content',
        display: 'flex',
        flexDirection: 'row',
        gap: 13,
      }}
    >
      <img
        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
        alt='Utopia logo'
        style={{ width: 51, height: 65 }}
        data-testid='first-child'
        data-uid='first-child'
      />
      <img
        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
        alt='Utopia logo'
        style={{ width: 51, height: 65 }}
        data-uid='second-child'
      />
      <img
        src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
        alt='Utopia logo'
        style={{ width: 51, height: 65 }}
        data-uid='third-child'
      />
    </div>
  </div>
`),
    )
  })

  it('converts groups correctly', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `
			<div style={{ ...props.style }} data-uid='a'>
				<Group data-uid='parent' style={{ position: 'absolute', left: 0, top: 0, width: 349, height: 239 }}>
					<div data-uid='foo' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 0, top: 0, width: 159, height: 131 }} />
					<div data-uid='bar' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 190, top: 108, width: 159, height: 131 }} />
				</Group>
			</div>
	  `,
      ),

      'await-first-dom-report',
    )

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
		<div style={{ ...props.style }} data-uid='a'>
			<div data-uid='parent' style={{ position: 'absolute', top: 0, left: 0, width: 'max-content', height: 'max-content', display: 'flex', flexDirection: 'row', gap: 31 }}>
				<div data-uid='foo' style={{ backgroundColor: '#aaaaaa33', width: 159, height: 131, contain: 'layout' }} />
				<div data-uid='bar' style={{ backgroundColor: '#aaaaaa33', width: 159, height: 131, contain: 'layout' }} />
			</div>
		</div>
	`),
    )
  })

  it('converts groups with constraints that imply a certain row-like layout', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(
        `
			<div style={{ ...props.style }} data-uid='a'>
        <Group
          data-uid='parent'
          style={{
            height: 204,
            position: 'absolute',
            left: 14,
            top: 343,
            width: 584,
          }}
        >
          <span
            data-uid='second'
            style={{
              wordBreak: 'break-word',
              fontSize: '31px',
              height: 119,
              position: 'absolute',
              left: 91,
              top: 29,
              width: 'max-content',
            }}
            data-constraints={['left', 'right']}
          >
            Hello World
          </span>
          <div
            data-uid='third'
            style={{
              backgroundColor: '#FF69B4AB',
              height: 204,
              contain: 'layout',
              width: 119,
              position: 'absolute',
              left: 465,
              top: 0,
            }}
            data-constraints={['right', 'width']}
          />
          <div
            data-uid='first'
            style={{
              backgroundColor: '#FF69B4AB',
              height: 204,
              contain: 'layout',
              width: 59,
              position: 'absolute',
              left: 0,
              top: 0,
            }}
            data-constraints={['left', 'width']}
          />
        </Group>
			</div>
	  `,
      ),

      'await-first-dom-report',
    )

    await convertParentToFlex(editor)

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
		<div style={{ ...props.style }} data-uid='a'>
      <div
        data-uid='parent'
        style={{
          height: 204,
          position: 'absolute',
          left: 14,
          top: 343,
          width: 584,
          alignItems: 'center',
          justifyContent: 'flex-start',
          display: 'flex',
          flexDirection: 'row',
          gap: 32,
        }}
      >
        <div
          data-uid='first'
          style={{
            backgroundColor: '#FF69B4AB',
            height: 204,
            contain: 'layout',
            width: 59,
            flexGrow: 0,
          }}
        />
        <span
          data-uid='second'
          style={{
            wordBreak: 'break-word',
            fontSize: '31px',
            height: 119,
            width: 'max-content',
            flexGrow: 1,
          }}
        >
          Hello World
        </span>
        <div
          data-uid='third'
          style={{
            backgroundColor: '#FF69B4AB',
            height: 204,
            contain: 'layout',
            width: 119,
            flexGrow: 0,
          }}
        />
      </div>
		</div>
	`),
    )
  })
})

describe('Smart Convert to Flex Reordering Children if Needed', () => {
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

    expect(
      getNavigatorTargetsFromEditorState(editor.getEditorState().editor).navigatorTargets.map(
        navigatorEntryToKey,
      ),
    ).toEqual(originalElementOrder)

    await expectSingleUndo2Saves(editor, () => pressShiftA(editor))

    expect(
      getNavigatorTargetsFromEditorState(editor.getEditorState().editor).navigatorTargets.map(
        navigatorEntryToKey,
      ),
    ).toEqual(originalElementOrder)
  })
})

describe('Smart Convert to Flex alignItems', () => {
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

describe('Smart Convert To Flex if Fragment Children', () => {
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
        alignItems: 'center',
        justifyContent: 'flex-start',
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
        alignItems: 'center',
        justifyContent: 'flex-start',
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
        alignItems: 'center',
        justifyContent: 'flex-start',
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

describe('Smart convert to flex centered layout', () => {
  it('adds centered layout to a single span child', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 334,
          top: -215,
          width: 165,
          height: 122,
        }}
        data-uid='a'
      >
        <span
          style={{
            position: 'absolute',
            wordBreak: 'break-word',
            left: 51,
            top: 73,
            fontSize: 15,
            width: 'max-content',
            height: 'max-content',
          }}
          data-testid='text'
          data-uid='text'
        >
          This will be centered
        </span>
      </div>
      `),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['a'])
    await selectComponentsForTest(editor, [targetPath])

    await expectSingleUndo2Saves(editor, () => pressShiftA(editor))

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 334,
        top: -215,
        width: 'max-content',
        height: 'max-content',
        display: 'flex',
        flexDirection: 'row',
        padding: '32px 0',
        alignItems: 'center',
        justifyContent: 'center',
      }}
      data-uid='a'
    >
      <span
        style={{
          wordBreak: 'break-word',
          fontSize: 15,
          width: 'max-content',
          height: 'max-content',
        }}
        data-testid='text'
        data-uid='text'
      >
        This will be centered
      </span>
    </div>
    `),
    )
  })
  it('adds centered layout if children are clustered around the x axis', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div style={{ ...props.style }} data-uid='root'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 52,
        top: 61,
        width: 557,
        height: 155,
      }}
      data-testid='container'
      data-uid='container'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 17,
          top: 67,
          width: 50,
          height: 50,
        }}
        data-uid='c32'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 99,
          top: 42,
          width: 50,
          height: 50,
        }}
        data-uid='aa1'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 184,
          top: 78,
          width: 50,
          height: 50,
        }}
        data-uid='4e1'
      />
    </div>
  </div>`),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['root', 'container'])
    await selectComponentsForTest(editor, [targetPath])

    await expectSingleUndo2Saves(editor, () => pressShiftA(editor))

    const { alignItems, justifyContent, flexDirection } =
      editor.renderedDOM.getByTestId('container').style
    expect({ alignItems, justifyContent, flexDirection }).toEqual({
      alignItems: 'center',
      justifyContent: 'flex-start',
      flexDirection: 'row',
    })

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div style={{ ...props.style }} data-uid='root'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 52,
        top: 61,
        width: 'max-content',
        height: 'max-content',
        display: 'flex',
        flexDirection: 'row',
        gap: 33.5,
        padding: '27px 17px',
        alignItems: 'center',
        justifyContent: 'flex-start',
      }}
      data-testid='container'
      data-uid='container'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 50,
          height: 50,
          contain: 'layout',
        }}
        data-uid='c32'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 50,
          height: 50,
          contain: 'layout',
        }}
        data-uid='aa1'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 50,
          height: 50,
          contain: 'layout',
        }}
        data-uid='4e1'
      />
    </div>
  </div>`),
    )
  })
  it('adds centered layout if children are clustered around the y axis', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div style={{ ...props.style }} data-uid='root'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 47,
          top: 15,
          width: 155,
          height: 394,
        }}
        data-testid='container'
        data-uid='container'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 33,
            top: 28,
            width: 50,
            height: 50,
          }}
          data-uid='9bd'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 58,
            top: 142,
            width: 50,
            height: 50,
          }}
          data-uid='1ff'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 28,
            top: 223,
            width: 50,
            height: 50,
          }}
          data-uid='0f3'
        />
      </div>
    </div>`),
      'await-first-dom-report',
    )

    const targetPath = EP.appendNewElementPath(TestScenePath, ['root', 'container'])
    await selectComponentsForTest(editor, [targetPath])

    await expectSingleUndo2Saves(editor, () => pressShiftA(editor))

    const { alignItems, justifyContent, flexDirection } =
      editor.renderedDOM.getByTestId('container').style
    expect({ alignItems, justifyContent, flexDirection }).toEqual({
      alignItems: 'center',
      justifyContent: 'flex-start',
      flexDirection: 'column',
    })

    expect(getPrintedUiJsCode(editor.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`<div style={{ ...props.style }} data-uid='root'><div
    style={{
      backgroundColor: '#aaaaaa33',
      position: 'absolute',
      left: 47,
      top: 15,
      width: 'max-content',
      height: 'max-content',
      display: 'flex',
      flexDirection: 'column',
      gap: 47.5,
      padding: '28px 33px',
      alignItems: 'center',
      justifyContent: 'flex-start',
    }}
    data-testid='container'
    data-uid='container'
  >
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        width: 50,
        height: 50,
        contain: 'layout',
      }}
      data-uid='9bd'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        width: 50,
        height: 50,
        contain: 'layout',
      }}
      data-uid='1ff'
    />
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        width: 50,
        height: 50,
        contain: 'layout',
      }}
      data-uid='0f3'
    />
  </div>
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
