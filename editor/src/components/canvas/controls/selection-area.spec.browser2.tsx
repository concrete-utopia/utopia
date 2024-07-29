import { BakedInStoryboardVariableName } from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import { DefaultNavigatorWidth } from '../../editor/store/editor-state'
import { CanvasControlsContainerID } from './new-canvas-controls'
import { mouseDragFromPointToPoint } from '../event-helpers.test-utils'
import { renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { toggleHidden } from '../../editor/actions/action-creators'
import { shiftModifier } from '../../../utils/modifiers'
import { selectComponents } from '../../editor/actions/meta-actions'

describe('Selection area', () => {
  it('can select an element on the storyboard', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <div data-uid='root'>
            <div
                data-uid='foo'
                style={{
                    background: "red",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 100,
                    left: 100,
                }}
            />
            <div
                data-uid='bar'
                style={{
                    background: "blue",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 200,
                    left: 120,
                }}
            />
        </div>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + DefaultNavigatorWidth + 100, y: rect.y + 100 },
      { x: rect.x + DefaultNavigatorWidth + 300, y: rect.y + 200 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/foo',
    ])
  })
  it('can select zero-sized elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <div data-uid='root'>
            <div
                data-uid='foo'
                style={{
                    background: "red",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 100,
                    left: 100,
                }}
            />
            <div
                data-uid='bar'
                style={{
                    position: "absolute",
                    top: 200,
                    left: 120,
                    width: 0,
                    height: 0,
                }}
            />
        </div>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + DefaultNavigatorWidth + 150, y: rect.y + 230 },
      { x: rect.x + DefaultNavigatorWidth + 300, y: rect.y + 350 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/bar',
    ])
  })
  it('can select multiple elements on the storyboard', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <div data-uid='root'>
            <div
                data-uid='foo'
                style={{
                    background: "red",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 100,
                    left: 100,
                }}
            />
            <div
                data-uid='bar'
                style={{
                    background: "blue",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 200,
                    left: 120,
                }}
            />
        </div>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + DefaultNavigatorWidth + 100, y: rect.y + 100 },
      { x: rect.x + DefaultNavigatorWidth + 300, y: rect.y + 310 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/bar',
      'root/foo',
    ])
  })
  it('ignores hidden elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <div data-uid='root'>
            <div
                data-uid='foo'
                style={{
                    background: "red",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 100,
                    left: 100,
                }}
            />
            <div
                data-uid='bar'
                style={{
                    background: "blue",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 200,
                    left: 120,
                }}
            />
        </div>
    )
}
`,
      'await-first-dom-report',
    )
    await renderResult.dispatch([toggleHidden([EP.fromString('root/bar')])], true)
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + DefaultNavigatorWidth + 100, y: rect.y + 100 },
      { x: rect.x + DefaultNavigatorWidth + 300, y: rect.y + 310 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/foo',
    ])
  })
  it('only selects the outermost child', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <div data-uid='root'>
            <div
                data-uid='foo'
                style={{
                    background: "red",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 100,
                    left: 100,
                }}
            >
                <div data-uid='child1' style={{ width: 20, height: 20, background: "orange" }} />
                <div data-uid='child2' style={{ width: 20, height: 20, background: "yellow" }} />
            </div>
            <div
                data-uid='bar'
                style={{
                    background: "blue",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 200,
                    left: 120,
                }}
            />
        </div>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + DefaultNavigatorWidth + 100, y: rect.y + 100 },
      { x: rect.x + DefaultNavigatorWidth + 300, y: rect.y + 310 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/bar',
      'root/foo',
    ])
  })
  it('can select children of a scene', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'
import { Scene } from 'utopia-api'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <Scene
            style={{
                width: 200,
                height: 200,
                position: 'absolute',
                left: 100,
                top: 100,
            }}
            data-uid='scene'
        >
            <div
                data-uid='foo'
                style={{
                    background: "red",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 20,
                    left: 20,
                }}
            />
        </Scene>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + DefaultNavigatorWidth + 100, y: rect.y + 100 },
      { x: rect.x + DefaultNavigatorWidth + 300, y: rect.y + 300 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'scene/foo',
    ])
  })
  it('can select children of a scene from inside the scene itself', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'
import { Scene } from 'utopia-api'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <Scene
            style={{
                width: 200,
                height: 200,
                position: 'absolute',
                left: 100,
                top: 100,
            }}
            data-uid='scene'
        >
            <div
                data-uid='foo'
                style={{
                    background: "red",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 20,
                    left: 20,
                }}
            />
        </Scene>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + DefaultNavigatorWidth + 210, y: rect.y + 170 },
      { x: rect.x + DefaultNavigatorWidth + 300, y: rect.y + 300 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'scene/foo',
    ])
  })
  it('can select multiple children of a scene', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'
import { Scene } from 'utopia-api'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <Scene
            style={{
                width: 200,
                height: 200,
                position: 'absolute',
                left: 100,
                top: 100,
            }}
            data-uid='scene'
        >
            <div
                data-uid='foo'
                style={{
                    background: "red",
                    width: 50,
                    height: 50,
                    position: "absolute",
                    top: 20,
                    left: 20,
                }}
            />
            <div
                data-uid='bar'
                style={{
                    background: "blue",
                    width: 10,
                    height: 10,
                    position: "absolute",
                    top: 80,
                    left: 80,
                }}
            />
        </Scene>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + DefaultNavigatorWidth + 100, y: rect.y + 100 },
      { x: rect.x + DefaultNavigatorWidth + 350, y: rect.y + 350 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'scene/bar',
      'scene/foo',
    ])
  })
  it("can select an entire scene if it's completely contained by the selection area", async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'
import { Scene } from 'utopia-api'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <div data-uid='root'>
            <Scene
                style={{
                    width: 200,
                    height: 200,
                    position: 'absolute',
                    left: 100,
                    top: 100,
                }}
                data-uid='scene'
            >
                <div data-uid='scene-root'>
                    <div data-uid='scene-container' style={{
                        position:"relative",
                        width: "100%",
                        height: "100%"
                    }}>
                        <div
                            data-uid='foo'
                            style={{
                                background: "red",
                                width: 50,
                                height: 50,
                                position: "absolute",
                                top: 20,
                                left: 20,
                            }}
                        />
                        <div
                            data-uid='bar'
                            style={{
                                background: "blue",
                                width: 10,
                                height: 10,
                                position: "absolute",
                                top: 80,
                                left: 80,
                            }}
                        />
                    </div>
                </div>
            </Scene>
        </div>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + DefaultNavigatorWidth + 100, y: rect.y + 100 },
      { x: rect.x + DefaultNavigatorWidth + 500, y: rect.y + 500 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/scene',
    ])
  })
  it('can select a mix of storyboard elements and scenes', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'
import { Scene } from 'utopia-api'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <div data-uid='root'>
            <Scene
                style={{
                    width: 200,
                    height: 200,
                    position: 'absolute',
                    left: 100,
                    top: 100,
                }}
                data-uid='scene'
            >
                <div data-uid='scene-root'>
                    <div data-uid='scene-container' style={{
                        position:"relative",
                        width: "100%",
                        height: "100%"
                    }}>
                        <div
                            data-uid='foo'
                            style={{
                                background: "red",
                                width: 50,
                                height: 50,
                                position: "absolute",
                                top: 20,
                                left: 20,
                            }}
                        />
                        <div
                            data-uid='bar'
                            style={{
                                background: "blue",
                                width: 10,
                                height: 10,
                                position: "absolute",
                                top: 80,
                                left: 80,
                            }}
                        />
                    </div>
                </div>
            </Scene>
            <div data-uid='baz'
                style={{
                    background: "green",
                    width: 50,
                    height: 80,
                    position: "absolute",
                    top: 300,
                    left: 200,
                }}
            ></div>
        </div>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + DefaultNavigatorWidth + 100, y: rect.y + 100 },
      { x: rect.x + DefaultNavigatorWidth + 500, y: rect.y + 500 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/baz',
      'root/scene',
    ])
  })
  it('skips scene children if also selecting storyboard elements', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'
import { Scene } from 'utopia-api'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <div data-uid='root'>
            <Scene
                style={{
                    width: 200,
                    height: 200,
                    position: 'absolute',
                    left: 100,
                    top: 100,
                }}
                data-uid='scene'
            >
                <div data-uid='scene-root'>
                    <div data-uid='scene-container' style={{
                        position:"relative",
                        width: "100%",
                        height: "100%"
                    }}>
                        <div
                            data-uid='foo'
                            style={{
                                background: "red",
                                width: 50,
                                height: 50,
                                position: "absolute",
                                top: 20,
                                left: 20,
                            }}
                        />
                        <div
                            data-uid='bar'
                            style={{
                                background: "blue",
                                width: 10,
                                height: 10,
                                position: "absolute",
                                top: 80,
                                left: 80,
                            }}
                        />
                    </div>
                </div>
            </Scene>
            <div data-uid='baz'
                style={{
                    background: "green",
                    width: 50,
                    height: 80,
                    position: "absolute",
                    top: 300,
                    left: 50,
                }}
            ></div>
        </div>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + DefaultNavigatorWidth + 100, y: rect.y + 100 },
      { x: rect.x + DefaultNavigatorWidth + 300, y: rect.y + 500 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/baz',
    ])
  })
  it('can add or remove other elements to already selected ones by pressing shift', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <div data-uid='root'>
            <div
                data-uid='foo'
                style={{
                background: '#ccc',
                width: 70,
                height: 50,
                position: 'absolute',
                top: 100,
                left: 100,
                }}
            />
            <div
                data-uid='bar'
                style={{
                background: '#ccc',
                width: 70,
                height: 50,
                position: 'absolute',
                top: 100,
                left: 180,
                }}
            />
            <div
                data-uid='baz'
                style={{
                background: '#ccc',
                width: 70,
                height: 50,
                position: 'absolute',
                top: 160,
                left: 100,
                }}
            />
            <div
                data-uid='qux'
                style={{
                background: '#ccc',
                width: 70,
                height: 50,
                position: 'absolute',
                top: 160,
                left: 180,
                }}
            />
        </div>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    // select the top left and bottom left elements
    {
      await mouseDragFromPointToPoint(
        container,
        { x: rect.x + DefaultNavigatorWidth + 100, y: rect.y + 100 },
        { x: rect.x + DefaultNavigatorWidth + 240, y: rect.y + 310 },
      )

      expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        'root/baz',
        'root/foo',
      ])
    }

    // add the bottom right element to the selection
    {
      await mouseDragFromPointToPoint(
        container,
        { x: rect.x + DefaultNavigatorWidth + 320, y: rect.y + 300 },
        { x: rect.x + DefaultNavigatorWidth + 340, y: rect.y + 240 },
        {
          moveBeforeMouseDown: true,
          staggerMoveEvents: true,
          modifiers: shiftModifier,
          mouseDownModifiers: shiftModifier,
        },
      )

      expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        'root/baz',
        'root/foo',
        'root/qux',
      ])
    }

    // add the top right element, but remove the top left
    {
      await mouseDragFromPointToPoint(
        container,
        { x: rect.x + DefaultNavigatorWidth + 320, y: rect.y + 100 },
        { x: rect.x + DefaultNavigatorWidth + 100, y: rect.y + 180 },
        {
          moveBeforeMouseDown: true,
          staggerMoveEvents: true,
          modifiers: shiftModifier,
          mouseDownModifiers: shiftModifier,
        },
      )

      expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
        'root/baz',
        'root/qux',
        'root/bar',
      ])
    }
  })
  it('ignores elements that are not visible because of overflow', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
import * as React from 'react'

export var ${BakedInStoryboardVariableName} = (props) => {
    return (
        <div data-uid='root'>
            <div data-uid='outer' style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 3, top: 67, width: 232, height: 'max-content', display: 'flex', flexDirection: 'row', padding: '45px 50px' }}>
				<div data-uid='inner' style={{ display: 'flex', flexDirection: 'row', alignItems: 'center', padding: '0px 0px 0px 20px', width: '100%', height: 70, overflowX: 'scroll', justifyContent: 'space-between', flexWrap: 'nowrap', gap: 20 }}>
					{
						// @utopia/uid=list
						[1, 2, 3, 4, 5, 6, 7, 8].map((k) => (
							<div key={k} style={{ backgroundColor: '#aaaaaa33', width: 50, height: 50 }}>
							{k}
							</div>
						))
					}
				</div>
			</div>
        </div>
    )
}
`,
      'await-first-dom-report',
    )
    const container = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    const rect = container.getBoundingClientRect()

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + 620, y: rect.y + 100 },
      { x: rect.x + 670, y: rect.y + 310 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews).toHaveLength(0)

    await renderResult.dispatch(selectComponents([EP.fromString('root/outer/inner')], true), true)

    await mouseDragFromPointToPoint(
      container,
      { x: rect.x + 620, y: rect.y + 100 },
      { x: rect.x + 670, y: rect.y + 310 },
      {
        moveBeforeMouseDown: true,
        staggerMoveEvents: true,
        modifiers: shiftModifier,
        mouseDownModifiers: shiftModifier,
      },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/outer/inner',
    ])
  })
})
