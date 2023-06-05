import { BakedInStoryboardVariableName } from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import { DefaultNavigatorWidth } from '../../editor/store/editor-state'
import { CanvasControlsContainerID } from './new-canvas-controls'
import { mouseDragFromPointToPoint } from '../event-helpers.test-utils'
import { renderTestEditorWithCode } from '../ui-jsx.test-utils'

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
      'root/foo',
      'root/bar',
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
      'root/foo',
      'root/bar',
    ])
  })
  it('can select children of a scene', async () => {
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
      { x: rect.x + DefaultNavigatorWidth + 300, y: rect.y + 300 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/scene/scene-root/scene-container/foo',
    ])
  })
  it('can select children of a scene from inside the scene itself', async () => {
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
      { x: rect.x + DefaultNavigatorWidth + 210, y: rect.y + 170 },
      { x: rect.x + DefaultNavigatorWidth + 300, y: rect.y + 300 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/scene/scene-root/scene-container/foo',
    ])
  })

  it('can select multiple children of a scene', async () => {
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
      { x: rect.x + DefaultNavigatorWidth + 350, y: rect.y + 350 },
      { moveBeforeMouseDown: true, staggerMoveEvents: true },
    )

    expect(renderResult.getEditorState().editor.selectedViews.map(EP.toString)).toEqual([
      'root/scene/scene-root/scene-container/foo',
      'root/scene/scene-root/scene-container/bar',
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
      'root/scene',
      'root/baz',
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
})
