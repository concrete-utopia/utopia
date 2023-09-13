import * as EP from '../../../core/shared/element-path'
import type { WindowPoint } from '../../../core/shared/math-utils'
import { windowPoint } from '../../../core/shared/math-utils'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import type { Modifiers } from '../../../utils/modifiers'
import { emptyModifiers, cmdModifier } from '../../../utils/modifiers'
import {
  selectComponentsForTest,
  setFeatureForBrowserTestsUseInDescribeBlockOnly,
} from '../../../utils/utils.test-utils'
import { switchEditorMode } from '../../editor/actions/action-creators'
import { EditorModes } from '../../editor/editor-modes'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointWithDelta,
  pressKey,
} from '../event-helpers.test-utils'
import { REMIX_SCENE_TESTID } from '../ui-jsx-canvas-renderer/remix-scene-component'
import type { EditorRenderResult } from '../ui-jsx.test-utils'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'

const DefaultRouteTextContent = 'Hello Remix!'
const RootTextContent = 'This is root!'

const storyboardFileContent = `
import * as React from 'react';
import Utopia, {
  Scene,
  Storyboard,
  RemixScene,
} from 'utopia-api';


export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      data-uid='scene'
      style={{ position: 'absolute', left: 400, top: 0, width: 375, height: 812 }}
    >
      <RemixScene />
    </Scene>
  </Storyboard>
);
`

/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectRemixSceneToBeRendered"] }] */

describe('Remix content', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)
  it('Renders the remix container with actual content', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
      import { RemixScene, Storyboard } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard>
          <RemixScene
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 212,
              top: 128,
            }}
            data-label='Playground'
          />
        </Storyboard>
      )
      `,
      ['/src/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'
      
      export default function Root() {
        return (
          <div>
            ${RootTextContent}
            <Outlet />
          </div>
        )
      }
      `,
      ['/src/routes/_index.js']: `import React from 'react'

      export default function Index() {
        return <h1>${DefaultRouteTextContent}</h1>
      }
      `,
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')
    await expectRemixSceneToBeRendered(renderResult)
  })

  it('Remix content has metadata', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
      import { RemixScene, Storyboard } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard data-uid='storyboard'>
          <RemixScene
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 212,
              top: 128,
            }}
            data-label='Playground'
            data-uid='remix-scene'
          />
        </Storyboard>
      )
      `,
      ['/src/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'
      
      export default function Root() {
        return (
          <div data-uid='rootdiv'>
            ${RootTextContent}
            <Outlet data-uid='outlet'/>
          </div>
        )
      }
      `,
      ['/src/routes/_index.js']: `import React from 'react'

      export default function Index() {
        return <div
          style={{
            width: 200,
            height: 200,
            position: 'absolute',
            left: 0,
            top: 0,
          }}
          data-uid='remix-div'
        >
          ${DefaultRouteTextContent}
        </div>
      }
      `,
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixDivMetadata =
      renderResult.getEditorState().editor.jsxMetadata[
        'storyboard/remix-scene:rootdiv/outlet:remix-div'
      ]

    expect(remixDivMetadata).not.toBeUndefined()

    expect(remixDivMetadata.globalFrame).toEqual({
      height: 200,
      width: 200,
      x: 212,
      y: 128,
    })
  })

  it('Two remix scenes, both have metadata', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
      import { RemixScene, Storyboard } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard data-uid='storyboard'>
          <RemixScene
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 212,
              top: 128,
            }}
            data-label='Playground'
            data-uid='remix-scene'
          />
           <RemixScene
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 0,
              top: 0,
            }}
            data-label='Playground'
            data-uid='remix-scene-2'
          />
        </Storyboard>
      )
      `,
      ['/src/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'
      
      export default function Root() {
        return (
          <div data-uid='rootdiv'>
            ${RootTextContent}
            <Outlet data-uid='outlet'/>
          </div>
        )
      }
      `,
      ['/src/routes/_index.js']: `import React from 'react'

      export default function Index() {
        return <div
          style={{
            width: 200,
            height: 200,
            position: 'absolute',
            left: 0,
            top: 0,
          }}
          data-uid='remix-div'
        >
          ${DefaultRouteTextContent}
        </div>
      }
      `,
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixDivMetadata =
      renderResult.getEditorState().editor.jsxMetadata[
        'storyboard/remix-scene:rootdiv/outlet:remix-div'
      ]

    expect(remixDivMetadata).not.toBeUndefined()

    expect(remixDivMetadata.globalFrame).toEqual({
      height: 200,
      width: 200,
      x: 212,
      y: 128,
    })

    const remixDiv2Metadata =
      renderResult.getEditorState().editor.jsxMetadata[
        'storyboard/remix-scene-2:rootdiv/outlet:remix-div'
      ]

    expect(remixDiv2Metadata).not.toBeUndefined()

    expect(remixDiv2Metadata.globalFrame).toEqual({
      height: 200,
      width: 200,
      x: 0,
      y: 0,
    })
  })

  it('Remix content can be selected', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
      import { RemixScene, Storyboard } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard data-uid='storyboard'>
          <RemixScene
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 212,
              top: 128,
            }}
            data-label='Playground'
            data-uid='remix-scene'
          />
        </Storyboard>
      )
      `,
      ['/src/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'
      
      export default function Root() {
        return (
          <div data-uid='rootdiv'>
            ${RootTextContent}
            <Outlet data-uid='outlet'/>
          </div>
        )
      }
      `,
      ['/src/routes/_index.js']: `import React from 'react'

      export default function Index() {
        return <div data-uid='remix-route-root'>
          <div data-uid='remix-div' data-testid='remix-div'>${DefaultRouteTextContent}</div>
        </div>
      }
      `,
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const targetElement = renderResult.renderedDOM.getByTestId('remix-div')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const clickPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    await mouseClickAtPoint(canvasControlsLayer, clickPoint, { modifiers: cmdModifier })

    expect(renderResult.getEditorState().editor.selectedViews).toHaveLength(1)

    expect(EP.toString(renderResult.getEditorState().editor.selectedViews[0])).toEqual(
      'storyboard/remix-scene:rootdiv/outlet:remix-route-root/remix-div',
    )
  })

  it('Remix content is rendered while a canvas interaction is in progress', async () => {
    const DraggedElementId = 'dragme'
    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
      import { RemixScene, Storyboard } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard>
          <RemixScene
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 212,
              top: 128,
            }}
            data-label='Playground'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 213,
              top: 47,
              width: 130,
              height: 61,
            }}
            data-uid='46a'
            data-testid='${DraggedElementId}'
          />
        </Storyboard>
      )
      `,
      ['/src/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'
      
      export default function Root() {
        return (
          <div>
            ${RootTextContent}
            <Outlet />
          </div>
        )
      }
      `,
      ['/src/routes/_index.js']: `import React from 'react'

      export default function Index() {
        return <h1>${DefaultRouteTextContent}</h1>
      }
      `,
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const targetElement = renderResult.renderedDOM.getByTestId(DraggedElementId)
    const targetElementBounds = targetElement.getBoundingClientRect()
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await expectRemixSceneToBeRendered(renderResult)

    await dragElement(canvasControlsLayer, startPoint, dragDelta, emptyModifiers, () =>
      expectRemixSceneToBeRendered(renderResult),
    )
  })
})

describe('Remix content with feature switch off', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', false)
  it('Doesnt render the remix container with feature switch off', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')
    await expect(async () =>
      renderResult.renderedDOM.findAllByTestId(REMIX_SCENE_TESTID),
    ).rejects.toThrow()
  })
})

describe('Remix navigation', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)
  it('Can navigate to a different route', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
        import { RemixScene, Storyboard } from 'utopia-api'
        
        export var storyboard = (
          <Storyboard>
            <RemixScene
              style={{
                width: 700,
                height: 759,
                position: 'absolute',
                left: 212,
                top: 128,
              }}
              data-label='Playground'
            />
          </Storyboard>
        )
        `,
      ['/src/root.js']: `import React from 'react'
        import { Outlet } from '@remix-run/react'
        
        export default function Root() {
          return (
            <div>
              ${RootTextContent}
              <Outlet />
            </div>
          )
        }
        `,
      ['/src/routes/_index.js']: `import React from 'react'
        import { Link } from '@remix-run/react'
  
        export default function Index() {
          return <Link to='/about' data-testid='remix-link'>${DefaultRouteTextContent}</Link>
        }
        `,
      ['/src/routes/about.js']: `import React from 'react'
  
        export default function About() {
          return <h1>About</h1>
        }
        `,
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')
    await renderResult.dispatch([switchEditorMode(EditorModes.liveMode())], true)

    const targetElement = renderResult.renderedDOM.getByTestId('remix-link')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const clickPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })

    await mouseClickAtPoint(targetElement, clickPoint)

    await expectRemixSceneToBeRendered(renderResult, 'About')
  })

  it('Remix navigation updates metadata', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
      import { RemixScene, Storyboard } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard data-uid='storyboard'>
          <RemixScene
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 212,
              top: 128,
            }}
            data-label='Playground'
            data-uid='remix-scene'
          />
        </Storyboard>
      )
      `,
      ['/src/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'
      
      export default function Root() {
        return (
          <div data-uid='rootdiv'>
            ${RootTextContent}
            <Outlet data-uid='outlet'/>
          </div>
        )
      }
      `,
      ['/src/routes/_index.js']: `import React from 'react'
      import { Link } from '@remix-run/react'

      export default function Index() {
        return <Link to='/about' data-testid='remix-link' data-uid='remixlink'>${DefaultRouteTextContent}</Link>
      }
      `,
      ['/src/routes/about.js']: `import React from 'react'

      export default function About() {
        return <div
          style={{
            width: 200,
            height: 200,
            position: 'absolute',
            left: 0,
            top: 0,
          }}
          data-uid='aboutdiv'
        >
          About
        </div>
      }
      `,
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const remixLinkMetadata =
      renderResult.getEditorState().editor.jsxMetadata[
        'storyboard/remix-scene:rootdiv/outlet:remixlink'
      ]
    expect(remixLinkMetadata).not.toBeUndefined()

    await renderResult.dispatch([switchEditorMode(EditorModes.liveMode())], true)

    const targetElement = renderResult.renderedDOM.getByTestId('remix-link')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const clickPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })

    await mouseClickAtPoint(targetElement, clickPoint)

    const remixLinkMetadataAfterNavigation =
      renderResult.getEditorState().editor.jsxMetadata[
        'storyboard/remix-scene:rootdiv/outlet:remixlink'
      ]
    expect(remixLinkMetadataAfterNavigation).toBeUndefined()

    const remixAboutDivMetadata =
      renderResult.getEditorState().editor.jsxMetadata[
        'storyboard/remix-scene:rootdiv/outlet:aboutdiv'
      ]

    expect(remixAboutDivMetadata).not.toBeUndefined()
    expect(remixAboutDivMetadata.globalFrame).toEqual({
      height: 200,
      width: 200,
      x: 212,
      y: 128,
    })
  })
})

describe('Editing Remix content', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)

  it('set opacity on remix element', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
      import { RemixScene, Storyboard } from 'utopia-api'
      
      export var storyboard = (
        <Storyboard data-uid='sb'>
          <RemixScene
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 212,
              top: 128,
            }}
            data-uid='rs'
            data-label='Playground'
          />
        </Storyboard>
      )
      `,
      ['/src/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'
      
      export default function Root() {
        return (
          <div data-uid='root'>
            ${RootTextContent}
            <Outlet data-uid='outlet' />
          </div>
        )
      }
      `,
      ['/src/routes/_index.js']: `import React from 'react'

      export default function Index() {
        return <h1 data-uid='title' data-testid='title'>${DefaultRouteTextContent}</h1>
      }
      `,
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    const pathString = 'sb/rs:root/outlet:title'

    await selectComponentsForTest(renderResult, [EP.fromString(pathString)])
    await pressKey('3')
    await pressKey('0')

    const titleElement = renderResult.renderedDOM.getByTestId('title')
    expect(titleElement.style.opacity).toEqual('0.3')
  })
})

async function dragElement(
  canvasControlsLayer: HTMLElement,
  startPoint: WindowPoint,
  dragDelta: WindowPoint,
  modifiers: Modifiers,
  midDragCallback: () => Promise<void>,
): Promise<void> {
  await mouseDownAtPoint(canvasControlsLayer, startPoint, { modifiers: cmdModifier })
  await mouseDragFromPointWithDelta(canvasControlsLayer, startPoint, dragDelta, {
    modifiers,
    midDragCallback,
  })
}

async function expectRemixSceneToBeRendered(
  editor: EditorRenderResult,
  textContent: string = DefaultRouteTextContent,
) {
  const [remixScene] = await editor.renderedDOM.findAllByTestId(REMIX_SCENE_TESTID)
  expect(remixScene).toBeDefined()

  const [rootText] = await editor.renderedDOM.findAllByText(RootTextContent)
  expect(rootText).toBeDefined()

  const [indexText] = await editor.renderedDOM.findAllByText(textContent)
  expect(indexText).toBeDefined()
}
