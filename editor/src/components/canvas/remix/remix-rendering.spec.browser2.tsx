import type { WindowPoint } from '../../../core/shared/math-utils'
import { windowPoint } from '../../../core/shared/math-utils'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import type { Modifiers } from '../../../utils/modifiers'
import { emptyModifiers, cmdModifier } from '../../../utils/modifiers'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../../utils/utils.test-utils'
import { switchEditorMode } from '../../editor/actions/action-creators'
import { EditorModes } from '../../editor/editor-modes'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import {
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointWithDelta,
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
