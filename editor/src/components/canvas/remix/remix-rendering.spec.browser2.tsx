import { within } from '@testing-library/react'
import * as EP from '../../../core/shared/element-path'
import type { WindowPoint } from '../../../core/shared/math-utils'
import { windowPoint } from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { NO_OP } from '../../../core/shared/utils'
import {
  createModifiedProject,
  createTestProjectWithCode,
} from '../../../sample-projects/sample-project-utils.test-utils'
import type { Modifiers } from '../../../utils/modifiers'
import { emptyModifiers, cmdModifier } from '../../../utils/modifiers'
import { selectComponentsForTest } from '../../../utils/utils.test-utils'
import {
  runDOMWalker,
  selectComponents,
  switchEditorMode,
} from '../../editor/actions/action-creators'
import { EditorModes } from '../../editor/editor-modes'
import { StoryboardFilePath, navigatorEntryToKey } from '../../editor/store/editor-state'
import type { PersistentModel } from '../../editor/store/editor-state'
import { AddRemoveLayoutSystemControlTestId } from '../../inspector/add-remove-layout-system-control'
import { CanvasControlsContainerID } from '../controls/new-canvas-controls'
import type { RemixSceneLabelButtonType } from '../controls/select-mode/remix-scene-label'
import {
  LocationDoesNotMatchRoutesTestId,
  RemixSceneLabelButtonTestId,
  RemixSceneLabelPathTestId,
} from '../controls/select-mode/remix-scene-label'
import {
  MockClipboardHandlers,
  firePasteEvent,
  mouseClickAtPoint,
  mouseDownAtPoint,
  mouseDragFromPointWithDelta,
  pressKey,
} from '../event-helpers.test-utils'
import { REMIX_SCENE_TESTID } from '../ui-jsx-canvas-renderer/remix-scene-component'
import type { EditorRenderResult } from '../ui-jsx.test-utils'
import {
  DefaultStartingFeatureSwitches,
  getPrintedUiJsCode,
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
} from '../ui-jsx.test-utils'
import {
  RemixNavigationBarButtonTestId,
  RemixNavigationBarPathTestId,
} from '../../editor/remix-navigation-bar'
import { NonResizableControlTestId } from '../controls/select-mode/non-resizable-control'
import {
  MultiSelectOutlineTestId,
  getMultiSelectElementOutlineTestId,
} from '../controls/select-mode/simple-outline-control'
import { updateFromCodeEditor } from '../../editor/actions/actions-from-vscode'
import { RemixIndexPathLabel } from './remix-utils'
import {
  type MetaCanvasStrategy,
  RegisteredCanvasStrategies,
} from '../canvas-strategies/canvas-strategies'
import CanvasActions from '../canvas-actions'

const DefaultRouteTextContent = 'Hello Remix!'
const RootTextContent = 'This is root!'
const AboutTextContent = 'About'

/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "expectRemixSceneToBeRendered"] }] */

async function renderRemixProject(
  project: PersistentModel,
  strategiesToUse: Array<MetaCanvasStrategy> = RegisteredCanvasStrategies,
) {
  const renderResult = await renderTestEditorWithModel(
    project,
    'await-first-dom-report',
    DefaultStartingFeatureSwitches,
    undefined,
    strategiesToUse,
  )
  await renderResult.dispatch([runDOMWalker()], true)
  return renderResult
}

describe('Remix content', () => {
  it('Renders the remix container with actual content and a cyclic dependency', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
      import { RemixScene, Storyboard } from 'utopia-api'
      import { Card } from '/app/components/card'

      export function gimmeData() {
        return '${DefaultRouteTextContent}'
      }
      
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
      ['/app/components/card.js']: `import * as React from 'react'
      import { gimmeData } from '${StoryboardFilePath}'

      export const Card = (props) => {
        const data = gimmeData()
        return (
          <h1>{data}</h1>
        )
      }
      `,
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'
      import { Card } from '/app/components/card'

      export default function Index() {
        return <Card />
      }
      `,
    })

    const renderResult = await renderRemixProject(project)
    await expectRemixSceneToBeRendered(renderResult)
  })

  it('Renders the remix project with separate export default statement in route file', async () => {
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
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'

      const Index = () => (<h1>${DefaultRouteTextContent}</h1>)
      export default Index
      `,
    })

    const renderResult = await renderRemixProject(project)
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
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'

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

    const renderResult = await renderRemixProject(project)

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

  it('Remix Outlet is automatically locked', async () => {
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
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'

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

    const renderResult = await renderRemixProject(project)

    expect(
      renderResult.getEditorState().editor.lockedElements.simpleLock.map(EP.toString),
    ).toContain('storyboard/remix-scene:rootdiv/outlet')
  })

  it("remix scene label signals that the existing routes don't match the current location", async () => {
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
            data-uid='remix-scene'
          />
        </Storyboard>
      )
      `,
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'
      import { Link } from '@remix-run/react'

      export default function Index() {
        return <Link to='/about' data-testid='remix-link' data-uid='remix-link'>${DefaultRouteTextContent}</Link>
      }
      `,
    })

    const renderResult = await renderRemixProject(project)

    await switchToLiveMode(renderResult)

    await clickRemixLink(renderResult)

    expect(renderResult.renderedDOM.queryAllByText('404 Not Found')).toHaveLength(1) // default 404 page is rendered
    expect(
      renderResult.renderedDOM.queryAllByTestId(LocationDoesNotMatchRoutesTestId),
    ).toHaveLength(1) // location not matching warning is rendered
  })

  it("changing a route whilst 404 is shown doesn't break the navigation", async () => {
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
            data-label='Playground'
            data-uid='remix-scene'
          />
        </Storyboard>
      )
      `,
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'
      import { Link } from '@remix-run/react'

      export default function Index() {
        return <Link to='/about' data-testid='remix-link' data-uid='remix-link'>${DefaultRouteTextContent}</Link>
      }
      `,
    })

    const renderResult = await renderRemixProject(project)

    await switchToLiveMode(renderResult)

    await clickRemixLink(renderResult)

    expect(renderResult.renderedDOM.queryByText('404 Not Found')).not.toBeNull() // default 404 page is rendered

    // Make a change to a route, which will in turn update the router
    const newRouteTextContent = 'I have changed!'
    await renderResult.dispatch(
      [
        updateFromCodeEditor(
          '/app/routes/_index.js',
          `import React from 'react'
    import { Link } from '@remix-run/react'

    export default function Index() {
      return <Link to='/about' data-testid='remix-link' data-uid='remix-link'>${newRouteTextContent}</Link>
    }
    `,
          null,
        ),
      ],
      true,
    )
    await renderResult.getDispatchFollowUpActionsFinished()

    // Ensure we can still hit the back button and navigate back to the previous route, with the new content
    const pathToRemixScene = EP.fromString('sb/remix-scene')
    await navigateWithRemixSceneLabelButton(renderResult, pathToRemixScene, 'back')
    expect(renderResult.renderedDOM.queryByText(newRouteTextContent)).not.toBeNull()
    expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene)).toEqual(RemixIndexPathLabel)
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
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'

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

    const renderResult = await renderRemixProject(project)

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

  it('Two remix outlets, both have metadata', async () => {
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
      ['/app/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'
      
      export default function Root() {
        return (
          <div data-uid='rootdiv'>
            ${RootTextContent}
            <Outlet data-uid='outlet'/>
            <Outlet data-uid='outlet2'/>
          </div>
        )
      }
      `,
      ['/app/routes/_index.js']: `import React from 'react'

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

    const renderResult = await renderRemixProject(project)

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
        'storyboard/remix-scene:rootdiv/outlet2:remix-div'
      ]

    expect(remixDiv2Metadata).not.toBeUndefined()

    expect(remixDiv2Metadata.globalFrame).toEqual({
      height: 200,
      width: 200,
      x: 212,
      y: 128,
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
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'

      export default function Index() {
        return <div data-uid='remix-route-root'>
          <div data-uid='remix-div' data-testid='remix-div'>${DefaultRouteTextContent}</div>
        </div>
      }
      `,
    })

    const renderResult = await renderRemixProject(project)

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

  it('Remix content can be selected from second outlet', async () => {
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
      ['/app/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'
      
      export default function Root() {
        return (
          <div data-uid='rootdiv'>
            ${RootTextContent}
            <Outlet data-uid='outlet'/>
            <Outlet data-uid='outlet2'/>
          </div>
        )
      }
      `,
      ['/app/routes/_index.js']: `import React from 'react'

      export default function Index() {
        return <div data-uid='remix-route-root'>
          <div data-uid='remix-div' data-testid='remix-div'>${DefaultRouteTextContent}</div>
        </div>
      }
      `,
    })

    const renderResult = await renderRemixProject(project)

    const targetElement = renderResult.renderedDOM.getAllByTestId('remix-div')[1]
    const targetElementBounds = targetElement.getBoundingClientRect()

    const clickPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
    await mouseClickAtPoint(canvasControlsLayer, clickPoint, { modifiers: cmdModifier })

    expect(renderResult.getEditorState().editor.selectedViews).toHaveLength(1)

    expect(EP.toString(renderResult.getEditorState().editor.selectedViews[0])).toEqual(
      'storyboard/remix-scene:rootdiv/outlet2:remix-route-root/remix-div',
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
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'

      export default function Index() {
        return <h1>${DefaultRouteTextContent}</h1>
      }
      `,
    })

    const renderResult = await renderRemixProject(project)

    const targetElement = renderResult.renderedDOM.getByTestId(DraggedElementId)
    const targetElementBounds = targetElement.getBoundingClientRect()

    const startPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
    const dragDelta = windowPoint({ x: 40, y: -25 })

    await expectRemixSceneToBeRendered(renderResult)

    await dragMouse(renderResult, startPoint, dragDelta, emptyModifiers, () =>
      expectRemixSceneToBeRendered(renderResult),
    )
  })
})

describe('Remix getLoadContext', () => {
  it('Renders the remix project that relies on async loader context', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
      import { RemixScene, Storyboard } from 'utopia-api'
      const getLoadContext = async (request) => {
        return {
          request: request,
          otherStuff: await Promise.resolve('hello!')
        }
      }
      
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
            getLoadContext={getLoadContext}
          />
        </Storyboard>
      )
      `,
      ['/app/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'
      import { json, useLoaderData } from 'react-router'
      export function loader({params, request, context}) {
        console.log('loader called', params, request, context)
        return json({
          contextEqualsRequest: context.request === request,
        })
      }
      
      export default function Root() {
        const loaderData = useLoaderData()
        return (
          <div>
            Request given to loader matches the request given to getLoadContext: {JSON.stringify(loaderData.contextEqualsRequest)}
            <div>${RootTextContent}</div>
            <Outlet />
          </div>
        )
      }
      `,
      ['/app/routes/_index.js']: `import React from 'react'
      const Index = () => (<h1>${DefaultRouteTextContent}</h1>)
      export default Index
      `,
    })

    const renderResult = await renderRemixProject(project)

    expect(
      renderResult.renderedDOM.getByText(
        'Request given to loader matches the request given to getLoadContext: true',
      ),
    ).toBeTruthy()

    await expectRemixSceneToBeRendered(renderResult)
  })

  it('Renders the remix project that relies on synchronous loader context', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
      import { RemixScene, Storyboard } from 'utopia-api'
      const getLoadContext = (request) => {
        return {
          request: request,
          otherStuff: 'hello!'
        }
      }
      
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
            getLoadContext={getLoadContext}
          />
        </Storyboard>
      )
      `,
      ['/app/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'
      import { json, useLoaderData } from 'react-router'
      export function loader({params, request, context}) {
        console.log('loader called', params, request, context)
        return json({
          contextEqualsRequest: context.request === request,
        })
      }
      
      export default function Root() {
        const loaderData = useLoaderData()
        return (
          <div>
            Request given to loader matches the request given to getLoadContext: {JSON.stringify(loaderData.contextEqualsRequest)}
            <div>${RootTextContent}</div>
            <Outlet />
          </div>
        )
      }
      `,
      ['/app/routes/_index.js']: `import React from 'react'
      const Index = () => (<h1>${DefaultRouteTextContent}</h1>)
      export default Index
      `,
    })

    const renderResult = await renderRemixProject(project)

    expect(
      renderResult.renderedDOM.getByText(
        'Request given to loader matches the request given to getLoadContext: true',
      ),
    ).toBeTruthy()

    await expectRemixSceneToBeRendered(renderResult)
  })
})

describe('Remix navigation', () => {
  const projectWithMultipleRoutes = () =>
    createModifiedProject({
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
            data-uid='remix'
          />
        </Storyboard>
      )
      `,
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'
      import { Link } from '@remix-run/react'

      export default function Index() {
        return <Link to='/about' data-testid='remix-link'>${DefaultRouteTextContent}</Link>
      }
      `,
      ['/app/routes/about.js']: `import React from 'react'

      export default function About() {
        return <h1>${AboutTextContent}</h1>
      }
      `,
    })

  const Remix1TestId = 'remix-1'
  const Remix2TestId = 'remix-2'

  const projectWithMultipleRemixScenes = () =>
    createModifiedProject({
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
            data-testid='${Remix1TestId}'
            data-uid='${Remix1TestId}'
          />

          <RemixScene
            style={{
              width: 700,
              height: 759,
              position: 'absolute',
              left: 1000,
              top: 128,
            }}
            data-label='Playground'
            data-testid='${Remix2TestId}'
            data-uid='${Remix2TestId}'
          />
        </Storyboard>
      )
      `,
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'
      import { Link } from '@remix-run/react'

      export default function Index() {
        return <Link to='/about' data-testid='remix-link'>${DefaultRouteTextContent}</Link>
      }
      `,
      ['/app/routes/about.js']: `import React from 'react'

      export default function About() {
        return <h1>${AboutTextContent}</h1>
      }
      `,
    })

  it('Can navigate to a different route', async () => {
    const renderResult = await renderRemixProject(
      createModifiedProject({
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
            data-uid='remix'
          />
        </Storyboard>
      )
      `,
        ['/app/root.js']: `import React from 'react'
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
        ['/app/routes/_index.js']: `import React from 'react'
      import { Link } from '@remix-run/react'

      export default function Index() {
        return <Link to='/posts' data-testid='remix-link'>${DefaultRouteTextContent}</Link>
      }
      `,
        ['/app/routes/posts._index.js']: `import React from 'react'
        import { Link } from '@remix-run/react'
        import { json, useLoaderData } from 'react-router'
        
        export function loader() {
          return json({
            activities: [
              {
                id: 0,
                name: 'Do the thing',
              },
            ]
          })
        }
        
        export default function Posts() {
          const { activities } = useLoaderData()
          return (
            <div>
              {activities.map(
                ({
                  id,
                  name,
                }) => (
                  <Link to={\`\${id}\`} data-testid='post-link'>{name}</Link>
                )
              )}
            </div>
          )
        }
        `,
        ['/app/routes/posts.$postId.js']: `import React from 'react'
        import { json, useLoaderData } from 'react-router'
        
        export function loader({ params }) {
          return json({
            id: 0,
            name: 'Do the thing',
            desc: 'Do it now!'
          })
        }
        export default function Post() {
          const {
            desc
          } = useLoaderData()
          return (
            <div>{desc}</div>
          )
        }`,
      }),
    )
    await switchToLiveMode(renderResult)

    await clickRemixLink(renderResult)

    await clickPostLink(renderResult)

    await expectRemixSceneToBeRendered(renderResult, 'Do it now!')
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
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'
      import { Link } from '@remix-run/react'

      export default function Index() {
        return <Link to='/about' data-testid='remix-link' data-uid='remixlink'>${DefaultRouteTextContent}</Link>
      }
      `,
      ['/app/routes/about.js']: `import React from 'react'

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

    const renderResult = await renderRemixProject(project)
    await switchToLiveMode(renderResult)

    const remixLinkMetadata =
      renderResult.getEditorState().editor.jsxMetadata[
        'storyboard/remix-scene:rootdiv/outlet:remixlink'
      ]
    expect(remixLinkMetadata).not.toBeUndefined()

    await clickRemixLink(renderResult)

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

  it('Can navigate through dynamic link', async () => {
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
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'
      import { Link } from '@remix-run/react'
      
      export default function Index() {
        return (
          <Link
            to='/1'
            data-testid='remix-link'
            data-uid='remix-link'
          >
            Go
          </Link>
        )
      }      
      `,
      ['/app/routes/$postId.js']: `import React from 'react'
      import { Link, useParams } from '@remix-run/react'
      
      export default function PostForId() {
        const { postId } = useParams()
        const postIdParsed = parseInt(postId)
        return (
          <div>
            <h1>post id: {postId}</h1>
            <Link to={${'`/${postIdParsed + 1}`'}} data-testid='remix-link'>Next</Link>
          </div>
        )
      }
      
      `,
    })

    const renderResult = await renderRemixProject(project)
    await switchToLiveMode(renderResult)
    expect(renderResult.renderedDOM.queryAllByText('Go').filter(filterOutMenuLabels)).toHaveLength(
      1,
    )

    for (let i = 1; i < 7; i++) {
      await clickRemixLink(renderResult)
      expect(
        renderResult.renderedDOM.queryAllByText(`post id: ${i}`).filter(filterOutMenuLabels),
      ).toHaveLength(1)
    }
  })

  describe('remix scene label', () => {
    it('can navigate with the scene label nav buttons, in live mode', async () => {
      const renderResult = await renderRemixProject(projectWithMultipleRoutes())

      const pathToRemixScene = EP.fromString('storyboard/remix')

      await switchToLiveMode(renderResult)
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene)).toEqual(RemixIndexPathLabel)

      await clickRemixLink(renderResult)

      expect(
        renderResult.renderedDOM.queryAllByText(AboutTextContent).filter(filterOutMenuLabels),
      ).toHaveLength(1)
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene)).toEqual('/about')

      await navigateWithRemixSceneLabelButton(renderResult, pathToRemixScene, 'back')

      expect(renderResult.renderedDOM.queryAllByText(RootTextContent)).toHaveLength(1)
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene)).toEqual(RemixIndexPathLabel)

      await navigateWithRemixSceneLabelButton(renderResult, pathToRemixScene, 'forward')
      expect(
        renderResult.renderedDOM.queryAllByText(AboutTextContent).filter(filterOutMenuLabels),
      ).toHaveLength(1)
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene)).toEqual('/about')
    })

    it('can navigate with the scene label nav buttons, in edit mode', async () => {
      const renderResult = await renderRemixProject(projectWithMultipleRoutes())

      const pathToRemixScene = EP.fromString('storyboard/remix')

      await switchToLiveMode(renderResult)
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene)).toEqual(RemixIndexPathLabel)

      await clickRemixLink(renderResult)

      expect(
        renderResult.renderedDOM.queryAllByText(AboutTextContent).filter(filterOutMenuLabels),
      ).toHaveLength(1)
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene)).toEqual('/about')

      await switchToEditMode(renderResult)

      // check that switching modes doesn't change the navigation state
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene)).toEqual('/about')
      expect(
        renderResult.renderedDOM.queryAllByText(AboutTextContent).filter(filterOutMenuLabels),
      ).toHaveLength(1)

      await navigateWithRemixSceneLabelButton(renderResult, pathToRemixScene, 'back')

      expect(renderResult.renderedDOM.queryAllByText(RootTextContent)).toHaveLength(1)
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene)).toEqual(RemixIndexPathLabel)

      await navigateWithRemixSceneLabelButton(renderResult, pathToRemixScene, 'forward')
      expect(
        renderResult.renderedDOM.queryAllByText(AboutTextContent).filter(filterOutMenuLabels),
      ).toHaveLength(1)
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene)).toEqual('/about')
    })

    it('navigating in one Remix scene does not affect the navigation state in the other', async () => {
      const renderResult = await renderRemixProject(projectWithMultipleRemixScenes())

      const pathToRemixScene1 = EP.fromString('storyboard/remix-1')
      const pathToRemixScene2 = EP.fromString('storyboard/remix-2')

      await switchToLiveMode(renderResult)
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene1)).toEqual(RemixIndexPathLabel)
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene2)).toEqual(RemixIndexPathLabel)

      await clickRemixLink(renderResult)
      const remixScene1 = renderResult.renderedDOM.getByTestId(Remix1TestId)
      expect(
        within(remixScene1).queryAllByText(AboutTextContent).filter(filterOutMenuLabels),
      ).toHaveLength(1)

      const remixScene2 = renderResult.renderedDOM.getByTestId(Remix2TestId)
      expect(within(remixScene2).queryAllByText(RootTextContent)).toHaveLength(1)

      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene1)).toEqual('/about')
      expect(getPathInRemixSceneLabel(renderResult, pathToRemixScene2)).toEqual(RemixIndexPathLabel)
    })
  })

  describe('Nav bar in the canvas toolbar', () => {
    it('can navigate using the nav bar in the canvas toolbar', async () => {
      const renderResult = await renderRemixProject(projectWithMultipleRoutes())

      await switchToLiveMode(renderResult)
      expect(getPathInRemixNavigationBar(renderResult)).toEqual(RemixIndexPathLabel)

      await clickRemixLink(renderResult)

      expect(
        renderResult.renderedDOM.queryAllByText(AboutTextContent).filter(filterOutMenuLabels),
      ).toHaveLength(1)
      expect(getPathInRemixNavigationBar(renderResult)).toEqual('/about')

      await navigateWithRemixNavigationBarButton(renderResult, 'back')

      expect(renderResult.renderedDOM.queryAllByText(RootTextContent)).toHaveLength(1)
      expect(getPathInRemixNavigationBar(renderResult)).toEqual(RemixIndexPathLabel)

      await navigateWithRemixNavigationBarButton(renderResult, 'forward')
      expect(
        renderResult.renderedDOM.queryAllByText(AboutTextContent).filter(filterOutMenuLabels),
      ).toHaveLength(1)
      expect(getPathInRemixNavigationBar(renderResult)).toEqual('/about')

      await navigateWithRemixNavigationBarButton(renderResult, 'home')
      expect(renderResult.renderedDOM.queryAllByText(RootTextContent)).toHaveLength(1)
      expect(getPathInRemixNavigationBar(renderResult)).toEqual(RemixIndexPathLabel)
    })

    it('can navigate inside multiple Remix scenes, using the nav bar in the canvas toolbar', async () => {
      const renderResult = await renderRemixProject(projectWithMultipleRemixScenes())

      await switchToLiveMode(renderResult)
      expect(getPathInRemixNavigationBar(renderResult)).toEqual(RemixIndexPathLabel)

      await clickRemixLink(renderResult)

      expect(
        renderResult.renderedDOM.queryAllByText(AboutTextContent).filter(filterOutMenuLabels),
      ).toHaveLength(1)
      expect(getPathInRemixNavigationBar(renderResult)).toEqual('/about')

      const remixScene2 = renderResult.renderedDOM.getByTestId(Remix2TestId)
      await mouseClickAtPoint(remixScene2, { x: 10, y: 10 })

      expect(getPathInRemixNavigationBar(renderResult)).toEqual(RemixIndexPathLabel)
    })
  })
})

describe('Editing Remix content', () => {
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
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'

      export default function Index() {
        return <h1 data-uid='title' data-testid='title'>${DefaultRouteTextContent}</h1>
      }
      `,
    })

    const renderResult = await renderRemixProject(project)

    const pathString = 'sb/rs:root/outlet:title'

    await selectComponentsForTest(renderResult, [EP.fromString(pathString)])
    await pressKey('3')
    await pressKey('0')

    const titleElement = renderResult.renderedDOM.getByTestId('title')
    expect(titleElement.style.opacity).toEqual('0.3')
  })

  const FlexDivTestId = 'flex-div'
  const AbsoluteDivTestId = 'absolute-div'
  const Child1TestId = 'child-1'

  const IndexJSFIlePath = '/app/routes/_index.js'

  const remixProjectForEditingTests = createModifiedProject({
    [StoryboardFilePath]: `import * as React from 'react'
    import { Storyboard, RemixScene } from 'utopia-api'
    
    export var storyboard = (
      <Storyboard data-uid='sb'>
        <RemixScene
          className='my-class'
          style={{
            position: 'absolute',
            width: 834,
            height: 1328,
            left: 8,
            top: -24,
            overflow: 'hidden',
          }}
          data-label='Mood Board'
          data-uid='remix-scene'
        />
      </Storyboard>
    )    
    `,
    ['/app/root.js']: `import * as React from 'react'
    import { Outlet } from '@remix-run/react'
    
    export default function App() {
      return (
        <div
          className='my-class'
          style={{
            width: '100%',
            height: '100%',
            contain: 'layout',
            transition: 'all 3s ease-out',
          }}
          data-uid='app'
        >
          <Outlet data-uid='outlet' />
        </div>
      )
    }    
    `,
    [IndexJSFIlePath]: `import * as React from 'react'
    import { Link } from '@remix-run/react'
    
    export default function Index() {
      return (
        <div
          className='my-class'
          style={{
            width: '100%',
            height: '100%',
            background: 'var(--off-white)',
            padding: '0px 0px 25px',
            boxShadow: '0px 2px 33px var(--yellow)',
            transition: 'all 3s ease-out',
          }}
          data-uid='index'
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              width: 752,
              height: 224,
              contain: 'layout',
              position: 'absolute',
              left: 46,
              top: 64,
            }}
            data-uid='absolute-div'
            data-testid='${AbsoluteDivTestId}'
          />
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              contain: 'layout',
              width: 752,
              height: 'max-content',
              position: 'absolute',
              left: 46,
              top: 340,
              display: 'flex',
              flexDirection: 'row',
              gap: 26,
              padding: '44px 20px',
            }}
            data-uid='flex-div'
            data-testid='${FlexDivTestId}'
          >
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 212,
                height: 136,
                contain: 'layout',
              }}
              data-uid='${Child1TestId}'
              data-testid='${Child1TestId}'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 212,
                height: 136,
                contain: 'layout',
              }}
              data-uid='child-2'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                width: 212,
                height: 136,
                contain: 'layout',
              }}
              data-uid='child-3'
            />
          </div>
        </div>
      )
    }    
    `,
  })

  it('set opacity on remix element from second outlet', async () => {
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
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'

      export default function Index() {
        return <h1 data-uid='title' data-testid='title'>${DefaultRouteTextContent}</h1>
      }
      `,
    })

    const renderResult = await renderRemixProject(project)

    const pathString = 'sb/rs:root/outlet2:title'

    await selectComponentsForTest(renderResult, [EP.fromString(pathString)])
    await pressKey('3')
    await pressKey('0')

    const titleElement = renderResult.renderedDOM.getByTestId('title')
    expect(titleElement.style.opacity).toEqual('0.3')
  })

  it('delete element from remix scene', async () => {
    const renderResult = await renderRemixProject(remixProjectForEditingTests)

    expect(renderResult.renderedDOM.queryAllByTestId(FlexDivTestId)).toHaveLength(1)

    await clickElementOnCanvasControlsLayer(renderResult, FlexDivTestId)
    await pressKey('Backspace')

    expect(renderResult.renderedDOM.queryAllByTestId(FlexDivTestId)).toHaveLength(0)

    expect(getPrintedUiJsCode(renderResult.getEditorState(), IndexJSFIlePath))
      .toEqual(`import * as React from 'react'
import { Link } from '@remix-run/react'

export default function Index() {
  return (
    <div
      className='my-class'
      style={{
        width: '100%',
        height: '100%',
        background: 'var(--off-white)',
        padding: '0px 0px 25px',
        boxShadow: '0px 2px 33px var(--yellow)',
        transition: 'all 3s ease-out',
      }}
      data-uid='index'
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 752,
          height: 224,
          contain: 'layout',
          position: 'absolute',
          left: 46,
          top: 64,
        }}
        data-uid='absolute-div'
        data-testid='absolute-div'
      />
    </div>
  )
}
`)
  })

  it('use the inspector to add a layout system', async () => {
    const renderResult = await renderRemixProject(remixProjectForEditingTests)

    const absoluteDiv = await clickElementOnCanvasControlsLayer(renderResult, AbsoluteDivTestId)

    const targetElement = renderResult.renderedDOM.getByTestId(AddRemoveLayoutSystemControlTestId())
    await mouseClickAtPoint(targetElement, { x: 1, y: 1 }, { modifiers: cmdModifier })

    expect(absoluteDiv.style.display).toEqual('flex')
  })

  it('flex reorder elements inside Remix', async () => {
    const renderResult = await renderRemixProject(remixProjectForEditingTests)

    await renderResult.dispatch([runDOMWalker()], true)

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/remix-scene',
        'regular-sb/remix-scene:app',
        'regular-sb/remix-scene:app/outlet',
        'regular-sb/remix-scene:app/outlet:index',
        'regular-sb/remix-scene:app/outlet:index/absolute-div',
        'regular-sb/remix-scene:app/outlet:index/flex-div',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-1',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-2',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-3',
      ],
    )

    const child1 = await clickElementOnCanvasControlsLayer(renderResult, Child1TestId)
    const child1Bounds = child1.getBoundingClientRect()
    await dragMouse(
      renderResult,
      windowPoint({ x: child1Bounds.x + 1, y: child1Bounds.y + 1 }),
      windowPoint({ x: child1Bounds.width * 1.5, y: 0 }),
    )

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/remix-scene',
        'regular-sb/remix-scene:app',
        'regular-sb/remix-scene:app/outlet',
        'regular-sb/remix-scene:app/outlet:index',
        'regular-sb/remix-scene:app/outlet:index/absolute-div',
        'regular-sb/remix-scene:app/outlet:index/flex-div',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-2',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-1', // <- child1 is the middle element after the reorder
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-3',
      ],
    )
  })

  it('absolute move elements inside Remix', async () => {
    const renderResult = await renderRemixProject(remixProjectForEditingTests)

    const absoluteDiv = await clickElementOnCanvasControlsLayer(renderResult, AbsoluteDivTestId)
    expect({ left: absoluteDiv.style.left, top: absoluteDiv.style.top }).toEqual({
      left: '46px',
      top: '64px',
    })

    const absoluteDivBounds = absoluteDiv.getBoundingClientRect()
    await dragMouse(
      renderResult,
      windowPoint({ x: absoluteDivBounds.x + 1, y: absoluteDivBounds.y + 1 }),
      windowPoint({ x: 33, y: 33 }),
    )

    expect({ left: absoluteDiv.style.left, top: absoluteDiv.style.top }).toEqual({
      left: '82px',
      top: '97px',
    })
  })

  it('a failed canvas interaction does not trash the metadata', async () => {
    // Attempt to interact with the project when no strategies are available, which should
    // then trigger a re-render with no changes to the project model
    const renderResult = await renderRemixProject(remixProjectForEditingTests, [])

    const absoluteDiv = await clickElementOnCanvasControlsLayer(renderResult, AbsoluteDivTestId)
    const styleBefore = { left: absoluteDiv.style.left, top: absoluteDiv.style.top }
    expect(styleBefore).toEqual({
      left: '46px',
      top: '64px',
    })

    const absoluteDivBounds = absoluteDiv.getBoundingClientRect()
    await dragMouse(
      renderResult,
      windowPoint({ x: absoluteDivBounds.x + 1, y: absoluteDivBounds.y + 1 }),
      windowPoint({ x: 33, y: 33 }),
    )

    await renderResult.getDispatchFollowUpActionsFinished()

    // Nothing should have changed
    expect({ left: absoluteDiv.style.left, top: absoluteDiv.style.top }).toEqual(styleBefore)

    // Ensure we have both types of metadata for all paths
    expect(Object.keys(renderResult.getEditorState().editor.spyMetadata)).toEqual([
      'sb',
      'sb/remix-scene',
      'sb/remix-scene:app',
      'sb/remix-scene:app/outlet',
      'sb/remix-scene:app/outlet:index',
      'sb/remix-scene:app/outlet:index/absolute-div',
      'sb/remix-scene:app/outlet:index/flex-div',
      'sb/remix-scene:app/outlet:index/flex-div/child-1',
      'sb/remix-scene:app/outlet:index/flex-div/child-2',
      'sb/remix-scene:app/outlet:index/flex-div/child-3',
    ])
    expect(Object.keys(renderResult.getEditorState().editor.domMetadata)).toEqual([
      'sb/remix-scene:app/outlet:index/absolute-div',
      'sb/remix-scene:app/outlet:index/flex-div/child-1',
      'sb/remix-scene:app/outlet:index/flex-div/child-2',
      'sb/remix-scene:app/outlet:index/flex-div/child-3',
      'sb/remix-scene:app/outlet:index/flex-div',
      'sb/remix-scene:app/outlet:index',
      'sb/remix-scene:app/outlet',
      'sb/remix-scene:app',
      'sb/remix-scene',
      'sb',
    ])
  })

  it('draw to insert into Remix', async () => {
    const renderResult = await renderRemixProject(remixProjectForEditingTests)

    await renderResult.dispatch([runDOMWalker()], true)

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/remix-scene',
        'regular-sb/remix-scene:app',
        'regular-sb/remix-scene:app/outlet',
        'regular-sb/remix-scene:app/outlet:index',
        'regular-sb/remix-scene:app/outlet:index/absolute-div',
        'regular-sb/remix-scene:app/outlet:index/flex-div',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-1',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-2',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-3',
      ],
    )

    await pressKey('d') // enter draw to insert mode

    const absoluteDiv = await clickElementOnCanvasControlsLayer(renderResult, AbsoluteDivTestId)
    const absoluteDivBounds = absoluteDiv.getBoundingClientRect()

    await dragMouse(
      renderResult,
      windowPoint({ x: absoluteDivBounds.x + 5, y: absoluteDivBounds.y + 5 }),
      windowPoint({ x: 12, y: 12 }),
    )

    // the new div is in there
    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState(), IndexJSFIlePath))
      .toEqual(`import * as React from 'react'
import { Link } from '@remix-run/react'

export default function Index() {
  return (
    <div
      className='my-class'
      style={{
        width: '100%',
        height: '100%',
        background: 'var(--off-white)',
        padding: '0px 0px 25px',
        boxShadow: '0px 2px 33px var(--yellow)',
        transition: 'all 3s ease-out',
      }}
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 752,
          height: 224,
          contain: 'layout',
          position: 'absolute',
          left: 46,
          top: 64,
        }}
        data-testid='absolute-div'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: -35,
            top: -35,
            width: 100,
            height: 100,
          }}
        />
      </div>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          contain: 'layout',
          width: 752,
          height: 'max-content',
          position: 'absolute',
          left: 46,
          top: 340,
          display: 'flex',
          flexDirection: 'row',
          gap: 26,
          padding: '44px 20px',
        }}
        data-testid='flex-div'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 212,
            height: 136,
            contain: 'layout',
          }}
          data-testid='child-1'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 212,
            height: 136,
            contain: 'layout',
          }}
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 212,
            height: 136,
            contain: 'layout',
          }}
        />
      </div>
    </div>
  )
}
`)
  })

  const clipboardMock = new MockClipboardHandlers().mock()

  it('copy-paste element inside Remix', async () => {
    const renderResult = await renderRemixProject(remixProjectForEditingTests)

    await selectComponentsForTest(renderResult, [
      EP.fromString('sb/remix-scene:app/outlet:index/absolute-div'),
    ])
    await pressKey('c', { modifiers: cmdModifier })
    const canvasRoot = renderResult.renderedDOM.getByTestId('canvas-root')

    firePasteEvent(canvasRoot)

    await clipboardMock.pasteDone
    await renderResult.getDispatchFollowUpActionsFinished()

    // the new div is in there
    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState(), IndexJSFIlePath))
      .toEqual(`import * as React from 'react'
import { Link } from '@remix-run/react'

export default function Index() {
  return (
    <div
      className='my-class'
      style={{
        width: '100%',
        height: '100%',
        background: 'var(--off-white)',
        padding: '0px 0px 25px',
        boxShadow: '0px 2px 33px var(--yellow)',
        transition: 'all 3s ease-out',
      }}
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 752,
          height: 224,
          contain: 'layout',
          position: 'absolute',
          left: 46,
          top: 64,
        }}
        data-testid='absolute-div'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 752,
          height: 224,
          contain: 'layout',
          position: 'absolute',
          left: 808,
          top: 64,
        }}
        data-testid='absolute-div'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          contain: 'layout',
          width: 752,
          height: 'max-content',
          position: 'absolute',
          left: 46,
          top: 340,
          display: 'flex',
          flexDirection: 'row',
          gap: 26,
          padding: '44px 20px',
        }}
        data-testid='flex-div'
      >
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 212,
            height: 136,
            contain: 'layout',
          }}
          data-testid='child-1'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 212,
            height: 136,
            contain: 'layout',
          }}
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 212,
            height: 136,
            contain: 'layout',
          }}
        />
      </div>
    </div>
  )
}
`)
  })

  it('dragging elements between Remix and the storyboard', async () => {
    const renderResult = await renderRemixProject(remixProjectForEditingTests)
    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/remix-scene',
        'regular-sb/remix-scene:app',
        'regular-sb/remix-scene:app/outlet',
        'regular-sb/remix-scene:app/outlet:index',
        'regular-sb/remix-scene:app/outlet:index/absolute-div',
        'regular-sb/remix-scene:app/outlet:index/flex-div',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-1',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-2',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-3',
      ],
    )

    {
      // Drag the element out of Remix
      const absoluteElement = await clickElementOnCanvasControlsLayer(
        renderResult,
        AbsoluteDivTestId,
      )
      const absoluteDivBounds = absoluteElement.getBoundingClientRect()

      await dragMouse(
        renderResult,
        windowPoint({ x: absoluteDivBounds.x + 1, y: absoluteDivBounds.y + 1 }),
        windowPoint({ x: 10, y: -77 }),
        emptyModifiers,
        async () =>
          renderResult.dispatch(
            [CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')],
            true,
          ),
      )
    }

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/remix-scene',
        'regular-sb/remix-scene:app',
        'regular-sb/remix-scene:app/outlet',
        'regular-sb/remix-scene:app/outlet:index',
        'regular-sb/remix-scene:app/outlet:index/flex-div',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-1',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-2',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-3',
        'regular-sb/absolute-div',
      ],
    )

    {
      // Drag the element back into Remix
      const absoluteElement = await clickElementOnCanvasControlsLayer(
        renderResult,
        AbsoluteDivTestId,
      )
      const absoluteDivBounds = absoluteElement.getBoundingClientRect()
      await dragMouse(
        renderResult,
        windowPoint({ x: absoluteDivBounds.x + 1, y: absoluteDivBounds.y + 1 }),
        windowPoint({ x: -10, y: 77 }),
        emptyModifiers,
        async () =>
          renderResult.dispatch(
            [CanvasActions.setUsersPreferredStrategy('ABSOLUTE_REPARENT')],
            true,
          ),
      )
    }

    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-sb/remix-scene',
        'regular-sb/remix-scene:app',
        'regular-sb/remix-scene:app/outlet',
        'regular-sb/remix-scene:app/outlet:index',
        'regular-sb/remix-scene:app/outlet:index/flex-div',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-1',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-2',
        'regular-sb/remix-scene:app/outlet:index/flex-div/child-3',
        'regular-sb/remix-scene:app/outlet:index/absolute-div',
      ],
    )
  })
})

describe('Canvas controls with Remix', () => {
  it('Multiselect from the same element in different scenes does not render multiselect outline', async () => {
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
      ['/app/root.js']: `import React from 'react'
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
      ['/app/routes/_index.js']: `import React from 'react'

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

    const renderResult = await renderRemixProject(project)

    // Both are the same elements (with same uid) from different Remix scenes
    const path1 = EP.fromString('storyboard/remix-scene:rootdiv/outlet:remix-div')
    const path2 = EP.fromString('storyboard/remix-scene-2:rootdiv/outlet:remix-div')

    await renderResult.dispatch([selectComponents([path1, path2], false)], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const nonResizableControl = renderResult.renderedDOM.queryByTestId(NonResizableControlTestId)
    expect(nonResizableControl).toBeNull()

    const multiselectOutlineControl =
      renderResult.renderedDOM.queryByTestId(MultiSelectOutlineTestId)
    expect(multiselectOutlineControl).toBeNull()

    const elementOutlineControl1 = await renderResult.renderedDOM.findByTestId(
      getMultiSelectElementOutlineTestId(path1),
    )
    expect(elementOutlineControl1).not.toBeNull()

    const elementOutlineControl2 = await renderResult.renderedDOM.findByTestId(
      getMultiSelectElementOutlineTestId(path2),
    )
    expect(elementOutlineControl2).not.toBeNull()
  })
})

async function clickElementOnCanvasControlsLayer(
  renderResult: EditorRenderResult,
  testId: string,
): Promise<HTMLElement> {
  const targetElement = renderResult.renderedDOM.getByTestId(testId)
  const targetElementBounds = targetElement.getBoundingClientRect()

  const clickPoint = windowPoint({ x: targetElementBounds.x + 2, y: targetElementBounds.y + 2 })
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
  await mouseClickAtPoint(canvasControlsLayer, clickPoint, { modifiers: cmdModifier })
  return targetElement
}

async function dragMouse(
  renderResult: EditorRenderResult,
  startPoint: WindowPoint,
  dragDelta: WindowPoint,
  modifiers: Modifiers = emptyModifiers,
  midDragCallback: () => Promise<void> = async () => NO_OP(),
): Promise<void> {
  const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)
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

const switchToLiveMode = (editor: EditorRenderResult) =>
  editor.dispatch([switchEditorMode(EditorModes.liveMode())], true)

const switchToEditMode = (editor: EditorRenderResult) =>
  editor.dispatch([switchEditorMode(EditorModes.selectMode(null, false, 'none'))], true)

async function clickLinkWithTestId(editor: EditorRenderResult, testId: string) {
  const targetElement = editor.renderedDOM.queryAllByTestId(testId)[0]
  const targetElementBounds = targetElement.getBoundingClientRect()

  const clickPoint = windowPoint({ x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 })
  await mouseClickAtPoint(targetElement, clickPoint)
}

async function clickRemixLink(editor: EditorRenderResult) {
  await clickLinkWithTestId(editor, 'remix-link')
  await editor.getDispatchFollowUpActionsFinished()
}

async function clickPostLink(editor: EditorRenderResult) {
  await clickLinkWithTestId(editor, 'post-link')
  await editor.getDispatchFollowUpActionsFinished()
}

async function navigateWithRemixSceneLabelButton(
  renderResult: EditorRenderResult,
  pathToRemixScene: ElementPath,
  button: RemixSceneLabelButtonType,
) {
  await mouseClickAtPoint(
    renderResult.renderedDOM.getByTestId(RemixSceneLabelButtonTestId(pathToRemixScene, button)),
    {
      x: 2,
      y: 2,
    },
  )
  await renderResult.getDispatchFollowUpActionsFinished()
}

const getPathInRemixSceneLabel = (
  renderResult: EditorRenderResult,
  pathToRemixScene: ElementPath,
) => renderResult.renderedDOM.getByTestId(RemixSceneLabelPathTestId(pathToRemixScene)).textContent

async function navigateWithRemixNavigationBarButton(
  renderResult: EditorRenderResult,
  button: RemixSceneLabelButtonType,
) {
  await mouseClickAtPoint(
    renderResult.renderedDOM.getByTestId(RemixNavigationBarButtonTestId(button)),
    {
      x: 2,
      y: 2,
    },
  )
  await renderResult.getDispatchFollowUpActionsFinished()
}

const getPathInRemixNavigationBar = (renderResult: EditorRenderResult) =>
  renderResult.renderedDOM.getByTestId(RemixNavigationBarPathTestId).textContent

function filterOutMenuLabels(htmlElement: HTMLElement): boolean {
  return !htmlElement.getAttribute('data-testid')?.startsWith('NavigatorItem')
}
