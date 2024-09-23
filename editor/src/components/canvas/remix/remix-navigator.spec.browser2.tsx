import { act } from '@testing-library/react'
import { getDomRectCenter } from '../../../core/shared/dom-utils'
import * as EP from '../../../core/shared/element-path'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../../utils/utils.test-utils'
import { runDOMWalker, selectComponents } from '../../editor/actions/action-creators'
import {
  StoryboardFilePath,
  navigatorEntryToKey,
  regularNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../../editor/store/editor-state'
import type { PersistentModel } from '../../editor/store/editor-state'
import { NavigatorItemTestId } from '../../navigator/navigator-item/navigator-item'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { dragElementWithDNDEvents } from '../event-helpers.test-utils'
import { windowPoint } from '../../../core/shared/math-utils'
import { RemixIndexPathLabel } from './remix-utils'
import { getNavigatorTargetsFromEditorState } from '../../navigator/navigator-utils'

const DefaultRouteTextContent = 'Hello Remix!'
const RootTextContent = 'This is root!'

async function renderRemixProject(project: PersistentModel) {
  const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')
  await renderResult.dispatch([runDOMWalker()], true)
  return renderResult
}

describe('Remix navigator', () => {
  it('Shows navigator for remix content', async () => {
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
      getNavigatorTargetsFromEditorState(renderResult.getEditorState().editor).navigatorTargets.map(
        navigatorEntryToKey,
      ),
    ).toEqual([
      'regular-storyboard/remix-scene',
      'regular-storyboard/remix-scene:rootdiv',
      'regular-storyboard/remix-scene:rootdiv/outlet',
      'regular-storyboard/remix-scene:rootdiv/outlet:remix-div',
    ])
  })
  it('Shows navigator for remix content with two outlets', async () => {
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
    expect(
      getNavigatorTargetsFromEditorState(renderResult.getEditorState().editor).navigatorTargets.map(
        navigatorEntryToKey,
      ),
    ).toEqual([
      'regular-storyboard/remix-scene',
      'regular-storyboard/remix-scene:rootdiv',
      'regular-storyboard/remix-scene:rootdiv/outlet',
      'regular-storyboard/remix-scene:rootdiv/outlet:remix-div',
      'regular-storyboard/remix-scene:rootdiv/outlet2',
      'regular-storyboard/remix-scene:rootdiv/outlet2:remix-div',
    ])
  })
  it('Navigator for remix content has default color', async () => {
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
    const navigatorItemElement = renderResult.renderedDOM.getByTestId(
      NavigatorItemTestId(
        varSafeNavigatorEntryToKey(
          regularNavigatorEntry(EP.fromString('storyboard/remix-scene:rootdiv/outlet:remix-div')),
        ),
      ),
    )
    expect(navigatorItemElement.style.color).toEqual('var(--utopitheme-fg0)')
  })

  it('Remix Outlet navigator item label contains the suffix of the full path', async () => {
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
    const outletItemElement = renderResult.renderedDOM.getByTestId(
      NavigatorItemTestId(
        varSafeNavigatorEntryToKey(
          regularNavigatorEntry(EP.fromString('storyboard/remix-scene:rootdiv/outlet')),
        ),
      ),
    )
    expect(outletItemElement.textContent).toEqual(`Outlet: ${RemixIndexPathLabel}`)
  })
})
describe('Reparenting in Remix projects in the navigator', () => {
  it('Can reparent into element in Remix', async () => {
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
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 500,
              top: 500,
              width: 88,
              height: 91,
            }}
            data-uid='drag-div'
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
        />
      }
      `,
    })

    const renderResult = await renderRemixProject(project)

    // the element we drag
    const dragElementTestId = NavigatorItemTestId(
      varSafeNavigatorEntryToKey(regularNavigatorEntry(EP.fromString('storyboard/drag-div'))),
    )
    const dragDivElement = await renderResult.renderedDOM.findByTestId(dragElementTestId)
    const dragElementRect = dragDivElement.getBoundingClientRect()
    const dragElementCenter = getDomRectCenter(dragElementRect)

    const dropElementTestId = NavigatorItemTestId(
      varSafeNavigatorEntryToKey(
        regularNavigatorEntry(EP.fromString('storyboard/remix-scene:rootdiv/outlet:remix-div')),
      ),
    )

    const dropElement = renderResult.renderedDOM.getByTestId(dropElementTestId)
    const dropElementRect = dropElement.getBoundingClientRect()
    const dropElementCenter = getDomRectCenter(dropElementRect)

    const dragDelta = windowPoint({
      x: dropElementCenter.x - dragElementCenter.x,
      y: dropElementCenter.y - dragElementCenter.y,
    })

    const targetElement = EP.fromString('storyboard/drag-div')
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    await dragElementWithDNDEvents(
      renderResult,
      dragElementTestId,
      dropElementTestId,
      windowPoint(dragElementCenter),
      dragDelta,
    )

    expect(
      getNavigatorTargetsFromEditorState(renderResult.getEditorState().editor).navigatorTargets.map(
        navigatorEntryToKey,
      ),
    ).toEqual([
      'regular-storyboard/remix-scene',
      'regular-storyboard/remix-scene:rootdiv',
      'regular-storyboard/remix-scene:rootdiv/outlet',
      'regular-storyboard/remix-scene:rootdiv/outlet:remix-div',
      'regular-storyboard/remix-scene:rootdiv/outlet:remix-div/drag-div', // <-- reparented here from storyboard
    ])
  })

  it('Can not reparent into Remix Outlet', async () => {
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
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 500,
              top: 500,
              width: 88,
              height: 91,
            }}
            data-uid='drag-div'
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
        />
      }
      `,
    })

    const renderResult = await renderRemixProject(project)

    // the element we drag
    const dragElementTestId = NavigatorItemTestId(
      varSafeNavigatorEntryToKey(regularNavigatorEntry(EP.fromString('storyboard/drag-div'))),
    )
    const dragDivElement = await renderResult.renderedDOM.findByTestId(dragElementTestId)
    const dragElementRect = dragDivElement.getBoundingClientRect()
    const dragElementCenter = getDomRectCenter(dragElementRect)

    const dropElementTestId = NavigatorItemTestId(
      varSafeNavigatorEntryToKey(
        regularNavigatorEntry(EP.fromString('storyboard/remix-scene:rootdiv/outlet')),
      ),
    )

    const dropElement = renderResult.renderedDOM.getByTestId(dropElementTestId)
    const dropElementRect = dropElement.getBoundingClientRect()
    const dropElementCenter = getDomRectCenter(dropElementRect)

    const dragDelta = windowPoint({
      x: dropElementCenter.x - dragElementCenter.x,
      y: dropElementCenter.y - dragElementCenter.y,
    })

    const targetElement = EP.fromString('storyboard/drag-div')
    await act(async () => {
      const dispatchDone = renderResult.getDispatchFollowUpActionsFinished()
      await renderResult.dispatch([selectComponents([targetElement], false)], false)
      await dispatchDone
    })

    await dragElementWithDNDEvents(
      renderResult,
      dragElementTestId,
      dropElementTestId,
      windowPoint(dragElementCenter),
      dragDelta,
    )

    expect(
      getNavigatorTargetsFromEditorState(renderResult.getEditorState().editor).navigatorTargets.map(
        navigatorEntryToKey,
      ),
    ).toEqual([
      'regular-storyboard/remix-scene',
      'regular-storyboard/remix-scene:rootdiv',
      'regular-storyboard/remix-scene:rootdiv/outlet',
      'regular-storyboard/remix-scene:rootdiv/outlet:remix-div',
      'regular-storyboard/drag-div', // <-- not reparented
    ])
  })
})
