import * as EP from '../../../core/shared/element-path'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../../utils/utils.test-utils'
import {
  StoryboardFilePath,
  navigatorEntryToKey,
  regularNavigatorEntry,
  varSafeNavigatorEntryToKey,
} from '../../editor/store/editor-state'
import { NavigatorItemTestId } from '../../navigator/navigator-item/navigator-item'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'

const DefaultRouteTextContent = 'Hello Remix!'
const RootTextContent = 'This is root!'

describe('Remix navigator', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)
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
    expect(renderResult.getEditorState().derived.navigatorTargets.map(navigatorEntryToKey)).toEqual(
      [
        'regular-storyboard/remix-scene',
        'regular-storyboard/remix-scene:rootdiv',
        'regular-storyboard/remix-scene:rootdiv/outlet',
        'regular-storyboard/remix-scene:rootdiv/outlet:remix-div',
      ],
    )
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
    const navigatorItemElement = renderResult.renderedDOM.getByTestId(
      NavigatorItemTestId(
        varSafeNavigatorEntryToKey(
          regularNavigatorEntry(EP.fromString('storyboard/remix-scene:rootdiv/outlet:remix-div')),
        ),
      ),
    )
    expect(navigatorItemElement.style.color).toEqual('var(--utopitheme-fg0)')
  })
})
