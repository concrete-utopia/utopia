require('jest-fetch-mock').enableMocks()
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { notLoggedIn } from '../../editor/action-types'
import {
  resetCanvas,
  runDOMWalker,
  setErrorBoundaryHandling,
  switchEditorMode,
} from '../../editor/actions/action-creators'
import { EditorModes } from '../../editor/editor-modes'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import type { ErrorBoundaryHandling, PersistentModel } from '../../editor/store/editor-state'
import { RegisteredCanvasStrategies } from '../canvas-strategies/canvas-strategies'
import { mouseClickAtPoint } from '../event-helpers.test-utils'
import { DefaultStartingFeatureSwitches, renderTestEditorWithModel } from '../ui-jsx.test-utils'
import type { EditorRenderResult } from '../ui-jsx.test-utils'

export async function renderRemixProject(project: PersistentModel) {
  const renderResult = await renderTestEditorWithModel(
    project,
    'await-first-dom-report',
    DefaultStartingFeatureSwitches,
    undefined,
    RegisteredCanvasStrategies,
    notLoggedIn,
    false,
  )
  await renderResult.dispatch([runDOMWalker()], true)
  return renderResult
}

async function clickLinkWithTestId(editor: EditorRenderResult, testId: string) {
  const targetElement = editor.renderedDOM.getByTestId(testId)
  const targetElementBounds = targetElement.getBoundingClientRect()

  await mouseClickAtPoint(targetElement, {
    x: targetElementBounds.x + 5,
    y: targetElementBounds.y + 5,
  })
}

const CustomBoundaryText = 'I am a custom error boundary'
const ErrorText = 'CATCH ME IF YOU CAN!'

function createProject(withBoundary: boolean) {
  return createModifiedProject({
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
        />
      </Storyboard>
    )
    `,
    ['/app/root.js']: `import React from 'react'
    import { Outlet } from '@remix-run/react'
    
    export default function Root() {
      return (
        <div>
          <Outlet />
        </div>
      )
    }
    `,
    ['/app/routes/_index.js']: `import React from 'react'
    import { Link } from '@remix-run/react'
    
    export default function Index() {
      return (
        <Link
          style={{
            width: 100,
            height: 100
          }}
          to='/thrower'
          data-testid='remix-link'
        >
          Throw!
        </Link>
      )
    }
    `,
    ['/app/routes/thrower.js']: `import React from 'react'
    import { useRouteError } from '@remix-run/react'
    
    ${
      withBoundary
        ? `
      export function ErrorBoundary() {
        const error = useRouteError()
        return <div>${CustomBoundaryText}</div>
      }
      `
        : ``
    }
    
    export default function Thrower() {
      throw new Error('${ErrorText}')
      return <h1>I won't render</h1>
    }
    `,
  })
}

export function createMaybeFailingProject(induceFailure: boolean): PersistentModel {
  return createModifiedProject({
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
        />
      </Storyboard>
    )
    `,
    ['/app/root.js']: `import React from 'react'
    import { Outlet } from '@remix-run/react'
    
    export default function Root() {
      return (
        <div>
          <Outlet />
        </div>
      )
    }
    `,
    ['/app/routes/_index.js']: `import React from 'react'
    import { Link } from '@remix-run/react'
    
    export default function Index() {
      ${induceFailure ? '' : '// '}throw new Error('Failure')
      return (
        <span>Index Content</span>
      )
    }
    `,
  })
}

export async function runTestReturningErrorBoundaries(
  withCustomBoundary: 'with-custom-boundary' | 'without-custom-boundary',
  errorBoundaryHandling: ErrorBoundaryHandling,
): Promise<{ customBoundary: HTMLElement | null; canvasOverlay: HTMLElement | null }> {
  const project = createProject(withCustomBoundary === 'with-custom-boundary')

  const renderResult = await renderRemixProject(project)

  await renderResult.dispatch(
    [
      setErrorBoundaryHandling(errorBoundaryHandling),
      resetCanvas(),
      switchEditorMode(EditorModes.liveMode()),
    ],
    true,
  )
  await clickLinkWithTestId(renderResult, 'remix-link')

  const customBoundary = renderResult.renderedDOM.queryByText(CustomBoundaryText)
  const canvasOverlay = renderResult.renderedDOM.queryByText(`Error: ${ErrorText}`)

  return {
    customBoundary: customBoundary,
    canvasOverlay: canvasOverlay,
  }
}
