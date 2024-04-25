/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "checkNavigationInputValue", "checkTextContentOfPage", "checkFavouriteRoutes"] }] */
import { createModifiedProject } from '../../sample-projects/sample-project-utils.test-utils'
import { RemixIndexPathLabel } from '../canvas/remix/remix-utils'
import type { EditorRenderResult } from '../canvas/ui-jsx.test-utils'
import { renderTestEditorWithModel } from '../canvas/ui-jsx.test-utils'
import { runDOMWalker } from './actions/action-creators'
import type { PersistentModel } from './store/editor-state'
import { StoryboardFilePath } from './store/editor-state'
import { RemixNavigationBarPathTestId } from './remix-navigation-bar'
import { fireEvent, waitFor } from '@testing-library/react'
import { defaultEither } from '../../core/shared/either'
import { getFeaturedRoutesFromPackageJSON } from '../../printer-parsers/html/external-resources-parser'

async function renderRemixProject(project: PersistentModel): Promise<EditorRenderResult> {
  const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')
  await renderResult.dispatch([runDOMWalker()], true)
  return renderResult
}

function getDefaultProject(): PersistentModel {
  return createModifiedProject({
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
            Outlet: 
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
          data-uid='index-div'
          data-testid='index-div'
        >
          Index Content 
        </div>
      }
      `,
    ['/app/routes/somepage.js']: `import React from 'react'

      export default function SomePage() {
        return <div
          style={{
            width: 200,
            height: 200,
            position: 'absolute',
            left: 0,
            top: 0,
          }}
          data-uid='somepage-div'
          data-testid='somepage-div'
        >
          Some Page 
        </div>
      }
      `,
  })
}

async function checkTextContentOfPage(
  renderResult: EditorRenderResult,
  elementTestId: string,
  expectedContent: string,
): Promise<void> {
  const outletItemElement = await waitFor(() => renderResult.renderedDOM.getByTestId(elementTestId))
  expect(outletItemElement.textContent?.trim()).toEqual(expectedContent)
}

function checkNavigationInputValue(renderResult: EditorRenderResult, expectedValue: string): void {
  const navigationInput = renderResult.renderedDOM.getByTestId(
    `${RemixNavigationBarPathTestId}-input`,
  ) as HTMLInputElement
  expect(navigationInput.value).toEqual(expectedValue)
}

async function setNavigationInputValue(
  renderResult: EditorRenderResult,
  value: string,
): Promise<void> {
  const navigationInput = renderResult.renderedDOM.getByTestId(
    `${RemixNavigationBarPathTestId}-input`,
  ) as HTMLInputElement

  fireEvent.change(navigationInput, { target: { value: value } })
  fireEvent.blur(navigationInput)
  fireEvent.keyDown(navigationInput, { key: 'Enter', keyCode: 13 })

  await renderResult.getDispatchFollowUpActionsFinished()
}

async function pressStar(renderResult: EditorRenderResult): Promise<void> {
  const star = renderResult.renderedDOM.getByTestId(`${RemixNavigationBarPathTestId}-star`)

  fireEvent.click(star)

  await renderResult.getDispatchFollowUpActionsFinished()
}

function checkFavouriteRoutes(renderResult: EditorRenderResult, expectedRoutes: string[]): void {
  const currentRoutes = defaultEither(
    [],
    getFeaturedRoutesFromPackageJSON(renderResult.getEditorState().editor.projectContents),
  )
  expect(currentRoutes).toEqual(expectedRoutes)
}

describe('Remix navigation bar', () => {
  describe('url bar', () => {
    it('should allow switching between routes', async () => {
      const renderResult = await renderRemixProject(getDefaultProject())

      await checkTextContentOfPage(renderResult, `index-div`, `Index Content`)

      checkNavigationInputValue(renderResult, `${RemixIndexPathLabel}`)

      await setNavigationInputValue(renderResult, '/somepage')

      await checkTextContentOfPage(renderResult, `somepage-div`, `Some Page`)

      checkNavigationInputValue(renderResult, `/somepage`)

      await setNavigationInputValue(renderResult, `${RemixIndexPathLabel}`)

      await checkTextContentOfPage(renderResult, `index-div`, `Index Content`)

      checkNavigationInputValue(renderResult, `${RemixIndexPathLabel}`)
    })
    it('should prevent switching to a path with a full domain', async () => {
      const renderResult = await renderRemixProject(getDefaultProject())

      await setNavigationInputValue(renderResult, 'http://www.sausages.com')

      expect(renderResult.getEditorState().editor.toasts.map((toast) => toast.message)).toEqual([
        `You can't edit this yet. Please use a path without a domain name.`,
      ])
    })
    it('should prevent switching to a path which does not exist', async () => {
      const renderResult = await renderRemixProject(getDefaultProject())

      await setNavigationInputValue(renderResult, '/sausages')

      expect(renderResult.getEditorState().editor.toasts.map((toast) => toast.message)).toEqual([
        `Route does not exist.`,
      ])
    })
    it('should prevent switching to a path which looks like gibberish', async () => {
      const renderResult = await renderRemixProject(getDefaultProject())

      await setNavigationInputValue(renderResult, '---')

      expect(renderResult.getEditorState().editor.toasts.map((toast) => toast.message)).toEqual([
        `Route does not exist.`,
      ])
    })
  })
  describe('favourite star', () => {
    it('adds and removes an entry from the favourites', async () => {
      const renderResult = await renderRemixProject(getDefaultProject())

      await setNavigationInputValue(renderResult, '/somepage')

      await pressStar(renderResult)

      checkFavouriteRoutes(renderResult, ['/somepage'])

      await pressStar(renderResult)

      checkFavouriteRoutes(renderResult, [])
    })
  })
})
