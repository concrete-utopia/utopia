import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { mouseClickAtPoint } from '../../canvas/event-helpers.test-utils'
import { renderTestEditorWithModel } from '../../canvas/ui-jsx.test-utils'
import { runDOMWalker, updateProjectServerState } from '../../editor/actions/action-creators'
import type { PersistentModel } from '../../editor/store/editor-state'
import { StoryboardFilePath } from '../../editor/store/editor-state'

async function renderRemixProject(project: PersistentModel) {
  const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')
  await renderResult.dispatch([runDOMWalker(null)], true)
  return renderResult
}

describe('Settings Pane', () => {
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
          Hello root
          <Outlet data-uid='outlet' />
        </div>
      )
    }
    `,
    ['/app/routes/_index.js']: `import React from 'react'

    export default function Index() {
      return <h1 data-uid='title' data-testid='title'>Hello Route</h1>
    }
    `,
  })

  it('Project name is editable if the user can edit project', async () => {
    const renderResult = await renderRemixProject(project)
    await renderResult.dispatch([updateProjectServerState({ isMyProject: 'yes' })], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const settingsTabButton = await renderResult.renderedDOM.findByText('Settings')

    await mouseClickAtPoint(settingsTabButton, { x: 3, y: 3 })

    const projectNameInputField = (await renderResult.renderedDOM.findByTestId(
      'projectName',
    )) as HTMLInputElement

    expect(projectNameInputField.value).toEqual('Test')
    expect(projectNameInputField.disabled).toEqual(false)
  })

  it('Project name is not editable if the user can not edit project', async () => {
    const renderResult = await renderRemixProject(project)
    await renderResult.dispatch([updateProjectServerState({ isMyProject: 'no' })], true)
    await renderResult.getDispatchFollowUpActionsFinished()

    const settingsTabButton = await renderResult.renderedDOM.findByText('Settings')

    await mouseClickAtPoint(settingsTabButton, { x: 3, y: 3 })

    const projectNameInputField = (await renderResult.renderedDOM.findByTestId(
      'projectName',
    )) as HTMLInputElement

    expect(projectNameInputField.value).toEqual('Test')
    expect(projectNameInputField.disabled).toEqual(true)
  })
})
