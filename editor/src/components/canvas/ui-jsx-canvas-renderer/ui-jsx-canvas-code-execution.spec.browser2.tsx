import { BakedInStoryboardUID } from '../../../core/model/scene-utils'
import { forceNotNull } from '../../../core/shared/optional-utils'
import type { ParsedTextFile, TextFile } from '../../../core/shared/project-file-types'
import {
  isParseSuccess,
  RevisionsState,
  textFile,
  textFileContents,
} from '../../../core/shared/project-file-types'
import { emptySet } from '../../../core/shared/set-utils'
import { lintAndParse } from '../../../core/workers/parser-printer/parser-printer'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { getProjectFileByFilePath } from '../../assets'
import { switchEditorMode, updateFile } from '../../editor/actions/action-creators'
import { updateFromCodeEditor } from '../../editor/actions/actions-from-vscode'
import { EditorModes } from '../../editor/editor-modes'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import { mouseClickAtPoint } from '../event-helpers.test-utils'
import {
  type EditorRenderResult,
  renderTestEditorWithCode,
  renderTestEditorWithModel,
  TestAppUID,
  TestSceneUID,
} from '../ui-jsx.test-utils'

const indirectFilePath = '/app/indirect.js'
const indirectDependencyValueBefore = 'Initial indirect dependency value'
const indirectFileContent = `export const IndirectDependencyValue = '${indirectDependencyValueBefore}'`

const directFilePath = '/app/direct.js'
const directFileContent = `
import { IndirectDependencyValue } from '${indirectFilePath}'

export const DirectDependencyValue = 'IndirectDependencyValue: ' + IndirectDependencyValue
`

const appFilePath = '/app/app.js'
const appFileContent = `
import * as React from 'react'
import { DirectDependencyValue } from '${directFilePath}'
export var App = (props) => {
  return (
    <div
      data-testid='app-root-div'
      data-uid='app-root'
      style={{
        position: 'relative',
        left: 0,
        top: 0,
        width: '100%',
        height: '100%'
      }}
    >
      { DirectDependencyValue }
    </div>
  )
}
`

const storyboardFileContent = `
import * as React from 'react';
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api';
import { App } from '${appFilePath}';

export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      data-uid='scene'
      style={{ position: 'absolute', left: 400, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='app' />
    </Scene>
  </Storyboard>
);
`

async function createAndRenderProject() {
  const project = createModifiedProject({
    [StoryboardFilePath]: storyboardFileContent,
    [appFilePath]: appFileContent,
    [directFilePath]: directFileContent,
    [indirectFilePath]: indirectFileContent,
  })
  return renderTestEditorWithModel(project, 'await-first-dom-report')
}

describe('Updating a transitive dependency', () => {
  it('Updates the rendered result', async () => {
    const { dispatch, getEditorState, renderedDOM } = await createAndRenderProject()

    const appRootDivBefore = renderedDOM.getByTestId('app-root-div')
    expect(appRootDivBefore.innerText).toEqual(
      `IndirectDependencyValue: ${indirectDependencyValueBefore}`,
    )

    const indirectDependencyValueAfter = 'Updated indirect dependency value'
    const updatedIndirectFileContent = `export const IndirectDependencyValue = '${indirectDependencyValueAfter}'`

    const updatedIndirectFileParsedTextFile = lintAndParse(
      indirectFilePath,
      [],
      updatedIndirectFileContent,
      null,
      emptySet(),
      'trim-bounds',
      'do-not-apply-steganography',
    ) as ParsedTextFile

    const oldFile = forceNotNull(
      'Unexpectedly null.',
      getProjectFileByFilePath(getEditorState().editor.projectContents, indirectFilePath),
    )

    const updatedIndirectFile = textFile(
      textFileContents(
        updatedIndirectFileContent,
        updatedIndirectFileParsedTextFile,
        RevisionsState.BothMatch,
      ),
      null,
      isParseSuccess(updatedIndirectFileParsedTextFile) ? updatedIndirectFileParsedTextFile : null,
      (oldFile as TextFile).versionNumber + 1,
    )

    await dispatch([updateFile(indirectFilePath, updatedIndirectFile, false)], true)

    const appRootDivAfter = renderedDOM.getByTestId('app-root-div')
    expect(appRootDivAfter.innerText).toEqual(
      `IndirectDependencyValue: ${indirectDependencyValueAfter}`,
    )
  })
})

describe('Re-mounting is avoided when', () => {
  const switchToLiveMode = (editor: EditorRenderResult) =>
    editor.dispatch([switchEditorMode(EditorModes.liveMode())], true)

  const clickButton = async (editor: EditorRenderResult) => {
    const targetElement = editor.renderedDOM.getByTestId('clicky')
    const targetElementBounds = targetElement.getBoundingClientRect()

    const clickPoint = { x: targetElementBounds.x + 5, y: targetElementBounds.y + 5 }
    await mouseClickAtPoint(targetElement, clickPoint)
  }

  const updateCode = async (editor: EditorRenderResult, filePath: string, newCode: string) => {
    await editor.dispatch([updateFromCodeEditor(filePath, newCode, null)], true)
    await editor.getDispatchFollowUpActionsFinished()
  }

  const startingJsConstText = 'Clicked '
  const updatedJsConstText = 'Clicked: '

  const startingComponentText = '{text}{n} times'
  const updatedComponentText = '{text}{n} times!'

  function getClickerCode(jsConstText: string, componentText: string): string {
    return `
      import * as React from 'react'

      const text = '${jsConstText}'

      export const TextElem = () => {
        const [n, setN] = React.useState(0)
        const click = React.useCallback(
          () => setN((v) => v + 1),
          [setN],
        )

        return (
          <div
            style={{
              position: 'absolute',
              left: 0,
              top: 0,
              width: 200,
              height: 200,
            }}
            onMouseDown={click}
            data-testid='clicky'
          >
            ${componentText}
          </div>
        )
      }

      export default function Clicker() {
        return <TextElem />
      }
    `
  }

  it('arbitrary JS or a component is edited in a regular project', async () => {
    const clickerComponentFile = '/app/clicker.js'

    const project = createModifiedProject({
      [StoryboardFilePath]: `
      import * as React from 'react'
      import { Storyboard } from 'utopia-api'
      import Clicker from '${clickerComponentFile}'

      export var storyboard = (
        <Storyboard>
          <Clicker />
        </Storyboard>
      )
      `,
      [clickerComponentFile]: getClickerCode(startingJsConstText, startingComponentText),
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    await switchToLiveMode(renderResult)

    // Ensure we can find the original text
    expect(renderResult.renderedDOM.queryByText('Clicked 0 times')).not.toBeNull()

    await clickButton(renderResult)

    // Ensure it has been updated
    expect(renderResult.renderedDOM.queryByText('Clicked 1 times')).not.toBeNull()

    // Update the top level arbitrary JS block
    await updateCode(
      renderResult,
      clickerComponentFile,
      getClickerCode(updatedJsConstText, startingComponentText),
    )

    // Check that it has updated without resetting the state
    expect(renderResult.renderedDOM.queryByText('Clicked: 1 times')).not.toBeNull()

    // Update the component itself
    await updateCode(
      renderResult,
      clickerComponentFile,
      getClickerCode(updatedJsConstText, updatedComponentText),
    )

    // Check again that it has updated without resetting the state
    expect(renderResult.renderedDOM.queryByText('Clicked: 1 times!')).not.toBeNull()
  })

  it('arbitrary JS or a component is edited in a remix project', async () => {
    const clickerRouteFile = '/app/routes/_index.js'

    const project = createModifiedProject({
      [StoryboardFilePath]: `import * as React from 'react'
      import { RemixScene, Storyboard } from 'utopia-api'

      export var storyboard = (
        <Storyboard>
          <RemixScene
            style={{
              position: 'absolute',
              left: 0,
              top: 0,
              width: 200,
              height: 200,
            }}
          />
        </Storyboard>
      )
      `,
      ['/app/root.js']: `import React from 'react'
      import { Outlet } from '@remix-run/react'

      export default function Root() {
        return (
          <div data-uid='rootdiv'>
            <Outlet data-uid='outlet'/>
          </div>
        )
      }
      `,
      [clickerRouteFile]: getClickerCode(startingJsConstText, startingComponentText),
    })

    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')

    await switchToLiveMode(renderResult)

    // Ensure we can find the original text
    expect(renderResult.renderedDOM.queryByText('Clicked 0 times')).not.toBeNull()

    await clickButton(renderResult)

    // Ensure it has been updated
    expect(renderResult.renderedDOM.queryByText('Clicked 1 times')).not.toBeNull()

    // Update the top level arbitrary JS block
    await updateCode(
      renderResult,
      clickerRouteFile,
      getClickerCode(updatedJsConstText, startingComponentText),
    )

    // Check that it has updated without resetting the state
    expect(renderResult.renderedDOM.queryByText('Clicked: 1 times')).not.toBeNull()

    // Update the component itself
    await updateCode(
      renderResult,
      clickerRouteFile,
      getClickerCode(updatedJsConstText, updatedComponentText),
    )

    // Check again that it has updated without resetting the state
    expect(renderResult.renderedDOM.queryByText('Clicked: 1 times!')).not.toBeNull()
  })
})

describe('Updating the scope when variables change in arbitrary JS', () => {
  it('should update the scope when a variable is reassigned', async () => {
    const renderResult = await renderWithAppCode(`
    let title='Wrong Title'
    export var App = (props) => {
      title='Correct Title'
      return (
        <div data-testid='title'>{title}</div>
      )
    }
    `)

    const title = renderResult.renderedDOM.getByTestId('title')
    expect(title.textContent).toEqual('Correct Title')
  })

  it('should update the scope when the entire prop variable is reassigned', async () => {
    const renderResult = await renderWithAppCode(`
    export var App = (props) => {
      props={title:'Correct Title'}
      return (
        <div data-testid='title'>{props.title}</div>
      )
    }
    `)

    const title = renderResult.renderedDOM.getByTestId('title')
    expect(title.textContent).toEqual('Correct Title')
  })

  it('should update the scope when a variable is reassigned inside the props object', async () => {
    const renderResult = await renderWithAppCode(`
    export var App = (props) => {
      props.title='Correct Title'
      return (
        <div data-testid='title'>{props.title}</div>
      )
    }
    `)

    const title = renderResult.renderedDOM.getByTestId('title')
    expect(title.textContent).toEqual('Correct Title')
  })
})

function renderWithAppCode(appCode: string) {
  return renderTestEditorWithCode(
    `
    import * as React from 'react'
    import {
      Scene,
      Storyboard,
    } from 'utopia-api'
     
    ${appCode}

    export var storyboard = (props) => {
      return (
        <Storyboard data-uid={'${BakedInStoryboardUID}'}>
          <Scene
            style={{ position: 'relative', left: 0, top: 0, width: 375, height: 812 }}
            data-uid={'${TestSceneUID}'}
          >
            <App
              data-uid='${TestAppUID}' 
              style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
              title='Initial Title'
            />
          </Scene>
        </Storyboard>
      )
    }
  `,
    'await-first-dom-report',
  )
}
