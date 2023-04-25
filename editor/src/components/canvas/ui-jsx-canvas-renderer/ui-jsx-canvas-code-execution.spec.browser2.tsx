import {
  isParseSuccess,
  ParsedTextFile,
  RevisionsState,
  textFile,
  textFileContents,
} from '../../../core/shared/project-file-types'
import { emptySet } from '../../../core/shared/set-utils'
import { lintAndParse } from '../../../core/workers/parser-printer/parser-printer'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { updateFile } from '../../editor/actions/action-creators'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'

const indirectFilePath = '/src/indirect.js'
const indirectDependencyValueBefore = 'Initial indirect dependency value'
const indirectFileContent = `export const IndirectDependencyValue = '${indirectDependencyValueBefore}'`

const directFilePath = '/src/direct.js'
const directFileContent = `
import { IndirectDependencyValue } from '${indirectFilePath}'

export const DirectDependencyValue = 'IndirectDependencyValue: ' + IndirectDependencyValue
`

const appFilePath = '/src/app.js'
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
  registerModule,
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
    const { dispatch, renderedDOM } = await createAndRenderProject()

    const appRootDivBefore = renderedDOM.getByTestId('app-root-div')
    expect(appRootDivBefore.innerText).toEqual(
      `IndirectDependencyValue: ${indirectDependencyValueBefore}`,
    )

    const indirectDependencyValueAfter = 'Updated indirect dependency value'
    const updatedIndirectFileContent = `export const IndirectDependencyValue = '${indirectDependencyValueAfter}'`

    const updatedIndirectFileParsedTextFile = lintAndParse(
      indirectFilePath,
      updatedIndirectFileContent,
      null,
      emptySet(),
      'trim-bounds',
    ) as ParsedTextFile

    const updatedIndirectFile = textFile(
      textFileContents(
        updatedIndirectFileContent,
        updatedIndirectFileParsedTextFile,
        RevisionsState.BothMatch,
      ),
      null,
      isParseSuccess(updatedIndirectFileParsedTextFile) ? updatedIndirectFileParsedTextFile : null,
      Date.now(),
    )

    await dispatch([updateFile(indirectFilePath, updatedIndirectFile, false)], true)

    const appRootDivAfter = renderedDOM.getByTestId('app-root-div')
    expect(appRootDivAfter.innerText).toEqual(
      `IndirectDependencyValue: ${indirectDependencyValueAfter}`,
    )
  })
})
