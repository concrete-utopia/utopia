import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { navigatorEntryToKey, StoryboardFilePath } from '../../editor/store/editor-state'
import { getNavigatorTargetsFromEditorState } from '../../navigator/navigator-utils'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'

const appFilePath = '/src/app.js'
const appFileContent = `
import * as React from 'react'
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
      {
        // @utopia/uid=conditional1
        [0, 1].length > 1 ? (
            // @utopia/uid=conditional2
            [0, 1].length === 2 ? (
            <div data-uid='div-inside-conditionals' />
          ) : null
        ) : (
          5
        )}
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
  })
  return renderTestEditorWithModel(project, 'await-first-dom-report')
}

describe('a project with conditionals', () => {
  it('fills the content of the navigator', async () => {
    const renderedProject = await createAndRenderProject()
    const navigatorTargets = getNavigatorTargetsFromEditorState(
      renderedProject.getEditorState().editor,
    ).visibleNavigatorTargets
    const pathStrings = navigatorTargets.map(navigatorEntryToKey)
    expect(pathStrings).toEqual([
      'regular-storyboard/scene',
      'regular-storyboard/scene/app',
      'regular-storyboard/scene/app:app-root',
      'regular-storyboard/scene/app:app-root/conditional1',
      'conditional-clause-storyboard/scene/app:app-root/conditional1-true-case',
      'regular-storyboard/scene/app:app-root/conditional1/conditional2',
      'conditional-clause-storyboard/scene/app:app-root/conditional1/conditional2-true-case',
      'regular-storyboard/scene/app:app-root/conditional1/conditional2/div-inside-conditionals',
      'conditional-clause-storyboard/scene/app:app-root/conditional1/conditional2-false-case',
      'synthetic-storyboard/scene/app:app-root/conditional1/conditional2/eff-attribute',
      'conditional-clause-storyboard/scene/app:app-root/conditional1-false-case',
      'synthetic-storyboard/scene/app:app-root/conditional1/d50-attribute',
    ])
  })
})
