import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import * as EP from '../../../core/shared/element-path'
import { FOR_TESTS_setNextGeneratedUids } from '../../../core/model/element-template-utils.test-utils'

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
      {[0, 1].length > 1 ? (
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
  })
  return renderTestEditorWithModel(project, 'await-first-dom-report')
}

describe('a project with conditionals', () => {
  it('fills the content of the navigator', async () => {
    FOR_TESTS_setNextGeneratedUids([
      'mock1',
      'conditional2',
      'mock2',
      'conditional1',
      'mock1',
      'conditional2',
      'mock2',
      'conditional1',
    ])
    const renderedProject = await createAndRenderProject()
    const navigatorTargets = renderedProject.getEditorState().derived.navigatorTargets
    const pathStrings = navigatorTargets.map(EP.toString)
    expect(pathStrings).toEqual([
      'storyboard/scene',
      'storyboard/scene/app',
      'storyboard/scene/app:app-root',
      'storyboard/scene/app:app-root/conditional1',
      'storyboard/scene/app:app-root/conditional1/conditional2',
      'storyboard/scene/app:app-root/conditional1/conditional2/div-inside-conditionals',
    ])
  })
})
