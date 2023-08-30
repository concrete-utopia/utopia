import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../../utils/utils.test-utils'
import { StoryboardFilePath } from '../../editor/store/editor-state'
import { REMIX_SCENE_TESTID } from '../ui-jsx-canvas-renderer/remix-scene-component'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'

const storyboardFileContent = `
import * as React from 'react';
import Utopia, {
  Scene,
  Storyboard,
  RemixScene,
} from 'utopia-api';


export var storyboard = (
  <Storyboard data-uid='storyboard'>
    <Scene
      data-uid='scene'
      style={{ position: 'absolute', left: 400, top: 0, width: 375, height: 812 }}
    >
      <RemixScene />
    </Scene>
  </Storyboard>
);
`

describe('Remix content', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)
  it('Renders the remix container with placeholder text', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')
    const remixScene = await renderResult.renderedDOM.findByTestId(REMIX_SCENE_TESTID)

    expect(remixScene.textContent).toEqual('Remix content is coming soon')
  })
})

setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', true)
describe('Remix content with feature switch off', () => {
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Remix support', false)
  it('Doesnt render the remix container with feature switch off', async () => {
    const project = createModifiedProject({
      [StoryboardFilePath]: storyboardFileContent,
    })
    const renderResult = await renderTestEditorWithModel(project, 'await-first-dom-report')
    await expect(async () =>
      renderResult.renderedDOM.findAllByTestId(REMIX_SCENE_TESTID),
    ).rejects.toThrow()
  })
})
