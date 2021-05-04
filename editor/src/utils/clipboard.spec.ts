import { contentsToTree } from '../components/assets'
import {
  renderTestEditorWithProjectContent,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../components/canvas/ui-jsx.test-utils'
import { createCodeFile } from '../components/custom-code/code-file.test-utils'
import { selectComponents } from '../components/editor/actions/action-creators'
import { DefaultPackageJson, StoryboardFilePath } from '../components/editor/store/editor-state'
import { directory } from '../core/model/project-file-utils'
import { BakedInStoryboardUID } from '../core/model/scene-utils'
import {
  ProjectContents,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../core/shared/project-file-types'
import * as EP from '../core/shared/element-path'
import { createClipboardDataFromSelectionNewWorld } from './clipboard'

describe('copy to clipboard', () => {
  it('creates copy data multifile elements', async () => {
    const appFilePath = '/src/app.js'
    let projectContents: ProjectContents = {
      '/package.json': textFile(
        textFileContents(
          JSON.stringify(DefaultPackageJson, null, 2),
          unparsed,
          RevisionsState.BothMatch,
        ),
        null,
        0,
      ),
      '/src': directory(),
      '/utopia': directory(),
      [StoryboardFilePath]: createCodeFile(
        StoryboardFilePath,
        `
import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
export var storyboard = (
  <Storyboard data-uid='${BakedInStoryboardUID}'>
    <Scene
      data-uid='${TestSceneUID}'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='${TestAppUID}' />
    </Scene>
  </Storyboard>
)`,
      ),
      [appFilePath]: createCodeFile(
        appFilePath,
        `
import * as React from 'react'
export var App = (props) => {
  return <div data-uid='app-outer-div' style={{position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF'}}>
    <div data-uid='app-inner-div-to-copy'/>
  </div>
}`,
      ),
    }
    const renderResult = await renderTestEditorWithProjectContent(contentsToTree(projectContents))
    const targetPath1 = EP.appendNewElementPath(TestScenePath, [
      'app-outer-div',
      'app-inner-div-to-copy',
    ])

    await renderResult.dispatch([selectComponents([targetPath1], false)], false)
    const clipboardData = createClipboardDataFromSelectionNewWorld(
      renderResult.getEditorState().editor,
    )

    expect(clipboardData?.data.length).toEqual(1)
    expect(clipboardData?.data[0].type).toEqual('ELEMENT_COPY')
    expect(clipboardData?.data[0].elements).toMatchInlineSnapshot(
      `"[{type:\\"JSX_ELEMENT\\",name:{baseVariable:\\"div\\",propertyPath:{propertyElements:[]}},props:[{key:\\"data-uid\\",value:{type:\\"ATTRIBUTE_VALUE\\",value:\\"app-inner-div-to-copy\\",comments:{leadingComments:[],trailingComments:[]}},comments:{leadingComments:[],trailingComments:[]}}],children:[]}]"`,
    )
  })
})
