import { jsxAttributesFromMap, jsxElement } from '../../../core/shared/element-template'
import {
  TestAppUID,
  TestScenePath,
  TestSceneUID,
  formatTestProjectCode,
  renderTestEditorWithCode,
} from '../../../components/canvas/ui-jsx.test-utils'
import { conditionalClauseInsertionPath, replaceWithSingleElement } from '../store/insertion-path'
import { insertElementIntoJSXConditional } from './wrap-unwrap-helpers'
import { importAlias, importDetails } from '../../../core/shared/project-file-types'
import * as EP from '../../../core/shared/element-path'
import {
  BakedInStoryboardUID,
  BakedInStoryboardVariableName,
} from '../../../core/model/scene-utils'
import { printParsedCodeForFile } from '../../../components/custom-code/code-file.test-utils'

describe('insertElementIntoJSXConditional', () => {
  it('inserts an element into a conditional, and duplicate name', async () => {
    const project = formatTestProjectCode(`
    import * as React from 'react'
    import { Spring } from 'non-existant-dummy-library'
    import { Scene, Storyboard, View, Group } from 'utopia-api'
  
    export var App = (props) => {
        return <div
              style={{
                height: '100%',
                width: '100%',
                contain: 'layout',
              }}
              data-uid='root'
            >
              <div
                style={{
                  height: 150,
                  width: 150,
                  position: 'absolute',
                  left: 154,
                  top: 134,
                }}
                data-uid='container'
              >
                {
                  // @utopia/uid=conditional
                  true ? 
                    <div data-uid='sss'/>
                   : <Spring data-uid='pop'/>
                }
              </div>
            </div>
    }
  
    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard data-uid='${BakedInStoryboardUID}'>
          <Scene
            style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
          >
            <App
            data-uid='${TestAppUID}'
              style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
            />
          </Scene>
        </Storyboard>
      )
    }`)
    const renderResult = await renderTestEditorWithCode(project, 'await-first-dom-report')

    const newEditorState = insertElementIntoJSXConditional(
      renderResult.getEditorState().editor,
      conditionalClauseInsertionPath(
        EP.appendNewElementPath(TestScenePath, ['root', 'container', 'conditional']),
        'true-case',
        replaceWithSingleElement(),
      ),
      jsxElement('Spring', 'spring', jsxAttributesFromMap({}), []),
      {
        './test.js': importDetails(null, [importAlias('Spring')], null),
      },
    )
    const required = formatTestProjectCode(`
    import * as React from 'react'
    import { Spring } from 'non-existant-dummy-library'
    import { Scene, Storyboard, View, Group } from 'utopia-api'
    import { Spring as Spring_2 } from './test.js'
  
    export var App = (props) => {
        return <div
              style={{
                height: '100%',
                width: '100%',
                contain: 'layout',
              }}
              data-uid='root'
            >
              <div
                style={{
                  height: 150,
                  width: 150,
                  position: 'absolute',
                  left: 154,
                  top: 134,
                }}
                data-uid='container'
              >
                {
                  // @utopia/uid=conditional
                   true ? (
                     <Spring_2>
                       <div data-uid='sss' />
                     </Spring_2>
                   ) : (
                     <Spring data-uid='pop'/>
                   )
                }
              </div>
            </div>
    }
  
    export var ${BakedInStoryboardVariableName} = (props) => {
      return (
        <Storyboard data-uid='${BakedInStoryboardUID}'>
          <Scene
            style={{ left: 0, top: 0, width: 400, height: 400 }}
          data-uid='${TestSceneUID}'
          >
            <App
            data-uid='${TestAppUID}'
              style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }}
            />
          </Scene>
        </Storyboard>
      )
    }`)
    const requiredString = `${required}`
    expect(printParsedCodeForFile(newEditorState, '/utopia/storyboard.js', false)).toEqual(
      requiredString,
    )
  })
})
