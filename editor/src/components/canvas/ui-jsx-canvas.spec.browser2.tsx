import { BakedInStoryboardVariableName, BakedInStoryboardUID } from '../../core/model/scene-utils'
import {
  renderTestEditorWithCode,
  TestSceneUID,
  TestAppUID,
  getPrintedUiJsCodeWithoutUIDs,
} from './ui-jsx.test-utils'

describe.only('Canvas', () => {
  it('renders fine with a valid registerComponent call', async () => {
    const renderResult = await renderTestEditorWithCode(
      `
      import * as React from 'react'
      import { View, Storyboard, Scene, registerComponent } from 'utopia-api'
      
      export var App = (props) => {
        return (
          <View
            style={{ position: 'absolute', height: '99.9', width: '77.7' }}
            data-uid={'aaa'}
          >
            <View style={{position: 'absolute'}} data-uid={'bbb'}>hi</View>
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
              />
            </Scene>
          </Storyboard>
        )
      }
      registerComponent({
        name: 'App',
        moduleName: '/utopia/storyboard',
        controls: {
          test: {
            control: 'checkbox'
          }
        },
        insert: '<App />',
        requiredImports: 'import { App } from "/utopia/storyboard";',
      })
      `,
      'await-first-dom-report',
    )
    expect(renderResult.getEditorState().derived.navigatorTargets.length).toEqual(4)
  })
  it('throws an exception for an invalid registerComponent call', async () => {
    let possibleError: Error | null = null
    process.prependOnceListener('uncaughtException', (error) => {
      possibleError = error
    })
    const renderResult = renderTestEditorWithCode(
      `
      import * as React from 'react'
      import { View, Storyboard, Scene, registerComponent } from 'utopia-api'
      
      export var App = (props) => {
        return (
          <View
            style={{ position: 'absolute', height: '99.9', width: '77.7' }}
            data-uid={'aaa'}
          >
            <View style={{position: 'absolute'}} data-uid={'bbb'}>hi</View>
          </View>
        )
      }
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard data-uid={'${BakedInStoryboardUID}'}>
            <Scene
              style={{ position: 'absolute', height: 200, left: 59, width: 200, top: 79 }}
              data-uid={'${TestSceneUID}'}
            >
              <App
                data-uid='${TestAppUID}'
              />
            </Scene>
          </Storyboard>
        )
      }
      registerComponent({
        name: 'App',
        moduleName: '/utopia/storyboard',
        controls: {
          test: {
            control: 'checkboxa'
          }
        },
        insert: '<App />',
        requiredImports: 'import { App } from "/utopia/storyboard";',
      })
      `,
      'await-first-dom-report',
      false,
    )

    await renderResult
    expect(possibleError).not.toBeNull()
  })
})
