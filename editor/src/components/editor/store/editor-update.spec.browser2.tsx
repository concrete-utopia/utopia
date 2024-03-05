import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ScenePathForTestUiJsFile } from '../../../core/model/test-ui-js-file.test-utils'
import * as EP from '../../../core/shared/element-path'
import { selectComponentsForTest } from '../../../utils/utils.test-utils'
import { renderTestEditorWithCode } from '../../canvas/ui-jsx.test-utils'
import { moveSelectedBackward } from '../actions/action-creators'

describe('actions', () => {
  it('MOVE_SELECTED_BACKWARD', async () => {
    const editor = await renderTestEditorWithCode(
      `import * as React from 'react'
        import {
          UtopiaUtils,
          Ellipse,
          Image,
          Rectangle,
          Storyboard,
          Text,
          View,
          Scene,
        } from 'utopia-api'
        
        var Test = (props) => {
          return (
            <View
              layout={{
                left: props.style.left,
                top: props.style.top,
                width: props.style.width,
                height: props.style.height,
              }}
              style={{
                position: 'absolute',
                backgroundColor: 'lightgrey',
              }}
              data-uid='view'
            >
              <React.Fragment>
                <Ellipse
                  layout={{
                    left: 150,
                    top: 25,
                    width: 100,
                    height: 100,
                  }}
                  style={{ backgroundColor: 'lightgreen' }}
                  data-uid='bbb'
                />
                <Rectangle
                  layout={{
                    left: 25,
                    top: 25,
                    width: 100,
                    height: 100,
                  }}
                  style={{ backgroundColor: 'orange' }}
                  data-uid='ccc'
                />
              </React.Fragment>
              <View
                layout={{
                  left: 150,
                  top: 150,
                  width: 100,
                  height: 100,
                  layoutSystem: 'group',
                }}
                style={{
                  position: 'absolute',
                  backgroundColor: 'red',
                  boxShadow: '10px 10px 8px #888888',
                }}
                data-uid='ddd'
              >
                <Rectangle
                  layout={{
                    left: 220,
                    top: 220,
                    width: 20,
                    height: 20,
                  }}
                  style={{ backgroundColor: 'orange' }}
                  data-uid='eee'
                />
                <Rectangle
                  layout={{
                    left: 90,
                    top: 90,
                    width: 100,
                    height: 100,
                  }}
                  style={{ backgroundColor: 'orange' }}
                  data-uid='fff'
                />
              </View>
              <View
                layout={{
                  left: 50,
                  top: 250,
                  width: 100,
                  height: 200,
                }}
                style={{
                  position: 'absolute',
                  backgroundColor: 'blue',
                }}
                data-uid='ggg'
              />
              <MyComponent data-uid='mycomponent' />
            </View>
          )
        }
        var MyComponent = (props) => {
          return (
            <View
              style={{ backgroundColor: 'green' }}
              data-uid='jjj'
            />
          )
        }
        var storyboard = (
          <Storyboard data-uid='utopia-storyboard-uid'>
            <Scene
              data-uid='scene-0'
              data-label='Test Scene'
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: 375,
                height: 812,
              }}
            >
              <Test
                data-uid='main-component-0'
                style={{
                  position: 'absolute',
                  left: 0,
                  top: 0,
                  width: 375,
                  height: 812,
                }}
              />
            </Scene>
            <Scene
              data-uid='scene-1'
              data-label='Test Scene 2'
              style={{
                position: 'absolute',
                left: 500,
                top: 0,
                width: 375,
                height: 812,
              }}
            />
          </Storyboard>
        )
        `,
      'await-first-dom-report',
    )

    const elementPathToMove = EP.appendNewElementPath(ScenePathForTestUiJsFile, ['view', 'ddd'])

    await selectComponentsForTest(editor, [elementPathToMove])

    const oldIndexInParent = MetadataUtils.getIndexInParent(
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.elementPathTree,
      elementPathToMove,
    )

    expect(oldIndexInParent).toBe(1)

    await editor.dispatch([moveSelectedBackward()], true)

    const newIndexInParent = MetadataUtils.getIndexInParent(
      editor.getEditorState().editor.jsxMetadata,
      editor.getEditorState().editor.elementPathTree,
      elementPathToMove,
    )

    expect(newIndexInParent).toBe(0)
  })
})
