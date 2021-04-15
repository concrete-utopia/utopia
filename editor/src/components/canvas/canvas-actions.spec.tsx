import { selectComponents } from '../editor/actions/action-creators'
import CanvasActions from './canvas-actions'
import {
  getPrintedUiJsCodeWithoutUIDs,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from './ui-jsx.test-utils'
import * as TP from '../../core/shared/template-path'
import {
  createDuplicationNewUIDsFromEditorState,
  getOriginalCanvasFrames,
  getOriginalFrames,
} from './canvas-utils'
import { moveDragState } from './canvas-types'
import { canvasPoint, canvasRectangle } from '../../core/shared/math-utils'
import { BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { PrettierConfig } from '../../core/workers/parser-printer/prettier-utils'
import * as Prettier from 'prettier'

describe('createDragState', () => {
  it('MOVE_DRAG_STATE with duplicate', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ position: 'relative', width: 400, height: 400 }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#DDDDDD', width: 50, height: 50 }}
          data-uid='bbb'
        >
          <div
            style={{ backgroundColor: 'hotpink', width: 50, height: 50 }}
            data-uid='ccc'
          />
        </div>
      </div>
      `),
    )

    const selectedViews = [TP.instancePath(TestScenePath, ['aaa', 'bbb'])]
    await renderResult.dispatch([selectComponents(selectedViews, false)], true)

    const editorState = renderResult.getEditorState().editor

    const duplicateNewUIDs = createDuplicationNewUIDsFromEditorState(editorState)
    const dragStateAction = CanvasActions.createDragState(
      moveDragState(
        canvasPoint({ x: 0, y: 0 }),
        canvasPoint({ x: 15, y: 25 }),
        null,
        getOriginalCanvasFrames(selectedViews, editorState.jsxMetadata),
        canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        false,
        false,
        true, // duplicate
        false,
        duplicateNewUIDs,
        canvasPoint({ x: 15, y: 15 }),
        editorState.jsxMetadata,
        selectedViews,
      ),
    )

    await renderResult.dispatch([dragStateAction], true)
    await renderResult.dispatch([CanvasActions.clearDragState(true)], true)

    expect(getPrintedUiJsCodeWithoutUIDs(renderResult.getEditorState())).toEqual(
      Prettier.format(
        `/** @jsx jsx */
      import * as React from 'react'
      import { Scene, Storyboard, View, jsx } from 'utopia-api'
    
      export var App = (props) => {
        return (
          <div style={{ position: 'relative', width: 400, height: 400 }}>
            <div style={{ backgroundColor: '#DDDDDD', width: 50, height: 50 }} >
              <div style={{ backgroundColor: 'hotpink', width: 50, height: 50 }} />
            </div>
            <div style={{ backgroundColor: '#DDDDDD', width: 50, height: 50, left: 15, top: 25 }}>
              <div style={{ backgroundColor: 'hotpink', width: 50, height: 50 }} />
            </div>
          </div>
        )
      }
    
      export var ${BakedInStoryboardVariableName} = (props) => {
        return (
          <Storyboard>
            <Scene style={{ left: 0, top: 0, width: 400, height: 400 }}>
              <App style={{ position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 }} />
            </Scene>
          </Storyboard>
        )
      }`,
        PrettierConfig,
      ),
    )
  })
})
