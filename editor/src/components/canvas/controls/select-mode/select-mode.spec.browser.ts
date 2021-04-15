import { act, fireEvent } from '@testing-library/react'
import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import * as TP from '../../../../core/shared/template-path'
import { setElectronWindow } from '../../../../core/shared/test-setup.test-utils'
import { wait } from '../../../../utils/utils.test-utils'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestAppUID,
  TestScenePath,
  TestSceneUID,
} from '../../ui-jsx.test-utils'
import { CanvasControlsContainerID } from '../new-canvas-controls'

describe('Select Mode Selection', () => {
  beforeAll(setElectronWindow)

  it('keep double clicking on a children eventually selects it – even if it is out of bounds of the parents', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
          <div data-uid='a' style={{ ...props.style }}>
            <div
              data-uid='b'
              style={{
                backgroundColor: '#0091FFAA',
                position: 'absolute',
                overflow: 'visible',
                left: 50,
                top: 50,
                height: 120,
                width: 120,
              }}
            >
              <div
              data-uid='c'
                style={{
                  backgroundColor: '#0091FFAA',
                  position: 'absolute',
                  left: 30,
                  top: 30,
                  height: 120,
                  width: 120,
                }}
              >
                <div
                  data-uid='d'
                  style={{
                    backgroundColor: '#0091FFAA',
                    position: 'absolute',
                    height: 120,
                    width: 120,
                    left: 30,
                    top: 30,
                  }}
                >
                  <div
                    data-uid='e'
                    style={{
                      backgroundColor: '#0091FFAA',
                      position: 'absolute',
                      left: 30,
                      top: 30,
                      height: 120,
                      width: 120,
                    }}
                  >
                    <div
                      data-uid='targetdiv'
                      data-testid='targetdiv'
                      style={{
                        backgroundColor: '#0091FFAA',
                        position: 'absolute',
                        left: 30,
                        top: 30,
                        height: 120,
                        width: 120,
                      }}
                    />
                  </div>
                </div>
              </div>
            </div>
          </div>
      `),
    )

    const areaControl = renderResult.renderedDOM.getByTestId('targetdiv')
    const areaControlBounds = areaControl.getBoundingClientRect()

    const canvasControlsLayer = renderResult.renderedDOM.getByTestId(CanvasControlsContainerID)

    const doubleClick = async () => {
      await act(async () => {
        const domFinished = renderResult.getDomReportDispatched()
        const dispatchDone = renderResult.getDispatchFollowUpactionsFinished()
        fireEvent(
          canvasControlsLayer,
          new MouseEvent('mousedown', {
            detail: 1,
            bubbles: true,
            cancelable: true,
            metaKey: false,
            clientX: areaControlBounds.left + 20,
            clientY: areaControlBounds.top + 20,
            buttons: 1,
          }),
        )
        fireEvent(
          canvasControlsLayer,
          new MouseEvent('mousedown', {
            detail: 2,
            bubbles: true,
            cancelable: true,
            metaKey: false,
            clientX: areaControlBounds.left + 20,
            clientY: areaControlBounds.top + 20,
            buttons: 1,
          }),
        )
        await domFinished
        await dispatchDone
      })
      await waitForAnimationFrame()
    }

    await doubleClick()

    const selectedViews1 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews1).toEqual([
      TP.instancePath(TP.emptyScenePath, [BakedInStoryboardUID, TestSceneUID]),
    ])

    await doubleClick()

    const selectedViews2 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews2).toEqual([
      TP.instancePath(TP.emptyScenePath, [BakedInStoryboardUID, TestSceneUID, TestAppUID]),
    ])

    await doubleClick()

    const selectedViews3 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews3).toEqual([TP.instancePath(TestScenePath, ['a'])])

    await doubleClick()

    const selectedViews4 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews4).toEqual([TP.instancePath(TestScenePath, ['a', 'b'])])

    await doubleClick()

    const selectedViews5 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews5).toEqual([TP.instancePath(TestScenePath, ['a', 'b', 'c'])])

    await doubleClick()

    const selectedViews6 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews6).toEqual([TP.instancePath(TestScenePath, ['a', 'b', 'c', 'd'])])

    await doubleClick()

    const selectedViews7 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews7).toEqual([TP.instancePath(TestScenePath, ['a', 'b', 'c', 'd', 'e'])])

    await doubleClick()

    // after 8 "double clicks", the `targetdiv` div should be selected
    const selectedViews8 = renderResult.getEditorState().editor.selectedViews
    expect(selectedViews8).toEqual([
      TP.instancePath(TestScenePath, ['a', 'b', 'c', 'd', 'e', 'targetdiv']),
    ])
  })
})

function waitForAnimationFrame(): Promise<void> {
  return new Promise<void>((resolve, reject) => {
    requestAnimationFrame(() => {
      requestAnimationFrame(() => {
        requestAnimationFrame(() => {
          resolve()
        })
      })
    })
  })
}
