import * as React from 'react'
import { render, fireEvent, act } from '@testing-library/react'

import { getStoreHook, TestInspectorContextProvider } from '../../common/inspector-test-utils'
import utils from '../../../../utils/utils'
import { CanvasVector } from '../../../../core/shared/math-utils'
import { SceneSection } from './scene-section'
import {
  setupReactWhyDidYouRender,
  enableWhyDidYouRenderOnComponent,
} from '../../../../utils/react-memoize-test-utils'
import { ScenePathForTestUiJsFile } from '../../../../core/model/test-ui-js-file'

describe('Scene Section', () => {
  enableWhyDidYouRenderOnComponent(SceneSection)

  it('make sure whyDidYouRender is enabled', () => {
    expect((SceneSection as any).whyDidYouRender).toBeTruthy()
  })
  /** This test has been x-ed because it:
   *  1. was a false negative, it didn't catch the scene section rendering on
   *     every scroll in production.
   *  2. failed irregularly: on the same branch it would fail if run with
   *     all tests, but pass if run by itself.
   */
  xit('doesnt rerender on irrelevant changes', () => {
    const storeHookForTest = getStoreHook(utils.NO_OP)
    storeHookForTest.updateStoreWithImmer((store) => {
      store.editor.selectedViews = [ScenePathForTestUiJsFile] // setting the first Scene as selected
    })

    const [getUpdateCount] = setupReactWhyDidYouRender(true)

    const { getByText } = render(
      <TestInspectorContextProvider editorStoreData={storeHookForTest}>
        <SceneSection />
      </TestInspectorContextProvider>,
    )

    // Component 'Test' is picked by the scene selector
    expect(getByText('Test')).toBeDefined()

    act(() => {
      storeHookForTest.updateStoreWithImmer((store) => {
        // irrelevant state change, we expect zero rerenders
        store.editor.canvas.roundedCanvasOffset = { x: 30, y: 50 } as CanvasVector
      })
    })

    expect(getUpdateCount()).toEqual(0)
  })
})
