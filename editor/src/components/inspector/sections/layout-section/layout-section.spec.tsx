import { act, render } from '@testing-library/react'
import * as React from 'react'
import { PropertyPath } from 'src/core/shared/project-file-types'
import { LayoutSystem } from 'utopia-api'
import { createLayoutPropertyPath } from '../../../../core/layout/layout-helpers-new'
import {
  enableWhyDidYouRenderOnComponent,
  setupReactWhyDidYouRender,
} from '../../../../utils/react-memoize-test-utils'
import utils from '../../../../utils/utils'
import { CanvasRectangle, CanvasVector, LocalRectangle } from '../../../../core/shared/math-utils'
import {
  editPropOfSelectedView,
  getStoreHook,
  TestInspectorContextProvider,
} from '../../common/inspector-test-utils'
import { LayoutSection } from './layout-section'
import { emptySpecialSizeMeasurements } from '../../../../core/shared/element-template'

describe('Layout Section', () => {
  enableWhyDidYouRenderOnComponent(LayoutSection)

  it('make sure whyDidYouRender is enabled', () => {
    expect((LayoutSection as any).whyDidYouRender).toBeTruthy()
  })
  it('doesnt rerender on irrelevant changes', () => {
    const storeHookForTest = getStoreHook(utils.NO_OP)

    storeHookForTest.updateStore((store) => {
      return editPropOfSelectedView(store, createLayoutPropertyPath('Width'), 198)
    })

    const [getUpdateCount] = setupReactWhyDidYouRender(true)

    const toggleAspectRatioLock = () => {}

    const { getByText } = render(
      <TestInspectorContextProvider editorStoreData={storeHookForTest}>
        <LayoutSection
          isChildOfFlexComponent={false}
          hasNonDefaultPositionAttributes={true}
          aspectRatioLocked={false}
          parentFlexAxis={null}
          toggleAspectRatioLock={toggleAspectRatioLock}
          layoutSystem={null}
          specialSizeMeasurements={emptySpecialSizeMeasurements}
          position='static'
          input={{
            frame: {
              x: 10,
              y: 10,
              width: 200,
              height: 100,
            } as LocalRectangle,
            parentFrame: {
              x: 0,
              y: 0,
              width: 350,
              height: 850,
            } as CanvasRectangle,
          }}
        />
      </TestInspectorContextProvider>,
    )

    // Component 'W' is picked by the selector
    expect(getByText('W')).toBeDefined()

    act(() => {
      storeHookForTest.updateStoreWithImmer((store) => {
        // irrelevant state change, we expect zero rerenders
        store.editor.canvas.roundedCanvasOffset = { x: 30, y: 50 } as CanvasVector
      })
    })

    expect(getUpdateCount()).toEqual(0)
  })
})
