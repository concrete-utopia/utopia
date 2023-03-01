import { act, render } from '@testing-library/react'
import React from 'react'
import { LayoutSystem } from 'utopia-api/core'
import {
  enableWhyDidYouRenderOnComponent,
  setupReactWhyDidYouRender,
} from '../../../../utils/react-memoize.test-utils'
import { CanvasRectangle, CanvasVector, LocalRectangle } from '../../../../core/shared/math-utils'
import {
  editPropOfSelectedView,
  getStoreHook,
  TestInspectorContextProvider,
} from '../../common/inspector.test-utils'
import { LayoutSection } from './layout-section'
import { emptySpecialSizeMeasurements } from '../../../../core/shared/element-template'
import { NO_OP } from '../../../../core/shared/utils'
import { stylePropPathMappingFn } from '../../common/property-path-hooks'
import { styleStringInArray } from '../../../../utils/common-constants'

describe('Layout Section', () => {
  enableWhyDidYouRenderOnComponent(LayoutSection)

  it('make sure whyDidYouRender is enabled', () => {
    expect((LayoutSection as any).whyDidYouRender).toBeTruthy()
  })
  it('doesnt rerender on irrelevant changes', () => {
    const storeHookForTest = getStoreHook()

    storeHookForTest.updateStore((store) => {
      return editPropOfSelectedView(store, stylePropPathMappingFn('width', styleStringInArray), 198)
    })

    const [getUpdateCount] = setupReactWhyDidYouRender(true)

    const { getByText } = render(
      <TestInspectorContextProvider
        selectedViews={storeHookForTest.getState().editor.selectedViews}
        editorStoreData={storeHookForTest}
      >
        <LayoutSection
          hasNonDefaultPositionAttributes={true}
          aspectRatioLocked={false}
          toggleAspectRatioLock={NO_OP}
        />
      </TestInspectorContextProvider>,
      { legacyRoot: true },
    )

    expect(getByText('flow')).toBeDefined()

    act(() => {
      storeHookForTest.updateStoreWithImmer((store) => {
        // irrelevant state change, we expect zero rerenders
        store.editor.canvas.roundedCanvasOffset = { x: 30, y: 50 } as CanvasVector
      })
    })

    expect(getUpdateCount()).toEqual(0)
  })
})
