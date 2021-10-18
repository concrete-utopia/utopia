import React from 'react'
import { render, fireEvent, act } from '@testing-library/react'

import * as EP from '../../../../core/shared/element-path'

import { getStoreHook, TestInspectorContextProvider } from '../../common/inspector.test-utils'
import utils from '../../../../utils/utils'
import { CanvasVector } from '../../../../core/shared/math-utils'
import {
  setupReactWhyDidYouRender,
  enableWhyDidYouRenderOnComponent,
} from '../../../../utils/react-memoize.test-utils'
import { ComponentSection, inferControlTypeBasedOnValue } from './component-section'
import { ScenePathForTestUiJsFile } from '../../../../core/model/test-ui-js-file.test-utils'
import { ArrayControlDescription, ObjectControlDescription } from 'utopia-api'

describe('Component Section', () => {
  enableWhyDidYouRenderOnComponent(ComponentSection)

  it('make sure whyDidYouRender is enabled', () => {
    expect((ComponentSection as any).whyDidYouRender).toBeTruthy()
  })
  xit('doesnt rerender on irrelevant changes', () => {
    const storeHookForTest = getStoreHook(utils.NO_OP)
    storeHookForTest.updateStoreWithImmer((store) => {
      store.editor.selectedViews = [
        EP.appendNewElementPath(ScenePathForTestUiJsFile, ['aaa', 'mycomponent']),
      ] // TODO add a Component instance to the test file and select that!
      store.editor.codeResultCache = {
        propertyControlsInfo: {
          '/src/app': {
            MyComponent: {
              text: {
                type: 'string',
                title: 'Title',
                defaultValue: '',
              },
            },
          },
        },
      } as any
    })

    const [getUpdateCount] = setupReactWhyDidYouRender(true)

    const { getByText } = render(
      <TestInspectorContextProvider
        selectedViews={storeHookForTest.api.getState().editor.selectedViews}
        editorStoreData={storeHookForTest}
      >
        <ComponentSection isScene={false} />
      </TestInspectorContextProvider>,
    )

    // Component 'Test' is picked by the scene selector
    expect(getByText('Component props')).toBeDefined()

    act(() => {
      storeHookForTest.updateStoreWithImmer((store) => {
        // irrelevant state change, we expect zero rerenders
        store.editor.canvas.roundedCanvasOffset = { x: 30, y: 50 } as CanvasVector
      })
    })

    expect(getUpdateCount()).toEqual(0)
  })
})

describe('inferControlTypeBasedOnValue', () => {
  it('Correctly infers simple number controls', () => {
    const result = inferControlTypeBasedOnValue(1)
    expect(result.type).toEqual('number')
  })

  it('Correctly infers simple string controls', () => {
    const result = inferControlTypeBasedOnValue('one')
    expect(result.type).toEqual('string')
  })

  it('Correctly infers color string controls', () => {
    // We could test more colors here, but those tests already exist in the actual function for parsing css colors
    const hexResult = inferControlTypeBasedOnValue('#00112233')
    expect(hexResult.type).toEqual('color')

    const rgbResult = inferControlTypeBasedOnValue('rgb(10,20,30)')
    expect(rgbResult.type).toEqual('color')

    const hslResult = inferControlTypeBasedOnValue('hsl(10,20%,30%)')
    expect(hslResult.type).toEqual('color')

    const keywordResult = inferControlTypeBasedOnValue('blue')
    expect(keywordResult.type).toEqual('color')
  })

  it('Correctly infers simple boolean controls', () => {
    const result = inferControlTypeBasedOnValue(true)
    expect(result.type).toEqual('boolean')
  })

  it('Correctly infers vector2 controls', () => {
    const result = inferControlTypeBasedOnValue([1, 2])
    expect(result.type).toEqual('vector2')
  })

  it('Correctly infers vector3 controls', () => {
    const result = inferControlTypeBasedOnValue([1, 2, 3])
    expect(result.type).toEqual('vector3')
  })

  it('Correctly infers simple array controls', () => {
    const numericArrayLength1Result = inferControlTypeBasedOnValue([1])
    expect(numericArrayLength1Result.type).toEqual('array')
    expect((numericArrayLength1Result as ArrayControlDescription).propertyControl.type).toEqual(
      'number',
    )

    const numericArrayLength4Result = inferControlTypeBasedOnValue([1, 2, 3, 4])
    expect(numericArrayLength4Result.type).toEqual('array')

    // Ensure non-numeric arrays aren't parsed as vectors
    const nonNumericArrayLength2Result = inferControlTypeBasedOnValue(['one', 'two'])
    expect(nonNumericArrayLength2Result.type).toEqual('array')
    const nonNumericArrayLength3Result = inferControlTypeBasedOnValue(['one', 'two', 'three'])
    expect(nonNumericArrayLength3Result.type).toEqual('array')
  })

  it('Correctly infers simple object controls', () => {
    const emptyObjectResult = inferControlTypeBasedOnValue({})
    expect(emptyObjectResult.type).toEqual('object')

    const result = inferControlTypeBasedOnValue({ k: 'value' })
    expect(result.type).toEqual('object')
    expect((result as ObjectControlDescription).object.k.type).toEqual('string')
  })

  it('Ignores the style prop', () => {
    const styleObjectResult = inferControlTypeBasedOnValue({}, 'style')
    expect(styleObjectResult.type).toEqual('ignore')
  })

  it('ignores a React element', () => {
    const reactComponentResult = inferControlTypeBasedOnValue(<div />)
    expect(reactComponentResult.type).toEqual('ignore')
  })

  it('Correctly infers nested array / object horror show controls', () => {
    const result = inferControlTypeBasedOnValue({
      arr: [
        {
          k: 'value',
        },
      ],
    })

    expect(result.type).toEqual('object')
    const outerObject = (result as ObjectControlDescription).object
    expect(outerObject.arr.type).toEqual('array')
    const arr = outerObject.arr as ArrayControlDescription
    expect(arr.propertyControl.type).toEqual('object')
    const innerObject = (arr.propertyControl as ObjectControlDescription).object
    expect(innerObject.k.type).toEqual('string')
  })
})
