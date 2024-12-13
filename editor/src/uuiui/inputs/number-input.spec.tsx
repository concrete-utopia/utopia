import React from 'react'
import type { CSSNumber, UnknownOrEmptyInput } from '../../components/inspector/common/css-utils'
import {
  cssNumber,
  isEmptyInputValue,
  isUnknownInputValue,
} from '../../components/inspector/common/css-utils'
import { act, fireEvent, render } from '@testing-library/react'
import {
  getStoreHook,
  TestInspectorContextProvider,
} from '../../components/inspector/common/inspector.test-utils'
import { calculateDragDirectionDelta, NumberInput } from './number-input'
import keycode from 'keycode'

jest.useFakeTimers()

describe('NumberInput', () => {
  it('calculateDragDirectionDelta should be able to invert its results', () => {
    const testCases = [
      [5, 2],
      [2, 5],
      [-5.5, 2],
      [-2, 5],
      [0, 4],
      [3.2, 1],
      [-6, 3],
      [-3, 6],
    ]

    testCases.forEach(([delta, scaling]) => {
      const { result, inverse } = calculateDragDirectionDelta(delta, scaling)
      expect(inverse(result)).toEqual(delta)
    })
  })
  it('triggers multiple transient updates and a single non-transient one when holding a key down and eventually releasing it', async () => {
    const storeHookForTest = getStoreHook()
    let value = cssNumber(100)
    let transientUpdates: Array<UnknownOrEmptyInput<CSSNumber>> = []

    function updateValue(number: UnknownOrEmptyInput<CSSNumber>): void {
      if (isUnknownInputValue(number) || isEmptyInputValue(number)) {
        throw new Error(`Unexpected value: ${number}`)
      } else {
        value = number
      }
      reRender()
    }

    function onTransientSubmitValue(number: UnknownOrEmptyInput<CSSNumber>): void {
      transientUpdates.push(number)
      updateValue(number)
    }
    let nonTransientUpdates: Array<UnknownOrEmptyInput<CSSNumber>> = []
    function onSubmitValue(number: UnknownOrEmptyInput<CSSNumber>): void {
      nonTransientUpdates.push(number)
      updateValue(number)
    }

    function reRender() {
      return render(
        <TestInspectorContextProvider
          selectedViews={storeHookForTest.getState().editor.selectedViews}
          editorStoreData={storeHookForTest}
        >
          <NumberInput
            testId='keydowntest'
            controlStatus='simple'
            value={value}
            numberType={'AnyValid'}
            defaultUnitToHide={null}
            // eslint-disable-next-line react/jsx-no-bind
            onTransientSubmitValue={onTransientSubmitValue}
            // eslint-disable-next-line react/jsx-no-bind
            onSubmitValue={onSubmitValue}
          />
        </TestInspectorContextProvider>,
      )
    }

    const result = reRender()
    const inputElement = result.getByTestId('keydowntest')
    for (let count = 0; count < 10; count++) {
      act(() => {
        fireEvent(
          inputElement,
          new KeyboardEvent('keydown', {
            bubbles: true,
            cancelable: true,
            key: 'ArrowUp',
            keyCode: keycode('ArrowUp'),
          }),
        )
        fireEvent(
          inputElement,
          new KeyboardEvent('keyup', {
            bubbles: true,
            cancelable: true,
            key: 'ArrowUp',
            keyCode: keycode('ArrowUp'),
          }),
        )
      })
    }
    // Need this for the delay that is in the `NumberInput`.
    jest.runAllTimers()
    expect(transientUpdates).toMatchInlineSnapshot(`
      Array [
        Object {
          "unit": null,
          "value": 101,
        },
        Object {
          "unit": null,
          "value": 102,
        },
        Object {
          "unit": null,
          "value": 103,
        },
        Object {
          "unit": null,
          "value": 104,
        },
        Object {
          "unit": null,
          "value": 105,
        },
        Object {
          "unit": null,
          "value": 106,
        },
        Object {
          "unit": null,
          "value": 107,
        },
        Object {
          "unit": null,
          "value": 108,
        },
        Object {
          "unit": null,
          "value": 109,
        },
        Object {
          "unit": null,
          "value": 110,
        },
      ]
    `)
    expect(nonTransientUpdates).toMatchInlineSnapshot(`
      Array [
        Object {
          "unit": null,
          "value": 110,
        },
      ]
    `)
  })
})
