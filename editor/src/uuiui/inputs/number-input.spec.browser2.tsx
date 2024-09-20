/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "checkPlaceholder"] }] */
import type { RenderResult } from '@testing-library/react'
import { act, fireEvent, render } from '@testing-library/react'
import React from 'react'
import {
  getStoreHook,
  TestInspectorContextProvider,
} from '../../components/inspector/common/inspector.test-utils'
import { NumberInput } from './number-input'
import type { CSSNumber, UnknownOrEmptyInput } from '../../components/inspector/common/css-utils'
import {
  cssNumber,
  isEmptyInputValue,
  isUnknownInputValue,
} from '../../components/inspector/common/css-utils'
import keycode from 'keycode'
import { wait } from '../../utils/utils.test-utils'

describe('NumberInput', () => {
  function checkPlaceholder(renderResult: RenderResult, expectedPlaceholder: string | null): void {
    const inputElement = renderResult.queryByTestId('placeholdertest')
    if (inputElement == null) {
      throw new Error('Could not find input element.')
    } else {
      expect(inputElement.getAttribute('placeholder')).toEqual(expectedPlaceholder)
    }
  }
  it('ensures that no placeholder property is in the input field by default', () => {
    const storeHookForTest = getStoreHook()
    const result = render(
      <TestInspectorContextProvider
        selectedViews={storeHookForTest.getState().editor.selectedViews}
        editorStoreData={storeHookForTest}
      >
        <NumberInput
          testId='placeholdertest'
          controlStatus='simple'
          value={null}
          numberType={'AnyValid'}
          defaultUnitToHide={null}
        />
      </TestInspectorContextProvider>,
    )
    checkPlaceholder(result, null)
  })
  it('ensures that the unknown control styles property shows in the input field', () => {
    const storeHookForTest = getStoreHook()
    const result = render(
      <TestInspectorContextProvider
        selectedViews={storeHookForTest.getState().editor.selectedViews}
        editorStoreData={storeHookForTest}
      >
        <NumberInput
          testId='placeholdertest'
          controlStatus='multiselect-simple-unknown-css'
          value={null}
          numberType={'AnyValid'}
          defaultUnitToHide={null}
        />
      </TestInspectorContextProvider>,
    )
    checkPlaceholder(result, 'Unknown')
  })
  it('ensures that the mixed control styles property shows in the input field', () => {
    const storeHookForTest = getStoreHook()
    const result = render(
      <TestInspectorContextProvider
        selectedViews={storeHookForTest.getState().editor.selectedViews}
        editorStoreData={storeHookForTest}
      >
        <NumberInput
          testId='placeholdertest'
          controlStatus='multiselect-mixed-simple-or-unset'
          value={null}
          numberType={'AnyValid'}
          defaultUnitToHide={null}
        />
      </TestInspectorContextProvider>,
    )
    checkPlaceholder(result, 'Mixed')
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
    await wait(700)
    expect(transientUpdates).toEqual([
      cssNumber(101),
      cssNumber(102),
      cssNumber(103),
      cssNumber(104),
      cssNumber(105),
      cssNumber(106),
      cssNumber(107),
      cssNumber(108),
      cssNumber(109),
      cssNumber(110),
    ])
    expect(nonTransientUpdates).toEqual([cssNumber(110)])
  })
})
