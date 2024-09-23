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
})
