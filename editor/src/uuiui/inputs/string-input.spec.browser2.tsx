/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "checkPlaceholder"] }] */
import type { RenderResult } from '@testing-library/react'
import { render } from '@testing-library/react'
import React from 'react'
import { StringInput } from './string-input'
import {
  TestInspectorContextProvider,
  getStoreHook,
} from '../../components/inspector/common/inspector.test-utils'

describe('StringInput', () => {
  function checkPlaceholder(renderResult: RenderResult, expectedPlaceholder: string | null): void {
    const inputElement = renderResult.queryByTestId('placeholdertest')
    if (inputElement == null) {
      throw new Error('Could not find input element.')
    } else {
      expect(inputElement.getAttribute('placeholder')).toEqual(expectedPlaceholder)
    }
  }
  it('ensures that no placeholder property shows in the input field by default', () => {
    const storeHookForTest = getStoreHook()
    const result = render(
      <TestInspectorContextProvider
        selectedViews={storeHookForTest.getState().editor.selectedViews}
        editorStoreData={storeHookForTest}
      >
        <StringInput testId='placeholdertest' controlStatus='simple' />
      </TestInspectorContextProvider>,
    )
    checkPlaceholder(result, null)
  })
  it('ensures that the placeholder property shows in the input field', () => {
    const storeHookForTest = getStoreHook()
    const result = render(
      <TestInspectorContextProvider
        selectedViews={storeHookForTest.getState().editor.selectedViews}
        editorStoreData={storeHookForTest}
      >
        <StringInput
          testId='placeholdertest'
          placeholder='this is a placeholder'
          controlStatus='simple'
        />
      </TestInspectorContextProvider>,
    )
    checkPlaceholder(result, 'this is a placeholder')
  })
  it('ensures that the unknown control styles property shows in the input field', () => {
    const storeHookForTest = getStoreHook()
    const result = render(
      <TestInspectorContextProvider
        selectedViews={storeHookForTest.getState().editor.selectedViews}
        editorStoreData={storeHookForTest}
      >
        <StringInput
          testId='placeholdertest'
          placeholder='this is a placeholder'
          controlStatus='multiselect-simple-unknown-css'
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
        <StringInput
          testId='placeholdertest'
          placeholder='this is a placeholder'
          controlStatus='multiselect-mixed-simple-or-unset'
        />
      </TestInspectorContextProvider>,
    )
    checkPlaceholder(result, 'Mixed')
  })
})
