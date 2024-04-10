/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "checkPlaceholder"] }] */
import type { RenderResult } from '@testing-library/react'
import { render } from '@testing-library/react'
import React from 'react'
import { StringInput } from './string-input'
import {
  TestInspectorContextProvider,
  getStoreHook,
} from '../../components/inspector/common/inspector.test-utils'
import {
  firePasteImageEvent,
  firePasteTextEvent,
} from '../../components/canvas/event-helpers.test-utils'
import { wait } from '../../core/model/performance-scripts'
import { imgBase641x1, makeImageFile } from '../../components/canvas/image-insert.test-utils'

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

  describe('paste events', () => {
    it('does nothing if there is no change handler', async () => {
      const storeHookForTest = getStoreHook()
      const result = render(
        <TestInspectorContextProvider
          selectedViews={storeHookForTest.getState().editor.selectedViews}
          editorStoreData={storeHookForTest}
        >
          <StringInput testId='the-input' value='hello' />
        </TestInspectorContextProvider>,
      )

      const inputElement = await result.findByTestId('the-input')
      if (inputElement == null || !(inputElement instanceof HTMLInputElement)) {
        throw new Error('Could not find input element.')
      }

      firePasteTextEvent(inputElement, 'well hello there!')
      await wait(0)

      expect(inputElement.value).toBe('hello')
    })

    it('does nothing if the pasted text is utopia data', async () => {
      const storeHookForTest = getStoreHook()
      const result = render(
        <TestInspectorContextProvider
          selectedViews={storeHookForTest.getState().editor.selectedViews}
          editorStoreData={storeHookForTest}
        >
          <StringInputWithChangeHandler testId='the-input' value='hello' />
        </TestInspectorContextProvider>,
      )

      const inputElement = await result.findByTestId('the-input')
      if (inputElement == null || !(inputElement instanceof HTMLInputElement)) {
        throw new Error('Could not find input element.')
      }

      const imagesToPaste = [await makeImageFile(imgBase641x1, 'chucknorris.png')]
      firePasteImageEvent(inputElement, imagesToPaste)
      await wait(0)

      expect(inputElement.value).toBe('hello')
    })

    it('does nothing if the pasted text is empty', async () => {
      const storeHookForTest = getStoreHook()
      const result = render(
        <TestInspectorContextProvider
          selectedViews={storeHookForTest.getState().editor.selectedViews}
          editorStoreData={storeHookForTest}
        >
          <StringInputWithChangeHandler testId='the-input' value='hello' />
        </TestInspectorContextProvider>,
      )

      const inputElement = await result.findByTestId('the-input')
      if (inputElement == null || !(inputElement instanceof HTMLInputElement)) {
        throw new Error('Could not find input element.')
      }

      firePasteTextEvent(inputElement, '')
      await wait(0)

      expect(inputElement.value).toBe('hello')
    })

    it('accepts pasted text', async () => {
      const storeHookForTest = getStoreHook()
      const result = render(
        <TestInspectorContextProvider
          selectedViews={storeHookForTest.getState().editor.selectedViews}
          editorStoreData={storeHookForTest}
        >
          <StringInputWithChangeHandler testId='the-input' value='hello' />
        </TestInspectorContextProvider>,
      )

      const inputElement = await result.findByTestId('the-input')
      if (inputElement == null || !(inputElement instanceof HTMLInputElement)) {
        throw new Error('Could not find input element.')
      }

      firePasteTextEvent(inputElement, 'well hello there!')
      await wait(0)

      expect(inputElement.value).toBe('well hello there!')
    })
  })
})

function StringInputWithChangeHandler(props: { testId: string; value: string }) {
  const [value, setValue] = React.useState(props.value)
  const onChange = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => setValue(e.currentTarget.value),
    [],
  )
  return (
    <StringInput onChange={onChange} testId={props.testId} controlStatus='simple' value={value} />
  )
}
