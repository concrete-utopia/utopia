/**
 * importing the shared entry point imports file, which should in theory mean that
 * the real running Editor and the test editors resolve dependencies in the same order,
 * which should in theory prevent diverging circular dependencies
 */
import './src/templates/editor-entry-point-imports'

// // The matchers API
import expect from 'expect'
import * as BrowserFS from 'browserfs'
import * as ReactTestingLibrary from '@testing-library/react'

// eslint react plugin uses this
BrowserFS.configure({ fs: 'InMemory', options: {} }, (e) => {
  if (e != null) {
    throw e
  }
})
window.BrowserFS = BrowserFS

ReactTestingLibrary.configure({
  getElementError: (message, _container) => {
    // Prevent the testing library from serialising the whole dom
    return new Error(message)
  },
})

window.expect = expect
window.jest = null
window.KarmaTestEnvironment = true

// This causes unhandled asynchronous exceptions (from promises) to fail tests.
window.addEventListener(
  'unhandledrejection',
  (error) => {
    throw new Error(`Unhandled asynchronous exception: ${error.reason}`)
  },
  { capture: true },
)
