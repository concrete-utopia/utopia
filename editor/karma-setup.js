// // The matchers API
import expect from 'expect'
import * as BrowserFS from 'browserfs'

import { setTestEnvironment } from './src/common/env-vars'

// eslint react plugin uses this
BrowserFS.configure({ fs: 'InMemory', options: {} }, (e) => {
  if (e != null) {
    throw e
  }
})
window.BrowserFS = BrowserFS

window.expect = expect
window.jest = null
window.KarmaTestEnvironment = true

// tell Utopia it is in a test environment
setTestEnvironment()
