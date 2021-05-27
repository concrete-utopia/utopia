// // The matchers API
import expect from 'expect'
import * as BrowserFS from 'browserfs'

// eslint react plugin uses this
BrowserFS.configure({ fs: 'InMemory', options: {} }, (e) => {
  if (e) {
    throw e
  }
})
window.BrowserFS = BrowserFS

window.expect = expect
