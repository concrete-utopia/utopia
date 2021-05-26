// // the jest.fn() API
import jest from 'jest-mock'
// // The matchers API
// import expect from 'expect'
import * as chai from 'chai'

// // Add missing Jest functions
window.test = window.it
window.test.each = (inputs) => (testName, test) =>
  inputs.forEach((args) => window.it(testName, () => test(...args)))
window.test.todo = function () {
  return undefined
}
window.jest = jest
window.expect = chai.expect
