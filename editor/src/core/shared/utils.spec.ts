import { urlSafeText } from './utils'

describe('urlSafeText', () => {
  it('for a simple string returns the same thing', () => {
    expect(urlSafeText('hat')).toEqual('hat')
  })
  it('for a string with a space in it replaces the space', () => {
    expect(urlSafeText('cat hat mat')).toEqual('cat-hat-mat')
  })
  it('for a string with all sorts in it, makes the url safe', () => {
    expect(urlSafeText('Awesome Pile Of Hats 😊?')).toEqual('awesome-pile-of-hats-%F0%9F%98%8A%3F')
  })
})
