import { Keyboard, Key } from './keyboard'

describe('Keyboard.key', () => {
  it('gets the same instance for invocations of the same parameters', () => {
    const first = Keyboard.key('b', ['alt', 'shift'])
    const second = Keyboard.key('b', ['shift', 'alt'])
    expect(first).toBe(second)
  })
})

describe('Keyboard.keysEquals', () => {
  it('returns true for two values created via the key function', () => {
    const first = Keyboard.key('b', ['alt', 'shift'])
    const second = Keyboard.key('b', ['shift', 'alt'])
    expect(Keyboard.areSameKey(first, second)).toEqual(true)
  })
  it('returns true for two values independently created', () => {
    const first: Key = {
      character: 'b',
      modifiers: ['alt', 'shift'],
    }
    const second: Key = {
      character: 'b',
      modifiers: ['shift', 'alt'],
    }
    expect(Keyboard.areSameKey(first, second)).toEqual(true)
  })
})
