import { canFollowTarget, followTarget, multiplayerInitialsFromName } from './multiplayer'

describe('multiplayer', () => {
  describe('multiplayerInitialsFromName', () => {
    it('regular name', () => {
      expect(multiplayerInitialsFromName('Foo Bar')).toEqual('FB')
    })
    it('name with many words', () => {
      expect(multiplayerInitialsFromName('Foo Bar Baz')).toEqual('FB')
    })
    it('single word', () => {
      expect(multiplayerInitialsFromName('Bar')).toEqual('BA')
    })
    it('a very short name', () => {
      expect(multiplayerInitialsFromName('F')).toEqual('FX')
    })
    it('no name at all', () => {
      expect(multiplayerInitialsFromName('')).toEqual('XX')
    })
  })

  describe('canFollowTarget', () => {
    it('can follow a single player', () => {
      expect(
        canFollowTarget(followTarget('foo', 0), followTarget('bar', 0), [
          { id: 'bar', connectionId: 0, following: null },
        ]),
      ).toBe(true)
    })
    it('cannot follow self', () => {
      expect(
        canFollowTarget(followTarget('foo', 0), followTarget('foo', 0), [
          { id: 'foo', connectionId: 1, following: null },
        ]),
      ).toBe(false)
    })
    it('can follow a single player with the same ID but on another connection', () => {
      expect(
        canFollowTarget(followTarget('foo', 0), followTarget('foo', 1), [
          { id: 'foo', connectionId: 1, following: null },
        ]),
      ).toBe(true)
    })
    it('cannot follow a player that follows another player', () => {
      expect(
        canFollowTarget(followTarget('foo', 0), followTarget('bar', 0), [
          { id: 'bar', connectionId: 0, following: 'baz' },
          { id: 'baz', connectionId: 0, following: null },
        ]),
      ).toBe(false)
    })
    it('cannot follow a player that follows another player indirectly', () => {
      expect(
        canFollowTarget(followTarget('foo', 0), followTarget('bar', 0), [
          { id: 'bar', connectionId: 0, following: 'baz' },
          { id: 'baz', connectionId: 0, following: 'qux' },
          { id: 'qux', connectionId: 0, following: null },
        ]),
      ).toBe(false)
    })
    it('cannot follow a player back', () => {
      expect(
        canFollowTarget(followTarget('foo', 0), followTarget('bar', 0), [
          { id: 'bar', connectionId: 0, following: 'foo' },
        ]),
      ).toBe(false)
    })
    it('cannot follow a player that has an indirect loop', () => {
      expect(
        canFollowTarget(followTarget('foo', 0), followTarget('bar', 0), [
          { id: 'bar', connectionId: 0, following: 'baz' },
          { id: 'baz', connectionId: 0, following: 'qux' },
          { id: 'qux', connectionId: 0, following: 'foo' },
        ]),
      ).toBe(false)
    })
  })
})
