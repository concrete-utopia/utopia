import { canFollowTarget, multiplayerInitialsFromName } from './multiplayer'

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
      expect(canFollowTarget('foo', 'bar', [{ id: 'bar', following: null }])).toBe(true)
    })
    it('can follow a player that follows another player', () => {
      expect(
        canFollowTarget('foo', 'bar', [
          { id: 'bar', following: 'baz' },
          { id: 'baz', following: null },
        ]),
      ).toBe(true)
    })
    it('can follow a player that follows another player indirectly', () => {
      expect(
        canFollowTarget('foo', 'bar', [
          { id: 'bar', following: 'baz' },
          { id: 'baz', following: 'qux' },
          { id: 'qux', following: null },
        ]),
      ).toBe(true)
    })
    it('cannot follow a player back', () => {
      expect(canFollowTarget('foo', 'bar', [{ id: 'bar', following: 'foo' }])).toBe(false)
    })
    it('cannot follow a player that has an indirect loop', () => {
      expect(
        canFollowTarget('foo', 'bar', [
          { id: 'bar', following: 'baz' },
          { id: 'baz', following: 'qux' },
          { id: 'qux', following: 'foo' },
        ]),
      ).toBe(false)
    })
  })
})
