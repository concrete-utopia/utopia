import { multiplayerInitialsFromName } from './multiplayer'

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
})
