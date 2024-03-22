import { githubRepositoryPrettyName } from './github'

describe('github', () => {
  describe('githubRepositoryStringOrNull', () => {
    it('returns empty string on null', async () => {
      expect(githubRepositoryPrettyName(null)).toBe('')
    })
    it('returns the string as-is when malformed', async () => {
      expect(githubRepositoryPrettyName('foo')).toBe('foo')
    })
    it('returns the owner+repo if branch is missing', async () => {
      expect(githubRepositoryPrettyName('foo/bar')).toBe('foo/bar')
    })
    it('returns the owner+repo and branch', async () => {
      expect(githubRepositoryPrettyName('foo/bar:baz')).toBe('foo/bar (baz)')
    })
  })
})
