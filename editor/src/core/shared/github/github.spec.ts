import type { ProjectContentTreeRoot } from '../../../components/assets'
import type { EditorDispatch } from '../../../components/editor/action-types'
import { emptyGithubData } from '../../../components/editor/store/editor-state'
import {
  getRefreshGithubActions,
  GithubHelpers,
  updateUserDetailsWhenAuthenticated,
} from './helpers'
import type { GithubOperationContext } from './operations/github-operation-context'

describe('github helpers', () => {
  let mockFetch = jest.spyOn(global, 'fetch')

  beforeEach(() => {
    mockFetch = jest.spyOn(global, 'fetch')
  })
  afterEach(() => {
    mockFetch.mockReset()
  })

  let mockDispatch: EditorDispatch = () => {}

  describe('getUserDetailsFromServer', () => {
    it('returns null if the response is not ok', async () => {
      mockFetch.mockResolvedValue(new Response(null, { status: 418 }))
      const got = await GithubHelpers.getUserDetailsFromServer()
      expect(got).toBe(null)
    })

    it('returns null if the request returns a failure', async () => {
      mockFetch.mockResolvedValue(new Response(JSON.stringify({ type: 'FAILURE' })))
      const got = await GithubHelpers.getUserDetailsFromServer()
      expect(got).toBe(null)
    })

    it('returns the user details if the request succeeds', async () => {
      mockFetch.mockResolvedValue(
        new Response(JSON.stringify({ type: 'SUCCESS', user: { id: 'that-guy' } })),
      )
      const got = await GithubHelpers.getUserDetailsFromServer()
      expect(got).toEqual({ id: 'that-guy' })
    })
  })

  describe('updateUserDetailsWhenAuthenticated', () => {
    it('returns false if the auth check fails', async () => {
      const got = await updateUserDetailsWhenAuthenticated(mockDispatch, Promise.resolve(false))
      expect(got).toBe(false)
    })

    it('returns false if there are no user details', async () => {
      mockFetch.mockResolvedValue(new Response(null, { status: 418 }))
      const got = await updateUserDetailsWhenAuthenticated(mockDispatch, Promise.resolve(true))
      expect(got).toBe(false)
    })

    it('returns true if the user details are there', async () => {
      mockFetch.mockResolvedValue(
        new Response(JSON.stringify({ type: 'SUCCESS', user: { id: 'that-guy' } })),
      )
      const got = await updateUserDetailsWhenAuthenticated(mockDispatch, Promise.resolve(true))
      expect(got).toBe(true)
    })
  })

  describe('getRefreshGithubActions', () => {
    function makeMockContext(
      fetch?: (url: string, options?: RequestInit) => Promise<Response>,
    ): GithubOperationContext {
      return {
        fetch: (url, options) => (fetch ?? global.fetch)?.(url, options),
        updateProjectContentsWithParseResults: async (): Promise<ProjectContentTreeRoot> => {
          return {}
        },
      }
    }

    it('returns actions to reset the GH data if GH is not authenticated', async () => {
      const got = await getRefreshGithubActions(
        mockDispatch,
        false,
        null,
        null,
        null,
        null,
        null,
        null,
        makeMockContext(),
        'user-initiated',
      )
      expect(got.length).toBe(1)
      const promises = await got[0]
      expect(promises.length).toBe(1)
      expect(promises[0].action).toBe('UPDATE_GITHUB_DATA')
      if (promises[0].action === 'UPDATE_GITHUB_DATA') {
        expect(promises[0].data).toEqual(emptyGithubData())
      }
    })

    it("returns actions to reset the GH data if GH is authenticated but there's no user on the server", async () => {
      mockFetch.mockResolvedValue(new Response(null, { status: 418 }))
      const got = await getRefreshGithubActions(
        mockDispatch,
        true,
        null,
        null,
        null,
        null, // <- user details
        null,
        null,
        makeMockContext(),
        'user-initiated',
      )
      expect(got.length).toBe(1)
      const promises = await got[0]
      expect(promises.length).toBe(2)
      expect(promises[0].action).toBe('SET_GITHUB_STATE')
      if (promises[0].action === 'SET_GITHUB_STATE') {
        expect(promises[0].githubState.authenticated).toBe(false)
      }
      expect(promises[1].action).toBe('UPDATE_GITHUB_DATA')
      if (promises[1].action === 'UPDATE_GITHUB_DATA') {
        expect(promises[1].data).toEqual(emptyGithubData())
      }
    })

    it("returns actions to update the GH data if GH is authenticated and there's a user on the server", async () => {
      mockFetch.mockResolvedValue(
        new Response(JSON.stringify({ type: 'SUCCESS', user: { id: 'that-guy' } })),
      )
      const got = await getRefreshGithubActions(
        mockDispatch,
        true,
        null,
        null,
        null,
        null, // <- user details
        null,
        null,
        makeMockContext(),
        'user-initiated',
      )
      expect(got.length).toBe(1)
      const promises = await got[0]
      expect(promises.length).toBe(1)
      expect(promises[0].action).toBe('UPDATE_GITHUB_DATA')
      if (promises[0].action === 'UPDATE_GITHUB_DATA') {
        expect(promises[0].data).toEqual({ githubUserDetails: { id: 'that-guy' } })
      }
    })

    it('proceeds with the refresh if the GH data is authenticated and the user is there', async () => {
      mockFetch.mockImplementation(async () => {
        return new Response(
          JSON.stringify({
            type: 'SUCCESS',
            user: { id: 'that-guy' },
            repositories: [],
          }),
        )
      })

      const got = await getRefreshGithubActions(
        mockDispatch,
        true,
        null,
        null,
        null,
        { login: 'the-login', avatarURL: 'the-avatar', htmlURL: 'the-html-url', name: 'the-name' }, // <- user details
        null,
        null,
        makeMockContext(mockFetch.getMockImplementation()),
        'user-initiated',
      )
      expect(got.length).toBe(2)

      let promises = await got[0]
      expect(promises.length).toBe(1)
      expect(promises[0].action).toBe('UPDATE_GITHUB_DATA')
      if (promises[0].action === 'UPDATE_GITHUB_DATA') {
        expect(promises[0].data).toEqual({
          userRepositories: [],
        })
      }

      promises = await got[1]
      expect(promises.length).toBe(1)
      expect(promises[0].action).toBe('UPDATE_GITHUB_DATA')
      if (promises[0].action === 'UPDATE_GITHUB_DATA') {
        expect(promises[0].data).toEqual({
          branches: null,
        })
      }
    })
  })
})
