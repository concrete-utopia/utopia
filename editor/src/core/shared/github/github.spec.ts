import type { ProjectContentTreeRoot } from '../../../components/assets'
import type { EditorDispatch } from '../../../components/editor/action-types'
import { updateGithubData } from '../../../components/editor/actions/action-creators'
import type { RepositoryEntry } from './helpers'
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

    function mockFetchUser() {
      // TODO this is because every runGithubOperation calls getUserDetailsFromServer, this should really go!
      mockFetch.mockResolvedValueOnce(
        new Response(
          JSON.stringify({
            type: 'SUCCESS',
            user: { login: 'bob' },
          }),
          { status: 200 },
        ),
      )
    }

    describe('when the repo is null', () => {
      it('returns the public repos and nullifies the branches', async () => {
        mockFetchUser()
        mockFetch.mockResolvedValueOnce(
          new Response(
            JSON.stringify({
              type: 'SUCCESS',
              repositories: [{ name: 'foo' }, { name: 'bar' }],
            }),
            { status: 200 },
          ),
        )

        const got = await getRefreshGithubActions(
          mockDispatch,
          null,
          null,
          null,
          null,
          null,
          makeMockContext(),
          'user-initiated',
        )
        expect(got.length).toBe(2)

        expect(got).toEqual([
          updateGithubData({
            userRepositories: [
              { name: 'foo' } as RepositoryEntry,
              { name: 'bar' } as RepositoryEntry,
            ],
          }),
          updateGithubData({ branches: null }),
        ])
      })
    })
    describe('when the repo is not null', () => {
      it('gets the branches and the upstream changes', async () => {
        mockFetch.mockResolvedValueOnce(
          new Response(
            JSON.stringify({
              type: 'SUCCESS',
              repositories: [{ name: 'foo' }, { name: 'bar' }],
            }),
            { status: 200 },
          ),
        )

        mockFetchUser()
        mockFetch.mockResolvedValueOnce(
          new Response(
            JSON.stringify({
              type: 'SUCCESS',
              branches: [{ name: 'one' }, { name: 'two' }],
            }),
            { status: 200 },
          ),
        )

        const got = await getRefreshGithubActions(
          mockDispatch,
          { owner: 'foo', repository: 'bar' },
          null,
          null,
          null,
          null,
          makeMockContext(),
          'user-initiated',
        )
        expect(got.length).toBe(2)

        expect(got).toEqual([
          updateGithubData({ branches: [{ name: 'one' }, { name: 'two' }] }),
          updateGithubData({ upstreamChanges: null }),
        ])
      })
    })
  })
})
