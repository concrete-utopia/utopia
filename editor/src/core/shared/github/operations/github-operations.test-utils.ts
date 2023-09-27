import type { Defer } from '../../../../utils/utils'
import { defer } from '../../../../utils/utils'
import Sinon from 'sinon'
import type { GithubOperationContext } from './github-operation-context'
import { GithubOperations } from '.'
import { saveProjectToGithub } from './commit-and-push'
import { getBranchChecksums } from './get-branch-checksums'
import type { ProjectContentTreeRoot } from '../../../../components/assets'
import { getBranchesForGithubRepository } from './list-branches'
import { updatePullRequestsForBranch } from './list-pull-requests-for-branch'
import { saveAssetsToProject, updateProjectWithBranchContent } from './load-branch'
import { GithubAuth } from '../../../../utils/github-auth'
import {
  setGithubState,
  setLoginState,
} from '../../../../components/editor/actions/action-creators'
import { getUsersPublicGithubRepositories } from './load-repositories'
import { updateProjectAgainstGithub } from './update-against-branch'
import { resolveConflict, startGithubPolling } from '../helpers'
import type { AsyncEditorDispatch } from '../../../../components/canvas/ui-jsx.test-utils'

export async function loginUserToGithubForTests(dispatch: AsyncEditorDispatch) {
  await dispatch(
    [
      setLoginState({ type: 'LOGGED_IN', user: { userId: 'user' } }),
      setGithubState({ authenticated: true }),
    ],
    true,
  )
}

export function fakeResponse<T>(payload: T): Response {
  return {
    ok: true,
    json: async () => payload,
  } as Response
}

interface FakeGithubApiConfig {
  [url: string]: Response | undefined
}

interface MockOperationContextOptions {
  apiConfig: FakeGithubApiConfig
  fakeResolvedProjectContents: ProjectContentTreeRoot
}

const MockOperationContext = (options: MockOperationContextOptions): GithubOperationContext => ({
  fetch: async (url) => {
    const response = options.apiConfig[url]
    if (response == null) {
      throw new Error(`No response defined for URL ${url}`)
    }
    return response
  },
  updateProjectContentsWithParseResults: async () => options.fakeResolvedProjectContents,
})

function wrapWithDeferResolve<Args extends any[], Return>(
  deferred: { current: Defer<void> },
  name: string,
  operation: (...operationParameters: Args) => Promise<Return>,
): (...parameters: Args) => Promise<Return> {
  return (...args) => {
    // console.log(`>>> calling ${name}`)
    const result = operation(...args)
    deferred.current.resolve()
    deferred.current = defer()
    return result
  }
}

export class MockGithubOperations {
  private sandbox: Sinon.SinonSandbox | null = null
  private privatePromise: Defer<void> = defer()
  getUsersPublicGithubRepositories: Defer<void> = defer()
  getBranchesForGithubRepository: Defer<void> = defer()
  updateProjectWithBranchContent: Defer<void> = defer()

  mock(options: MockOperationContextOptions): MockGithubOperations {
    beforeEach(() => {
      this.sandbox = Sinon.createSandbox()
      this.privatePromise = defer()

      const startGithubAuthenticationStub = this.sandbox.stub(
        GithubAuth,
        'startGithubAuthentication',
      )
      startGithubAuthenticationStub.callsFake(
        wrapWithDeferResolve(
          { current: this.privatePromise },
          'startGithubAuthentication',
          async () => {},
        ),
      )

      const isAuthenticatedWithGithubStub = this.sandbox.stub(
        GithubAuth,
        'isAuthenticatedWithGithub',
      )
      isAuthenticatedWithGithubStub.callsFake(
        wrapWithDeferResolve(
          { current: this.privatePromise },
          'isAuthenticatedWithGithub',
          async () => true,
        ),
      )

      const operationContext = MockOperationContext(options)

      const saveProjectToGithubStub = this.sandbox.stub(GithubOperations, 'saveProjectToGithub')
      saveProjectToGithubStub.callsFake(
        wrapWithDeferResolve(
          { current: this.privatePromise },
          'saveProjectToGithub',
          saveProjectToGithub(operationContext),
        ),
      )

      const getBranchCheckSumsStub = this.sandbox.stub(GithubOperations, 'getBranchCheckSums')
      getBranchCheckSumsStub.callsFake(
        wrapWithDeferResolve(
          { current: this.privatePromise },
          'getBranchCheckSums',
          getBranchChecksums(operationContext),
        ),
      )

      const getBranchesForGithubRepositoryStub = this.sandbox.stub(
        GithubOperations,
        'getBranchesForGithubRepository',
      )
      getBranchesForGithubRepositoryStub.callsFake(
        wrapWithDeferResolve(
          { current: this.getBranchesForGithubRepository },
          'getBranchesForGithubRepository',
          getBranchesForGithubRepository(operationContext),
        ),
      )

      const updatePullRequestsForBranchStub = this.sandbox.stub(
        GithubOperations,
        'updatePullRequestsForBranch',
      )
      updatePullRequestsForBranchStub.callsFake(
        wrapWithDeferResolve(
          { current: this.privatePromise },
          'updatePullRequestsForBranch',
          updatePullRequestsForBranch(operationContext),
        ),
      )

      const saveAssetsToProjectStub = this.sandbox.stub(GithubOperations, 'saveAssetsToProject')
      saveAssetsToProjectStub.callsFake(
        wrapWithDeferResolve(
          { current: this.privatePromise },
          'saveAssetsToProject',
          saveAssetsToProject(operationContext),
        ),
      )

      const getUsersPublicGithubRepositoriesStub = this.sandbox.stub(
        GithubOperations,
        'getUsersPublicGithubRepositories',
      )
      getUsersPublicGithubRepositoriesStub.callsFake(
        wrapWithDeferResolve(
          { current: this.getUsersPublicGithubRepositories },
          'getUsersPublicGithubRepositories',
          getUsersPublicGithubRepositories(operationContext),
        ),
      )

      const updateProjectAgainstGithubStub = this.sandbox.stub(
        GithubOperations,
        'updateProjectAgainstGithub',
      )
      updateProjectAgainstGithubStub.callsFake(
        wrapWithDeferResolve(
          { current: this.privatePromise },
          'updateProjectAgainstGithub',
          updateProjectAgainstGithub(operationContext),
        ),
      )

      const startGithubPollingStub = this.sandbox.stub(GithubOperations, 'startGithubPolling')
      startGithubPollingStub.callsFake(
        wrapWithDeferResolve(
          { current: this.privatePromise },
          'startGithubPolling',
          startGithubPolling(operationContext),
        ),
      )

      const resolveConflictStub = this.sandbox.stub(GithubOperations, 'resolveConflict')
      resolveConflictStub.callsFake(
        wrapWithDeferResolve(
          { current: this.privatePromise },
          'resolveConflict',
          resolveConflict(operationContext),
        ),
      )

      const updateProjectWithBranchContentStub = this.sandbox.stub(
        GithubOperations,
        'updateProjectWithBranchContent',
      )
      updateProjectWithBranchContentStub.callsFake(
        wrapWithDeferResolve(
          { current: this.updateProjectWithBranchContent },
          'updateProjectWithBranchContent',
          updateProjectWithBranchContent(operationContext),
        ),
      )
    })

    afterEach(() => {
      this.sandbox?.restore()
      this.sandbox = null
    })

    return this
  }

  resetDoneSignal(): void {
    this.privatePromise = defer()
  }
}
