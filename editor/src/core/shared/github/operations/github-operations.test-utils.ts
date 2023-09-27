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
import type { EditorDispatch } from '../../../../components/editor/action-types'
import { setGithubState } from '../../../../components/editor/actions/action-creators'
import { getUsersPublicGithubRepositories } from './load-repositories'
import { updateProjectAgainstGithub } from './update-against-branch'
import { resolveConflict, startGithubPolling } from '../helpers'

export function setGithubAuthenticatedForTests(dispatch: EditorDispatch) {
  dispatch([setGithubState({ authenticated: true })], 'everyone')
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
  operation: (...operationParameters: Args) => Promise<Return>,
): (...parameters: Args) => Promise<Return> {
  return (...args) => {
    const result = operation(...args)
    deferred.current.resolve()
    deferred.current = defer()
    return result
  }
}

export class MockGithubOperations {
  githubOperationDone: Defer<void> = defer()
  sandbox: Sinon.SinonSandbox | null = null

  mock(options: MockOperationContextOptions): MockGithubOperations {
    beforeEach(() => {
      this.sandbox = Sinon.createSandbox()
      this.githubOperationDone = defer()

      const startGithubAuthenticationStub = this.sandbox.stub(
        GithubAuth,
        'startGithubAuthentication',
      )
      startGithubAuthenticationStub.callsFake(async () => {})

      const isAuthenticatedWithGithubStub = this.sandbox.stub(
        GithubAuth,
        'isAuthenticatedWithGithub',
      )
      isAuthenticatedWithGithubStub.callsFake(async () => true)

      const operationContext = MockOperationContext(options)

      const saveProjectToGithubStub = this.sandbox.stub(GithubOperations, 'saveProjectToGithub')
      saveProjectToGithubStub.callsFake(
        wrapWithDeferResolve(
          { current: this.githubOperationDone },
          saveProjectToGithub(operationContext),
        ),
      )

      const getBranchCheckSumsStub = this.sandbox.stub(GithubOperations, 'getBranchCheckSums')
      getBranchCheckSumsStub.callsFake(
        wrapWithDeferResolve(
          { current: this.githubOperationDone },
          getBranchChecksums(operationContext),
        ),
      )

      const getBranchesForGithubRepositoryStub = this.sandbox.stub(
        GithubOperations,
        'getBranchesForGithubRepository',
      )
      getBranchesForGithubRepositoryStub.callsFake(
        wrapWithDeferResolve(
          { current: this.githubOperationDone },
          getBranchesForGithubRepository(operationContext),
        ),
      )

      const updatePullRequestsForBranchStub = this.sandbox.stub(
        GithubOperations,
        'updatePullRequestsForBranch',
      )
      updatePullRequestsForBranchStub.callsFake(
        wrapWithDeferResolve(
          { current: this.githubOperationDone },
          updatePullRequestsForBranch(operationContext),
        ),
      )

      const saveAssetsToProjectStub = this.sandbox.stub(GithubOperations, 'saveAssetsToProject')
      saveAssetsToProjectStub.callsFake(
        wrapWithDeferResolve(
          { current: this.githubOperationDone },
          saveAssetsToProject(operationContext),
        ),
      )

      const getUsersPublicGithubRepositoriesStub = this.sandbox.stub(
        GithubOperations,
        'getUsersPublicGithubRepositories',
      )
      getUsersPublicGithubRepositoriesStub.callsFake(
        wrapWithDeferResolve(
          { current: this.githubOperationDone },
          getUsersPublicGithubRepositories(operationContext),
        ),
      )

      const updateProjectAgainstGithubStub = this.sandbox.stub(
        GithubOperations,
        'updateProjectAgainstGithub',
      )
      updateProjectAgainstGithubStub.callsFake(
        wrapWithDeferResolve(
          { current: this.githubOperationDone },
          updateProjectAgainstGithub(operationContext),
        ),
      )

      const startGithubPollingStub = this.sandbox.stub(GithubOperations, 'startGithubPolling')
      startGithubPollingStub.callsFake(
        wrapWithDeferResolve(
          { current: this.githubOperationDone },
          startGithubPolling(operationContext),
        ),
      )

      const resolveConflictStub = this.sandbox.stub(GithubOperations, 'resolveConflict')
      resolveConflictStub.callsFake(
        wrapWithDeferResolve(
          { current: this.githubOperationDone },
          resolveConflict(operationContext),
        ),
      )

      const updateProjectWithBranchContentStub = this.sandbox.stub(
        GithubOperations,
        'updateProjectWithBranchContent',
      )
      updateProjectWithBranchContentStub.callsFake(
        wrapWithDeferResolve(
          { current: this.githubOperationDone },
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
    this.githubOperationDone = defer()
  }
}
