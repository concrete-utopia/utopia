import { updateProjectContentsWithParseResults } from '../../parser-projectcontents-utils'
import { saveProjectToGithub } from './commit-and-push'
import { getBranchChecksums } from './get-branch-checksums'
import type { GithubOperationContext } from './github-operation-context'
import { getBranchesForGithubRepository } from './list-branches'
import { updatePullRequestsForBranch } from './list-pull-requests-for-branch'

const ProdOperationContext: GithubOperationContext = {
  fetch: fetch,
  updateProjectContentsWithParseResults: updateProjectContentsWithParseResults,
}

export const GithubOperations = {
  saveProjectToGithub: saveProjectToGithub(ProdOperationContext),
  getBranchCheckSums: getBranchChecksums(ProdOperationContext),
  getBranchesForGithubRepository: getBranchesForGithubRepository(ProdOperationContext),
  updatePullRequestsForBranch: updatePullRequestsForBranch(ProdOperationContext),
} as const
