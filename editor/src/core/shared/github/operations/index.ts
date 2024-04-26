import { updateProjectContentsWithParseResults } from '../../parser-projectcontents-utils'
import { resolveConflict, startGithubPolling } from '../helpers'
import { saveProjectToGithub } from './commit-and-push'
import { getBranchChecksums } from './get-branch-checksums'
import type { GithubOperationContext } from './github-operation-context'
import { getBranchesForGithubRepository } from './list-branches'
import { updatePullRequestsForBranch } from './list-pull-requests-for-branch'
import { saveAssetsToProject, updateProjectWithBranchContent } from './load-branch'
import { getUsersPublicGithubRepositories, searchPublicGithubRepository } from './load-repositories'
import { updateProjectAgainstGithub } from './update-against-branch'

const OperationContext: GithubOperationContext = {
  fetch: (...args) => window.fetch(...args),
  updateProjectContentsWithParseResults: updateProjectContentsWithParseResults,
}

export const GithubOperations = {
  saveProjectToGithub: saveProjectToGithub(OperationContext),
  getBranchCheckSums: getBranchChecksums(OperationContext),
  getBranchesForGithubRepository: getBranchesForGithubRepository(OperationContext),
  updatePullRequestsForBranch: updatePullRequestsForBranch(OperationContext),
  saveAssetsToProject: saveAssetsToProject(OperationContext),
  getUsersPublicGithubRepositories: getUsersPublicGithubRepositories(OperationContext),
  searchPublicGithubRepository: searchPublicGithubRepository(OperationContext),
  updateProjectAgainstGithub: updateProjectAgainstGithub(OperationContext),
  startGithubPolling: startGithubPolling(OperationContext),
  resolveConflict: resolveConflict(OperationContext),
  updateProjectWithBranchContent: updateProjectWithBranchContent(OperationContext),
} as const
