import { getBranchProjectContents } from '../../../../components/editor/server'
import { resolveConflict } from '../helpers'
import { saveProjectToGithub } from './commit-and-push'
import { getBranchChecksums } from './get-branch-checksums'
import { OperationContext } from './github-operation-context'
import { getBranchesForGithubRepository } from './list-branches'
import { updatePullRequestsForBranch } from './list-pull-requests-for-branch'
import { saveAssetsToProject, updateProjectWithBranchContent } from './load-branch'
import { getUsersPublicGithubRepositories, searchPublicGithubRepository } from './load-repositories'
import { updateProjectAgainstGithub } from './update-against-branch'

export const GithubOperations = {
  saveProjectToGithub: saveProjectToGithub(OperationContext),
  getBranchCheckSums: getBranchChecksums(OperationContext),
  getBranchesForGithubRepository: getBranchesForGithubRepository(OperationContext),
  updatePullRequestsForBranch: updatePullRequestsForBranch(OperationContext),
  saveAssetsToProject: saveAssetsToProject(OperationContext),
  getUsersPublicGithubRepositories: getUsersPublicGithubRepositories(OperationContext),
  searchPublicGithubRepository: searchPublicGithubRepository(OperationContext),
  updateProjectAgainstGithub: updateProjectAgainstGithub(OperationContext),
  resolveConflict: resolveConflict(OperationContext),
  updateProjectWithBranchContent: updateProjectWithBranchContent(OperationContext),
  getBranchProjectContents: getBranchProjectContents(OperationContext),
} as const
