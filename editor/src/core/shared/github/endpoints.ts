import urljoin from 'url-join'
import { UTOPIA_BACKEND } from '../../../common/env-vars'
import type { GithubRepo } from '../../../components/editor/store/editor-state'

export const GithubEndpoints = {
  repositories: () => urljoin(UTOPIA_BACKEND, 'github', 'user', 'repositories'),
  userDetails: () => urljoin(UTOPIA_BACKEND, 'github', 'user'),
  save: (projectID: string) => urljoin(UTOPIA_BACKEND, 'github', 'save', projectID),
  getBranches: ({ owner, repository }: GithubRepo) =>
    urljoin(UTOPIA_BACKEND, 'github', 'branches', owner, repository),
  branchContents: (githubRepo: GithubRepo, branchName: string) =>
    urljoin(
      UTOPIA_BACKEND,
      'github',
      'branches',
      githubRepo.owner,
      githubRepo.repository,
      'branch',
      branchName,
    ),
  asset: (githubRepo: GithubRepo, assetSha: string) =>
    urljoin(
      UTOPIA_BACKEND,
      'github',
      'branches',
      githubRepo.owner,
      githubRepo.repository,
      'asset',
      assetSha,
    ),
  updatePullRequests: (githubRepo: GithubRepo, branchName: string) =>
    urljoin(
      UTOPIA_BACKEND,
      'github',
      'branches',
      githubRepo.owner,
      githubRepo.repository,
      'branch',
      branchName,
      'pullrequest',
    ),
  authenticationStatus: () => urljoin(UTOPIA_BACKEND, 'github', 'authentication', 'status'),
  authenticationStart: () => urljoin(UTOPIA_BACKEND, 'github', 'authentication', 'start'),
} as const
