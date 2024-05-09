import urljoin from 'url-join'
import { UTOPIA_BACKEND, UTOPIA_BACKEND_BASE_URL } from '../../../common/env-vars'
import type { GithubRepo } from '../../../components/editor/store/editor-state'

export const GithubEndpoints = {
  repositories: () => urljoin(UTOPIA_BACKEND, 'github', 'user', 'repositories'),
  searchRepository: () =>
    urljoin(UTOPIA_BACKEND_BASE_URL, 'internal', 'github', 'repositories', 'search'),
  userDetails: () => urljoin(UTOPIA_BACKEND, 'github', 'user'),
  save: (projectID: string) => urljoin(UTOPIA_BACKEND, 'github', 'save', projectID),
  getBranches: ({ owner, repository }: GithubRepo) =>
    urljoin(UTOPIA_BACKEND_BASE_URL, 'internal', 'github', 'branches', owner, repository),
  getBranchProjectContents: (projectID: string, owner: string, repo: string, branch: string) =>
    urljoin(
      UTOPIA_BACKEND_BASE_URL,
      'internal',
      'projects',
      projectID,
      'github',
      'branches',
      owner,
      repo,
      'branch',
      encodeURIComponent(branch),
    ),
  branchContents: (githubRepo: GithubRepo, branchName: string) =>
    urljoin(
      UTOPIA_BACKEND,
      'github',
      'branches',
      githubRepo.owner,
      githubRepo.repository,
      'branch',
      encodeURIComponent(branchName),
    ),
  defaultBranchContents: (githubRepo: GithubRepo) =>
    urljoin(
      UTOPIA_BACKEND,
      'github',
      'branches',
      githubRepo.owner,
      githubRepo.repository,
      'default-branch',
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
      encodeURIComponent(branchName),
      'pullrequest',
    ),
  authenticationStatus: () => urljoin(UTOPIA_BACKEND, 'github', 'authentication', 'status'),
  authenticationStart: () => urljoin(UTOPIA_BACKEND, 'github', 'authentication', 'start'),
} as const
