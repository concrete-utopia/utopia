import React from 'react'
import { Button } from '../../../../uuiui/button'
import urljoin from 'url-join'
import { useEditorState } from '../../../../components/editor/store/store-hook'
import { UIGridRow } from '../../../../components/inspector/widgets/ui-grid-row'
import { GithubSpinner } from './github-spinner'
import { RefreshIcon } from './refresh-icon'
import { updatePullRequestsForBranch } from '../../../../core/shared/github'
import {
  isGithubListingPullRequestsForBranch,
  PullRequest,
} from '../../../../components/editor/store/editor-state'

export const PullRequestPane = () => {
  const dispatch = useEditorState((store) => store.dispatch, 'PullRequestPane dispatch')

  const githubRepo = useEditorState((store) => {
    return store.editor.githubSettings.targetRepository
  }, 'PullRequestPane githubRepo')

  const branchName = useEditorState((store) => {
    return store.editor.githubSettings.branchName
  }, 'PullRequestPane branch')

  const pullRequests = useEditorState((store) => {
    return store.editor.githubData.currentBranchPullRequests
  }, 'PullRequestPane pullRequest')

  const githubOperations = useEditorState(
    (store) => store.editor.githubOperations,
    'PullRequestPane githubOperations',
  )

  const githubWorking = React.useMemo(() => {
    return githubOperations.length > 0
  }, [githubOperations])

  const isUpdatingPullRequests = React.useMemo(() => {
    if (githubRepo != null && branchName != null) {
      return isGithubListingPullRequestsForBranch(githubOperations, githubRepo, branchName)
    } else {
      return false
    }
  }, [githubOperations, githubRepo, branchName])

  const refreshPullRequests = React.useCallback(() => {
    if (githubRepo != null && branchName != null) {
      void updatePullRequestsForBranch(dispatch, githubRepo, branchName)
    }
  }, [dispatch, githubRepo, branchName])

  const pullRequestsWithCreateNew: Array<PullRequest> | null = React.useMemo(() => {
    if (githubRepo != null && branchName != null) {
      const createPullRequestURL = urljoin(
        'https://github.com',
        githubRepo.owner,
        githubRepo.repository,
        'compare',
        branchName,
      )
      const createPullRequestURLWithParams = `${createPullRequestURL}?expand=1`
      return [
        ...(pullRequests ?? []),
        {
          title: 'Create PR on Github.',
          htmlURL: createPullRequestURLWithParams,
        },
      ]
    } else {
      return null
    }
  }, [pullRequests, githubRepo, branchName])

  if (pullRequestsWithCreateNew == null) {
    return null
  } else {
    return (
      <>
        <UIGridRow padded variant='<----------1fr---------><-auto->'>
          <span style={{ fontWeight: 500 }}>Pull Requests</span>
          <Button
            spotlight
            highlight
            style={{ padding: '0 6px' }}
            disabled={githubWorking}
            onMouseDown={refreshPullRequests}
          >
            {isUpdatingPullRequests ? <GithubSpinner /> : <RefreshIcon />}
          </Button>
        </UIGridRow>
        <UIGridRow padded variant='<----------1fr---------><-auto->'>
          <div
            style={{
              display: 'flex',
              flexDirection: 'column',
            }}
          >
            {pullRequestsWithCreateNew.map((pr, index) => {
              return (
                <div
                  key={`pull-request-${index}`}
                  style={{
                    gap: 8,
                    paddingLeft: 8,
                    paddingRight: 8,
                    paddingBottom: 8,
                    paddingTop: 8,
                    width: '100%',
                    maxHeight: 220,
                    overflowY: 'scroll',
                  }}
                >
                  <a href={pr.htmlURL} target='_blank' rel='noopener noreferrer'>
                    {pr.title}
                  </a>
                </div>
              )
            })}
          </div>
        </UIGridRow>
      </>
    )
  }
}
