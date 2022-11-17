/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { isGithubListingPullRequestsForBranch } from '../../../../components/editor/store/editor-state'
import { useEditorState } from '../../../../components/editor/store/store-hook'
import { UIGridRow } from '../../../../components/inspector/widgets/ui-grid-row'
import { updatePullRequestsForBranch } from '../../../../core/shared/github'
import { FlexColumn } from '../../../../uuiui'
import { Button } from '../../../../uuiui/button'
import { GithubSpinner } from './github-spinner'
import { RefreshIcon } from './refresh-icon'

export const PullRequestPane = React.memo(() => {
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

  const openBlank = React.useCallback(
    (url: string) => () => {
      window.open(url, '_blank')
    },
    [],
  )

  if (pullRequests == null) {
    return null
  }

  return (
    <UIGridRow padded={false} variant='<-------------1fr------------->' style={{ width: '100%' }}>
      <UIGridRow padded={false} variant='<-------------1fr------------->'>
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
      <FlexColumn
        style={{
          border: '1px solid #2D2E33',
          borderRadius: 3,
          gap: 4,
        }}
      >
        {pullRequests.map((pr, index) => {
          return (
            <UIGridRow
              key={`pull-request-${index}`}
              padded={true}
              variant='|--16px--|<--------auto-------->'
              css={{
                '&:hover': {
                  backgroundColor: '#eee',
                  cursor: 'pointer',
                },
              }}
              onClick={openBlank(pr.htmlURL)}
            >
              <span>#{pr.number}</span>
              <span>{pr.title}</span>
            </UIGridRow>
          )
        })}
      </FlexColumn>
    </UIGridRow>
  )
})
