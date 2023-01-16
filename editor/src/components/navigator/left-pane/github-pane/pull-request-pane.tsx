/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { Substores, useEditorState } from '../../../../components/editor/store/store-hook'
import { UIGridRow } from '../../../../components/inspector/widgets/ui-grid-row'
import { FlexColumn } from '../../../../uuiui'

export const PullRequestPane = React.memo(() => {
  const pullRequests = useEditorState(
    Substores.github,
    (store) => {
      return store.editor.githubData.currentBranchPullRequests
    },
    'PullRequestPane pullRequest',
  )

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
                borderRadius: 3,
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
