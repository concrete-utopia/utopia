import React from 'react'
import { CommentSection } from './sections/comment-section'
import { FlexColumn, Section } from '../../uuiui'
import { useIsLoggedIn } from '../../core/shared/multiplayer-hooks'

export const CommentsPane = React.memo(() => {
  const isLoggedIn = useIsLoggedIn()

  if (!isLoggedIn) {
    return null
  }

  return (
    <FlexColumn
      id='leftPaneSettings'
      key='leftPaneSettings'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
        overflowY: 'scroll',
        alignSelf: 'stretch',
      }}
    >
      <Section>
        <CommentSection />
      </Section>
    </FlexColumn>
  )
})
CommentsPane.displayName = 'CommentsPane'
