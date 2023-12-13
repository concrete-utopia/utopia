import React from 'react'
import { CommentSection } from './sections/comment-section'
import { FlexColumn, Section } from '../../uuiui'

export const CommentsPane = React.memo(() => {
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
