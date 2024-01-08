import React from 'react'
import type { ThreadData } from '@liveblocks/client'
import type { ThreadMetadata } from '../../../../liveblocks.config'
import { useColorTheme } from '../../../uuiui'

interface CommentRepliesCounterProps {
  thread: ThreadData<ThreadMetadata>
}

export const CommentRepliesCounter = React.memo((props: CommentRepliesCounterProps) => {
  const colorTheme = useColorTheme()

  const repliesCount = props.thread.comments.filter((c) => c.deletedAt == null).length - 1

  if (repliesCount <= 0) {
    return <div />
  }

  return (
    <div
      style={{
        paddingLeft: 44,
        fontSize: 9,
        color: colorTheme.fg6.value,
        marginBottom: 8,
      }}
    >
      {repliesCount} {repliesCount > 1 ? 'replies' : 'reply'}
    </div>
  )
})
CommentRepliesCounter.displayName = 'CommentRepliesCounter'
