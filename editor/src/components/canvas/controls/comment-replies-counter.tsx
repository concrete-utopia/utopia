import React from 'react'
import type { ThreadData } from '@liveblocks/client'
import type { ThreadMetadata } from '../../../../liveblocks.config'
import { useColorTheme } from '../../../uuiui'
import { darkColorThemeCssVariables } from '../../../uuiui/styles/theme/utopia-theme'
import { dark } from '../../../uuiui/styles/theme/dark'
import { useTheme } from '@emotion/react'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { getCurrentTheme } from '../../editor/store/editor-state'

interface CommentRepliesCounterProps {
  thread: ThreadData<ThreadMetadata>
  forceDarkMode?: boolean
}

export const CommentRepliesCounter = React.memo((props: CommentRepliesCounterProps) => {
  const colorTheme = useColorTheme()

  const theme = useEditorState(
    Substores.userState,
    (store) => getCurrentTheme(store.userState),
    'CommentRepliesCounter theme',
  )

  const color = React.useMemo(() => {
    // If we don't force dark mode because of a blue background, we can just use foreground color
    if (!props.forceDarkMode) {
      return colorTheme.fg6.value
    }
    // In dark mode we need a lighter foreground which is visible well on blue background
    if (theme === 'dark') {
      return colorTheme.fg2.value
    }
    // In light mode we need a light background color, so it is visible on blue background
    return colorTheme.bg3.value
  }, [colorTheme, theme, props.forceDarkMode])

  const repliesCount = props.thread.comments.filter((c) => c.deletedAt == null).length - 1

  if (repliesCount <= 0) {
    return <div />
  }

  return (
    <div
      style={{
        paddingLeft: 44,
        fontSize: 9,
        color: color,
        marginBottom: 8,
      }}
    >
      {repliesCount} {repliesCount > 1 ? 'replies' : 'reply'}
    </div>
  )
})
CommentRepliesCounter.displayName = 'CommentRepliesCounter'
