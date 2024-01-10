import { ClientSideSuspense } from '@liveblocks/react'
import type { CommentProps } from '@liveblocks/react-comments'
import { Comment } from '@liveblocks/react-comments'
import React from 'react'
import type { UserMeta } from '../../liveblocks.config'
import { MultiplayerAvatar } from '../components/user-bar'
import {
  multiplayerColorFromIndex,
  multiplayerInitialsFromName,
  normalizeMultiplayerName,
} from '../core/shared/multiplayer'
import { ErrorBoundary } from './react-error-boundary'

type Fallback = NonNullable<React.ReactNode> | null

export const MultiplayerWrapper = React.memo(
  (props: { errorFallback: Fallback; suspenseFallback: Fallback; children: any }) => {
    return (
      <ErrorBoundary fallback={props.errorFallback}>
        <ClientSideSuspense fallback={props.suspenseFallback}>
          {() => props.children}
        </ClientSideSuspense>
      </ErrorBoundary>
    )
  },
)
MultiplayerWrapper.displayName = 'MultiplayerWrapper'

export const CommentWrapper = React.memo(
  ({ user, ...commentProps }: { user: UserMeta | null } & CommentProps) => {
    if (user == null) {
      return <Comment {...commentProps} />
    }
    return (
      <div data-testid='comment-wrapper' style={{ position: 'relative' }}>
        <MultiplayerAvatar
          name={multiplayerInitialsFromName(normalizeMultiplayerName(user.name))}
          color={multiplayerColorFromIndex(user.colorIndex)}
          style={{
            position: 'absolute',
            top: 11,
            zIndex: 1,
            left: 11,
            width: 25.5, // matching the size of the liveblocks component
            height: 25.5, // matching the size of the liveblocks component
          }}
          picture={user.avatar}
        />
        <Comment {...commentProps} />
      </div>
    )
  },
)
CommentWrapper.displayName = 'CommentWrapper'
