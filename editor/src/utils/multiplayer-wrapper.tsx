import { ClientSideSuspense } from '@liveblocks/react'
import type { CommentProps } from '@liveblocks/react-comments'
import { Comment } from '@liveblocks/react-comments'
import type { CSSProperties } from 'react'
import React from 'react'
import { useStatus, type UserMeta } from '../../liveblocks.config'
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
    const roomStatus = useStatus()
    if (roomStatus !== 'connected') {
      return null
    }

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

export const baseMultiplayerAvatarStyle: CSSProperties = {
  position: 'absolute',
  top: 11,
  left: 11,
  zIndex: 1,
  width: 25.5, // matching the size of the liveblocks component
  height: 25.5, // matching the size of the liveblocks component
}

export type CommentWrapperProps = {
  user: UserMeta | null
} & CommentProps

export const CommentWrapper = React.memo((props: CommentWrapperProps) => {
  const { user, ...commentProps } = props

  if (user == null) {
    return <Comment {...commentProps} />
  }

  return (
    <div data-testid='comment-wrapper' style={{ position: 'relative' }}>
      <MultiplayerAvatar
        name={multiplayerInitialsFromName(normalizeMultiplayerName(user.name))}
        color={multiplayerColorFromIndex(null)}
        style={baseMultiplayerAvatarStyle}
        picture={user.avatar}
      />
      <Comment {...commentProps} />
    </div>
  )
})
CommentWrapper.displayName = 'CommentWrapper'
