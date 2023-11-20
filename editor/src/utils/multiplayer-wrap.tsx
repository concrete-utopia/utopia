import React from 'react'
import { ErrorBoundary } from './react-error-boundary'
import { ClientSideSuspense } from '@liveblocks/react'

export const MultiplayerWrap = React.memo(
  (props: {
    errorFallback: React.ReactNode
    suspenseFallback: NonNullable<React.ReactNode> | null
    children: any
  }) => {
    return (
      <ErrorBoundary fallback={props.errorFallback}>
        <ClientSideSuspense fallback={props.suspenseFallback}>
          {() => props.children}
        </ClientSideSuspense>
      </ErrorBoundary>
    )
  },
)
