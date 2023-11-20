import React from 'react'
import { ErrorBoundary } from './react-error-boundary'
import { ClientSideSuspense } from '@liveblocks/react'

type Fallback = NonNullable<React.ReactNode> | null

export const MultiplayerWrap = React.memo(
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
