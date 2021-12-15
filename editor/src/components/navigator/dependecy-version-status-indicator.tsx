import React from 'react'
import type { PackageStatus } from '../../core/shared/npm-dependency-types'

export const NpmDependencyVersionAndStatusIndicator = React.memo<{
  status: PackageStatus
  version: string | null
}>((props) => {
  switch (props.status) {
    case 'loaded':
    case 'default-package':
      return <span>{props.version}</span>
    case 'loading':
      return (
        <span>
          <img
            src='/editor/animated-icons/animation-loading-14x12@2x.gif'
            width={14}
            height={12}
            style={{ marginRight: 8, position: 'relative', top: 2 }}
          />
          loading…
        </span>
      )
    case 'updating':
      return (
        <span>
          <img
            src='/editor/animated-icons/animation-loading-14x12@2x.gif'
            width={14}
            height={12}
            style={{ marginRight: 8, position: 'relative', top: 2, filter: 'saturate(0%)' }}
          />
          updating...
        </span>
      )
    case 'version-lookup':
      return (
        <span>
          <img
            src='/editor/animated-icons/animation-loading-14x12@2x.gif'
            width={14}
            height={12}
            style={{ marginRight: 8, position: 'relative', top: 2, filter: 'saturate(0%)' }}
          />
          preparing...
        </span>
      )
    case 'error':
      return <span>failed to load</span>
    case 'not-found':
      return <span>not found</span>
    default:
      const _exhaustiveCheck: never = props.status
      throw new Error(`Unhandled PackageStatus ${JSON.stringify(props.status)}.`)
  }
})
