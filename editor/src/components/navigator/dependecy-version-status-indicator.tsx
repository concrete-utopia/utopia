import * as React from 'react'
import { betterReactMemo } from '../../uuiui-deps'
import type { PackageStatus } from '../../core/shared/npm-dependency-types'

export const NpmDependencyVersionAndStatusIndicator = betterReactMemo<{
  status: PackageStatus
  version: string | null
}>('NpmDependencyVersionAndStatusIndicator', (props) => {
  switch (props.status) {
    case 'loaded':
    case 'default-package':
      return <span>{props.version}</span>
    case 'loading':
    case 'updating':
    case 'version-lookup':
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
    case 'error':
      return <span>failed to load</span>
    default:
      const _exhaustiveCheck: never = props.status
      throw new Error(`Unhandled PackageStatus ${JSON.stringify(props.status)}.`)
  }
})
