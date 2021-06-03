/**@jsx jsx */
import * as React from 'react'
import { css, jsx } from '@emotion/react'
import { PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { betterReactMemo, InspectorContextMenuWrapper } from '../../../uuiui-deps'
import { optionalAddOnUnsetValues } from '../common/context-menu-items'
import { useInspectorInfoSimpleUntyped } from '../common/property-path-hooks'

type PropertyLabelProps = {
  target: ReadonlyArray<PropertyPath>
  propNamesToUnset?: string[]
  style?: React.CSSProperties
  children: React.ReactNode
}

function useMetadataInfoForDomain(target: ReadonlyArray<PropertyPath>) {
  return useInspectorInfoSimpleUntyped(
    target,
    (v) => v,
    (v) => v,
  )
}

export const PropertyLabel = betterReactMemo('PropertyLabel', (props: PropertyLabelProps) => {
  const metadata = useMetadataInfoForDomain(props.target)
  const propsToUnset = props.propNamesToUnset ?? props.target.map(PP.lastPart)
  const contextMenuItems = optionalAddOnUnsetValues(
    metadata.propertyStatus.set,
    propsToUnset,
    metadata.onUnsetValues,
  )
  return (
    <InspectorContextMenuWrapper
      id={`property-label-${propsToUnset.join('-')}`}
      data={null}
      items={contextMenuItems}
      style={{
        color: 'var(--special-effect)',
        overflow: 'hidden',
        ...props.style,
      }}
    >
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          overflowX: 'scroll',
          whiteSpace: 'nowrap',
          textOverflow: 'ellipsis',
        }}
      >
        {props.children}
      </div>
    </InspectorContextMenuWrapper>
  )
})
