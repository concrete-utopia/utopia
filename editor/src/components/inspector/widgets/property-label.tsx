import * as React from 'react'
import * as R from 'ramda'

import * as PP from '../../../core/shared/property-path'
import { betterReactMemo, InspectorContextMenuWrapper } from 'uuiui-deps'
import { useInspectorInfoSimpleUntyped } from '../common/property-path-hooks'
import { optionalAddOnUnsetValues } from '../common/context-menu-items'
import { PropertyPath } from '../../../core/shared/project-file-types'

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
  const contextMenuItems = optionalAddOnUnsetValues(
    metadata.propertyStatus.set,
    props.propNamesToUnset ?? props.target.map(PP.lastPart),
    metadata.onUnsetValues,
  )
  return (
    <InspectorContextMenuWrapper
      id={`property-label-${props.target}`}
      data={null}
      items={contextMenuItems}
      style={{
        color: metadata.controlStyles.mainColor,
        ...(props.style ?? {}),
      }}
    >
      {props.children}
    </InspectorContextMenuWrapper>
  )
})
