/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { css, jsx } from '@emotion/react'
import type { PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { optionalAddOnUnsetValues } from '../common/context-menu-items'
import { useInspectorInfoSimpleUntyped } from '../common/property-path-hooks'
import type { ControlStyles } from '../common/control-styles'
import { useColorTheme } from '../../../uuiui'

type PropertyLabelProps = {
  target: ReadonlyArray<PropertyPath>
  propNamesToUnset?: string[]
  style?: React.CSSProperties
  children: React.ReactNode
  controlStyles?: ControlStyles
}

function useMetadataInfoForDomain(target: ReadonlyArray<PropertyPath>) {
  return useInspectorInfoSimpleUntyped(
    target,
    (v) => v,
    (v) => v,
  )
}

export const PropertyLabel = React.memo((props: PropertyLabelProps) => {
  const colorTheme = useColorTheme()
  const metadata = useMetadataInfoForDomain(props.target)
  const propsToUnset = props.propNamesToUnset ?? props.target.map(PP.lastPart)
  const contextMenuItems = optionalAddOnUnsetValues(
    metadata.propertyStatus.set,
    propsToUnset,
    metadata.onUnsetValues,
  )

  const controlStyles = props.controlStyles ?? metadata.controlStyles

  return (
    <div
      css={{
        ...(props.style as any), // TODO Emotion and React 18 types don't like each other
        display: 'flex',
        alignItems: 'center',
        whiteSpace: 'nowrap',
        textOverflow: 'ellipsis',
        color: colorTheme.fg1.value,
      }}
    >
      {props.children}
    </div>
  )
})
PropertyLabel.displayName = 'PropertyLabel'
