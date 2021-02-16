import * as React from 'react'
import { FlexWrap } from 'utopia-api'
import { ControlStatus, ControlStyles, getControlStyles } from '../../../common/control-status'
import { useInspectorLayoutInfo, useInspectorStyleInfo } from '../../../common/property-path-hooks'
import { GridRow } from '../../../widgets/grid-row'
import {
  FlexAlignContentControl,
  FlexAlignItemsControl,
  FlexGapControl,
  FlexWrapControl,
  FlexJustifyContentControl,
  FlexDirectionControl,
  getDirectionAwareLabels,
} from './flex-container-controls'
import { PropertyLabel } from '../../../widgets/property-label'
import { createLayoutPropertyPath } from '../../../../../core/layout/layout-helpers-new'
import { useWrappedEmptyOrUnknownOnSubmitValue } from '../../../../../uuiui'
import { betterReactMemo } from '../../../../../uuiui-deps'
import { emptyValues } from '../../../common/css-utils'

const flexGapProp = [createLayoutPropertyPath('FlexGap')]
const alignItemsProp = [createLayoutPropertyPath('alignItems')]

export const FlexContainerControls = betterReactMemo<{ seeMoreVisible: boolean }>(
  'FlexContainerControls',
  (props) => {
    // Right now flex layout isn't supported on groups, so just don't show the controls if a group is selected
    const flexWrap = useInspectorLayoutInfo('flexWrap')
    const flexDirection = useInspectorLayoutInfo('flexDirection')
    const alignItems = useInspectorLayoutInfo('alignItems')
    const alignContent = useInspectorLayoutInfo('alignContent')
    const justifyContent = useInspectorLayoutInfo('justifyContent')
    const flexGap = useInspectorLayoutInfo('FlexGap')

    const flexWrapValueOrDefault = flexWrap.value ?? emptyValues['flexWrap']
    const flexDirectionOrDefault = flexDirection.value ?? emptyValues['flexDirection']

    const {
      justifyFlexStart,
      justifyFlexEnd,
      alignDirection,
      alignItemsFlexStart,
      alignItemsFlexEnd,
      alignContentFlexStart,
      alignContentFlexEnd,
    } = getDirectionAwareLabels(flexWrapValueOrDefault, flexDirectionOrDefault)

    const alignItemsControlStatus: ControlStatus =
      flexWrap.value === FlexWrap.NoWrap ? 'disabled' : alignItems.controlStatus
    const alignItemsControlStyles: ControlStyles =
      flexWrap.value === FlexWrap.NoWrap ? getControlStyles('disabled') : alignItems.controlStyles

    const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
      flexGap.onSubmitValue,
      flexGap.onUnsetValues,
    )
    const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
      flexGap.onSubmitValue,
      flexGap.onUnsetValues,
    )

    return (
      <>
        <GridRow tall padded={true} type='<---1fr--->|------172px-------|'>
          <FlexDirectionControl
            value={flexDirection.value}
            controlStatus={flexDirection.controlStatus}
            controlStyles={flexDirection.controlStyles}
            onSubmitValue={flexDirection.onSubmitValue}
            onUnset={flexDirection.onUnsetValues}
            flexWrap={flexWrapValueOrDefault}
          />
          <FlexJustifyContentControl
            value={justifyContent.value}
            onSubmitValue={justifyContent.onSubmitValue}
            onUnset={justifyContent.onUnsetValues}
            controlStatus={justifyContent.controlStatus}
            controlStyles={justifyContent.controlStyles}
            flexDirection={flexDirectionOrDefault}
            justifyFlexStart={justifyFlexStart}
            justifyFlexEnd={justifyFlexEnd}
          />
        </GridRow>
        <GridRow padded={true} type='<---1fr--->|------172px-------|'>
          <PropertyLabel target={flexGapProp}>Gap</PropertyLabel>
          <FlexGapControl
            value={flexGap.value}
            onSubmitValue={wrappedOnSubmitValue}
            onTransientSubmitValue={wrappedOnTransientSubmitValue}
            onUnset={flexGap.onUnsetValues}
            controlStatus={flexGap.controlStatus}
            controlStyles={flexGap.controlStyles}
          />
        </GridRow>
        <GridRow padded={true} type='<---1fr--->|------172px-------|'>
          <PropertyLabel target={alignItemsProp}>Align</PropertyLabel>
          <FlexAlignItemsControl
            value={alignItems.value}
            controlStatus={alignItems.controlStatus}
            controlStyles={alignItems.controlStyles}
            onSubmitValue={alignItems.onSubmitValue}
            onUnset={alignItems.onUnsetValues}
            alignDirection={alignDirection}
            alignItemsFlexStart={alignItemsFlexStart}
            alignItemsFlexEnd={alignItemsFlexEnd}
          />
        </GridRow>
        <GridRow padded={true} type='<---1fr--->|------172px-------|'>
          <FlexWrapControl
            value={flexWrap.value}
            onSubmitValue={flexWrap.onSubmitValue}
            onUnset={flexWrap.onUnsetValues}
            controlStatus={flexWrap.controlStatus}
            controlStyles={flexWrap.controlStyles}
          />
          <FlexAlignContentControl
            value={alignContent.value}
            onSubmitValue={alignContent.onSubmitValue}
            onUnset={alignContent.onUnsetValues}
            controlStatus={alignItemsControlStatus}
            controlStyles={alignItemsControlStyles}
            alignDirection={alignDirection}
            alignContentFlexStart={alignContentFlexStart}
            alignContentFlexEnd={alignContentFlexEnd}
          />
        </GridRow>
      </>
    )
  },
)
