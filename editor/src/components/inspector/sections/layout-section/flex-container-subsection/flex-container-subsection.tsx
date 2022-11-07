import React from 'react'
import { FlexWrap } from 'utopia-api/core'
import { ControlStatus, ControlStyles, getControlStyles } from '../../../common/control-status'
import {
  useInspectorLayoutInfo,
  useMapInspectorInfoFromCSSNumberToNumber,
} from '../../../common/property-path-hooks'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  FlexAlignContentControl,
  FlexAlignItemsControl,
  FlexGapControl,
  FlexWrapControl,
  FlexJustifyContentControl,
  FlexDirectionControl,
  getDirectionAwareLabels,
} from './flex-container-controls'
import { useWrappedEmptyOrUnknownOnSubmitValue } from '../../../../../uuiui'

export const FlexContainerControls = React.memo<{ seeMoreVisible: boolean }>((props) => {
  // Right now flex layout isn't supported on groups, so just don't show the controls if a group is selected
  const flexWrap = useInspectorLayoutInfo('flexWrap')
  const flexDirection = useInspectorLayoutInfo('flexDirection')
  const alignItems = useInspectorLayoutInfo('alignItems')
  const alignContent = useInspectorLayoutInfo('alignContent')
  const justifyContent = useInspectorLayoutInfo('justifyContent')
  const gap = useMapInspectorInfoFromCSSNumberToNumber(useInspectorLayoutInfo('gap'))

  const {
    justifyFlexStart,
    justifyFlexEnd,
    alignDirection,
    alignItemsFlexStart,
    alignItemsFlexEnd,
    alignContentFlexStart,
    alignContentFlexEnd,
  } = getDirectionAwareLabels(flexWrap.value, flexDirection.value)

  const alignItemsControlStatus: ControlStatus =
    flexWrap.value === FlexWrap.NoWrap ? 'disabled' : alignItems.controlStatus
  const alignItemsControlStyles: ControlStyles =
    flexWrap.value === FlexWrap.NoWrap ? getControlStyles('disabled') : alignItems.controlStyles

  const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    gap.onSubmitValue,
    gap.onUnsetValues,
  )
  const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    gap.onSubmitValue,
    gap.onUnsetValues,
  )

  return (
    <>
      <UIGridRow tall padded={true} variant='<---1fr--->|------172px-------|'>
        <FlexDirectionControl
          value={flexDirection.value}
          controlStatus={flexDirection.controlStatus}
          controlStyles={flexDirection.controlStyles}
          onSubmitValue={flexDirection.onSubmitValue}
          onUnset={flexDirection.onUnsetValues}
          flexWrap={flexWrap.value}
        />
        <FlexJustifyContentControl
          value={justifyContent.value}
          onSubmitValue={justifyContent.onSubmitValue}
          onUnset={justifyContent.onUnsetValues}
          controlStatus={justifyContent.controlStatus}
          controlStyles={justifyContent.controlStyles}
          flexDirection={flexDirection.value}
          justifyFlexStart={justifyFlexStart}
          justifyFlexEnd={justifyFlexEnd}
        />
      </UIGridRow>
      <FlexGapControl
        value={gap.value}
        onSubmitValue={wrappedOnSubmitValue}
        onTransientSubmitValue={wrappedOnTransientSubmitValue}
        onUnset={gap.onUnsetValues}
        controlStatus={gap.controlStatus}
        controlStyles={gap.controlStyles}
      />
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
      <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
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
      </UIGridRow>
    </>
  )
})
