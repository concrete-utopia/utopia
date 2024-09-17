import React, { useCallback } from 'react'
import * as PP from '../../../../../core/shared/property-path'
import { useInspectorStyleInfo, useIsSubSectionVisible } from '../../../common/property-path-hooks'
import { useWrappedEmptyOrUnknownOnSubmitValue, SimplePercentInput } from '../../../../../uuiui'
import { InspectorContextMenuItems, InspectorContextMenuWrapper } from '../../../../../uuiui-deps'
import { setCSSNumberValue } from '../../../common/css-utils'
import { PropertyLabel } from '../../../widgets/property-label'

const opacityProp = [PP.create('style', 'opacity')]

export const OpacityRow = React.memo(() => {
  const opacityMetadata = useInspectorStyleInfo('opacity')

  const opacity = opacityMetadata.value
  const scale = opacity.unit === '%' ? 100 : 1

  const isVisible = useIsSubSectionVisible('opacity')

  const handleMouseDown = useCallback((e: React.MouseEvent) => {
    e.stopPropagation()
  }, [])

  const opacityContextMenuItems = InspectorContextMenuItems.optionalAddOnUnsetValues(
    opacity != null,
    ['opacity'],
    opacityMetadata.onUnsetValues,
  )

  const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    (value: number) => opacityMetadata.onSubmitValue(setCSSNumberValue(opacity, value * scale)),
    opacityMetadata.onUnsetValues,
  )
  const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    (value: number) =>
      opacityMetadata.onTransientSubmitValue(setCSSNumberValue(opacity, value * scale)),
    opacityMetadata.onUnsetValues,
  )

  if (!isVisible) {
    return null
  }

  return (
    <InspectorContextMenuWrapper
      id='opacity-row-context-menu'
      items={opacityContextMenuItems}
      data={null}
    >
      <SimplePercentInput
        id='opacity'
        key='opacity'
        testId='opacity'
        value={opacity?.value ?? 0}
        onSubmitValue={wrappedOnSubmitValue}
        onTransientSubmitValue={wrappedOnTransientSubmitValue}
        onForcedSubmitValue={wrappedOnSubmitValue}
        controlStatus={opacityMetadata.controlStatus}
        innerLabel={<span style={{ fontSize: 12 }}>α</span>}
        minimum={0}
        maximum={1}
        stepSize={0.01}
        inputProps={{ onMouseDown: handleMouseDown }}
        defaultUnitToHide={null}
      />
    </InspectorContextMenuWrapper>
  )
})
OpacityRow.displayName = 'OpacityRow'
