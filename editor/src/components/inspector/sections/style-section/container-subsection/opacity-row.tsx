import React from 'react'
import * as PP from '../../../../../core/shared/property-path'
import { PropertyLabel } from '../../../widgets/property-label'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { useInspectorStyleInfo, useIsSubSectionVisible } from '../../../common/property-path-hooks'
import { useWrappedEmptyOrUnknownOnSubmitValue, SimplePercentInput } from '../../../../../uuiui'
import { InspectorContextMenuItems, InspectorContextMenuWrapper } from '../../../../../uuiui-deps'
import { setCSSNumberValue } from '../../../common/css-utils'

const opacityProp = [PP.create('style', 'opacity')]

export const OpacityRow = React.memo(() => {
  const opacityMetadata = useInspectorStyleInfo('opacity')

  const opacity = opacityMetadata.value
  const scale = opacity.unit === '%' ? 100 : 1

  const isVisible = useIsSubSectionVisible('opacity')

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
      <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
        <SimplePercentInput
          id='opacity-input'
          testId='opacity-input'
          value={opacity?.value ?? 0}
          onSubmitValue={wrappedOnSubmitValue}
          onTransientSubmitValue={wrappedOnTransientSubmitValue}
          onForcedSubmitValue={wrappedOnSubmitValue}
          controlStatus={opacityMetadata.controlStatus}
          DEPRECATED_labelBelow={<span style={{ fontSize: 12 }}>α</span>}
          minimum={0}
          maximum={1}
          stepSize={0.01}
          inputProps={{ onMouseDown: (e) => e.stopPropagation() }}
          defaultUnitToHide={null}
          incrementControls={false}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})
OpacityRow.displayName = 'OpacityRow'
