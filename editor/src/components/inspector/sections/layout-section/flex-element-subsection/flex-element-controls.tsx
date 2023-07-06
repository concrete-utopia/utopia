import React from 'react'

import type { SelectOption } from '../../../controls/select-control'
import { SelectControl } from '../../../controls/select-control'
import type { OptionChainOption } from '../../../controls/option-chain-control'
import { OptionChainControl } from '../../../controls/option-chain-control'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { addSetProperty, unsetPropertyMenuItem } from '../../../common/context-menu-items'
import {
  InspectorPropsContext,
  stylePropPathMappingFn,
  useInspectorLayoutInfo,
} from '../../../common/property-path-hooks'
import { useWrappedEmptyOrUnknownOnSubmitValue, ChainedNumberInput } from '../../../../../uuiui'
import { useInspectorInfoLonghandShorthand } from '../../../common/longhand-shorthand-hooks'
import type { GridRowProps } from '../../../widgets/ui-grid-row'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { PropertyLabel } from '../../../widgets/property-label'
import { useContextSelector } from 'use-context-selector'

export const PositionControl = React.memo(() => {
  const position = useInspectorLayoutInfo('position')

  return (
    <InspectorContextMenuWrapper
      id={`flex-element-position-context-menu`}
      items={[unsetPropertyMenuItem('Position', position.onUnsetValues)]}
      data={{}}
    >
      <OptionChainControl
        id='flex.element.position.position'
        key='flex.element.position.position'
        testId='flex.element.position.position'
        value={position.value}
        options={
          [
            {
              tooltip: 'Absolute',
              value: 'absolute',
              icon: {
                category: 'inspector-position',
                type: 'absolute',
                width: 16,
                height: 16,
                color: 'secondary',
              },
            },
            {
              tooltip: 'Relative',
              value: 'relative',
              icon: {
                category: 'inspector-position',
                type: 'relative',
                width: 16,
                height: 16,
                color: 'secondary',
              },
            },
          ] as Array<OptionChainOption<string | number>>
        }
        DEPRECATED_controlOptions={{
          labelBelow: 'Position',
        }}
        onSubmitValue={position.onSubmitValue}
        controlStatus={position.controlStatus}
        controlStyles={position.controlStyles}
      />
    </InspectorContextMenuWrapper>
  )
})

interface AlignSelfControlProps {
  variant: GridRowProps['variant']
}

export const AlignSelfControl = React.memo((props: AlignSelfControlProps) => {
  const alignSelf = useInspectorLayoutInfo('alignSelf')
  const targetPath = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  const alignSelfProp = React.useMemo(() => {
    return [stylePropPathMappingFn('alignSelf', targetPath)]
  }, [targetPath])

  return (
    <InspectorContextMenuWrapper
      id={`align-self-context-menu`}
      items={[unsetPropertyMenuItem('Align Self', alignSelf.onUnsetValues)]}
      data={{}}
    >
      <UIGridRow padded={true} variant={props.variant}>
        <PropertyLabel target={alignSelfProp}>Align Self</PropertyLabel>
        <SelectControl
          id='flex.element.alignSelf'
          key='flex.element.alignSelf'
          testId='flex.element.alignSelf'
          options={
            [
              {
                value: 'auto',
                label: 'Auto',
              },
              {
                value: 'flex-start',
                label: 'Flex Start',
              },
              {
                value: 'center',
                label: 'Center',
              },
              {
                value: 'flex-end',
                label: 'Flex End',
              },
              {
                value: 'stretch',
                label: 'Stretch',
              },
            ] as Array<SelectOption>
          }
          value={alignSelf.value}
          onSubmitValue={alignSelf.onSubmitValue}
          controlStatus={alignSelf.controlStatus}
          controlStyles={alignSelf.controlStyles}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

export const MarginControl = React.memo(() => {
  const { marginTop, marginRight, marginBottom, marginLeft } = useInspectorInfoLonghandShorthand(
    ['marginTop', 'marginRight', 'marginBottom', 'marginLeft'],
    'margin',
    stylePropPathMappingFn,
  )

  const wrappedMarginTopOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    marginTop.onSubmitValue,
    marginTop.onUnsetValues,
  )
  const wrappedMarginTopOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    marginTop.onSubmitValue,
    marginTop.onUnsetValues,
  )
  const wrappedMarginRightOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    marginRight.onSubmitValue,
    marginRight.onUnsetValues,
  )
  const wrappedMarginRightOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    marginRight.onSubmitValue,
    marginRight.onUnsetValues,
  )
  const wrappedMarginBottomOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    marginBottom.onSubmitValue,
    marginBottom.onUnsetValues,
  )
  const wrappedMarginBottomOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    marginBottom.onSubmitValue,
    marginBottom.onUnsetValues,
  )
  const wrappedMarginLeftOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    marginLeft.onSubmitValue,
    marginLeft.onUnsetValues,
  )
  const wrappedMarginLeftOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    marginLeft.onSubmitValue,
    marginLeft.onUnsetValues,
  )

  return (
    <ChainedNumberInput
      idPrefix={'margin'}
      propsArray={[
        {
          value: marginTop.value,
          DEPRECATED_labelBelow: 'T',
          controlStatus: marginTop.controlStatus,
          onSubmitValue: wrappedMarginTopOnSubmitValue,
          onTransientSubmitValue: wrappedMarginTopOnTransientSubmitValue,
          numberType: 'LengthPercent',
          testId: 'margin-T',
          defaultUnitToHide: 'px',
        },
        {
          value: marginRight.value,
          DEPRECATED_labelBelow: 'R',
          controlStatus: marginRight.controlStatus,
          onSubmitValue: wrappedMarginRightOnSubmitValue,
          onTransientSubmitValue: wrappedMarginRightOnTransientSubmitValue,
          numberType: 'LengthPercent',
          testId: 'margin-R',
          defaultUnitToHide: 'px',
        },
        {
          value: marginBottom.value,
          DEPRECATED_labelBelow: 'B',
          controlStatus: marginBottom.controlStatus,
          onSubmitValue: wrappedMarginBottomOnSubmitValue,
          onTransientSubmitValue: wrappedMarginBottomOnTransientSubmitValue,
          numberType: 'LengthPercent',
          testId: 'margin-B',
          defaultUnitToHide: 'px',
        },
        {
          value: marginLeft.value,
          DEPRECATED_labelBelow: 'L',
          controlStatus: marginLeft.controlStatus,
          onSubmitValue: wrappedMarginLeftOnSubmitValue,
          onTransientSubmitValue: wrappedMarginLeftOnTransientSubmitValue,
          numberType: 'LengthPercent',
          testId: 'margin-L',
          defaultUnitToHide: 'px',
        },
      ]}
    />
  )
})
