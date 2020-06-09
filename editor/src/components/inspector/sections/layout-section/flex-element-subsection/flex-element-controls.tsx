import * as React from 'react'
import { ChainedNumberInput, Icn, useWrappedEmptyOnSubmitValue } from 'uuiui'
import { betterReactMemo } from 'uuiui-deps'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { unsetPropertyMenuItem } from '../../../common/context-menu-items'
import { useInspectorLayoutInfo } from '../../../common/property-path-hooks'
import { SegmentControl } from '../../../controls/segment-control'
import { SelectControl, SelectOption } from '../../../controls/select-control'

export const PositionControl = betterReactMemo('PositionControl', () => {
  const position = useInspectorLayoutInfo('position')

  return (
    <InspectorContextMenuWrapper
      id={`flex-element-position-context-menu`}
      items={[unsetPropertyMenuItem('Position', position.onUnsetValues)]}
      data={{}}
    >
      <SegmentControl
        id='flex.element.position.position'
        value={position.value}
        options={[
          {
            tooltip: 'Absolute',
            value: 'absolute',
            label: (
              <Icn
                category='inspector-position'
                type='absolute'
                width={16}
                height={16}
                color='gray'
              />
            ),
          },
          {
            tooltip: 'Relative',
            value: 'relative',
            label: (
              <Icn
                category='inspector-position'
                type='relative'
                width={16}
                height={16}
                color='gray'
              />
            ),
          },
        ]}
        labelBelow='Position'
        onSubmitValue={position.onSubmitValue}
        controlStatus={position.controlStatus}
        controlStyles={position.controlStyles}
      />
    </InspectorContextMenuWrapper>
  )
})

export const AlignSelfControl = betterReactMemo('AlignSelfControl', () => {
  const alignSelf = useInspectorLayoutInfo('alignSelf')

  return (
    <InspectorContextMenuWrapper
      id={`align-self-context-menu`}
      items={[unsetPropertyMenuItem('Align Self', alignSelf.onUnsetValues)]}
      data={{}}
    >
      <SelectControl
        id='flex.element.alignSelf'
        key='flex.element.alignSelf'
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
    </InspectorContextMenuWrapper>
  )
})

export const MarginControl = betterReactMemo('MarginControl', () => {
  const marginTop = useInspectorLayoutInfo('marginTop')
  const marginRight = useInspectorLayoutInfo('marginRight')
  const marginBottom = useInspectorLayoutInfo('marginBottom')
  const marginLeft = useInspectorLayoutInfo('marginLeft')

  const wrappedMarginTopOnSubmitValue = useWrappedEmptyOnSubmitValue(
    marginTop.onSubmitValue,
    marginTop.onUnsetValues,
  )
  const wrappedMarginTopOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    marginTop.onSubmitValue,
    marginTop.onUnsetValues,
  )
  const wrappedMarginRightOnSubmitValue = useWrappedEmptyOnSubmitValue(
    marginRight.onSubmitValue,
    marginRight.onUnsetValues,
  )
  const wrappedMarginRightOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    marginRight.onSubmitValue,
    marginRight.onUnsetValues,
  )
  const wrappedMarginBottomOnSubmitValue = useWrappedEmptyOnSubmitValue(
    marginBottom.onSubmitValue,
    marginBottom.onUnsetValues,
  )
  const wrappedMarginBottomOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    marginBottom.onSubmitValue,
    marginBottom.onUnsetValues,
  )
  const wrappedMarginLeftOnSubmitValue = useWrappedEmptyOnSubmitValue(
    marginLeft.onSubmitValue,
    marginLeft.onUnsetValues,
  )
  const wrappedMarginLeftOnTransientSubmitValue = useWrappedEmptyOnSubmitValue(
    marginLeft.onSubmitValue,
    marginLeft.onUnsetValues,
  )

  return (
    <ChainedNumberInput
      idPrefix={'margin'}
      propsArray={[
        {
          value: marginTop.value,
          labelBelow: 'T',
          controlStatus: marginTop.controlStatus,
          disabled: !marginTop.controlStyles.interactive,
          onSubmitValue: wrappedMarginTopOnSubmitValue,
          onTransientSubmitValue: wrappedMarginTopOnTransientSubmitValue,
          numberType: 'UnitlessPercent',
        },
        {
          value: marginRight.value,
          labelBelow: 'R',
          controlStatus: marginRight.controlStatus,
          disabled: !marginRight.controlStyles.interactive,
          onSubmitValue: wrappedMarginRightOnSubmitValue,
          onTransientSubmitValue: wrappedMarginRightOnTransientSubmitValue,
          numberType: 'UnitlessPercent',
        },
        {
          value: marginBottom.value,
          labelBelow: 'B',
          controlStatus: marginBottom.controlStatus,
          disabled: !marginBottom.controlStyles.interactive,
          onSubmitValue: wrappedMarginBottomOnSubmitValue,
          onTransientSubmitValue: wrappedMarginBottomOnTransientSubmitValue,
          numberType: 'UnitlessPercent',
        },
        {
          value: marginLeft.value,
          labelBelow: 'L',
          controlStatus: marginLeft.controlStatus,
          disabled: !marginLeft.controlStyles.interactive,
          onSubmitValue: wrappedMarginLeftOnSubmitValue,
          onTransientSubmitValue: wrappedMarginLeftOnTransientSubmitValue,
          numberType: 'UnitlessPercent',
        },
      ]}
    />
  )
})
