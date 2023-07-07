import React from 'react'
import type { FlexAlignment, FlexJustifyContent } from 'utopia-api/core'
import { FlexWrap } from 'utopia-api/core'
import type { ControlStatus, ControlStyles } from '../../../common/control-status'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import type { OptionChainOption } from '../../../controls/option-chain-control'
import { OptionChainControl } from '../../../controls/option-chain-control'
import type { DEPRECATEDSliderControlOptions } from '../../../controls/slider-control'
import { SliderControl } from '../../../controls/slider-control'
import {
  InspectorPropsContext,
  stylePropPathMappingFn,
  useInspectorLayoutInfo,
} from '../../../common/property-path-hooks'
import type { SelectOption } from '../../../controls/select-control'
import type { OptionsType } from 'react-select'
import { unsetPropertyMenuItem } from '../../../common/context-menu-items'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { PropertyLabel } from '../../../widgets/property-label'
import {
  PopupList,
  useWrappedEmptyOrUnknownOnSubmitValue,
  SimpleNumberInput,
} from '../../../../../uuiui'
import { OnSubmitValueOrEmpty } from '../../../controls/control'
import { useContextSelector } from 'use-context-selector'
import type { FlexDirection } from '../../../common/css-utils'
import { CSSNumber, setCSSNumberValue } from '../../../common/css-utils'
import { SliderNumberControl } from '../../../controls/slider-number-control'

type uglyLabel =
  | 'left'
  | 'center'
  | 'right'
  | 'bottom'
  | 'top'
  | 'spaceBetween'
  | 'spaceAround'
  | 'spaceEvenly'
  | 'horizontal'
  | 'vertical'
type prettyLabel =
  | 'Left'
  | 'Center'
  | 'Right'
  | 'Bottom'
  | 'Top'
  | 'Space Between'
  | 'Space Around'
  | 'Space Evenly'
  | 'Horizontal'
  | 'Vertical'

const PrettyLabel: { [K in uglyLabel]: prettyLabel } = {
  left: 'Left',
  center: 'Center',
  right: 'Right',
  bottom: 'Bottom',
  top: 'Top',
  spaceBetween: 'Space Between',
  spaceAround: 'Space Around',
  spaceEvenly: 'Space Evenly',
  horizontal: 'Horizontal',
  vertical: 'Vertical',
}

interface FlexFieldControlProps<T> {
  value: T
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  onSubmitValue: (newValue: T) => void
  onUnset: () => void
}

interface FlexDirectionControlProps extends FlexFieldControlProps<FlexDirection> {
  flexWrap: FlexWrap
}

export const FlexDirectionControl = React.memo((props: FlexDirectionControlProps) => {
  return (
    <InspectorContextMenuWrapper
      id={`flexDirection-context-menu`}
      items={[unsetPropertyMenuItem('Flex Direction', props.onUnset)]}
      data={{}}
      style={{
        display: 'flex',
        flexDirection: 'column',
      }}
    >
      <OptionChainControl
        id='flex.container.flexDirection'
        key='flex.container.flexDirection'
        testId='flex.container.flexDirection'
        value={props.value}
        DEPRECATED_controlOptions={{
          labelBelow: 'Direction',
        }}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
        options={flexDirectionOptions(props.flexWrap)}
        onSubmitValue={props.onSubmitValue}
      />
    </InspectorContextMenuWrapper>
  )
})

interface FlexAlignItemsControlProps extends FlexFieldControlProps<FlexAlignment> {
  alignDirection: uglyLabel
  alignItemsFlexStart: uglyLabel
  alignItemsFlexEnd: uglyLabel
}

export const FlexAlignItemsControl = React.memo((props: FlexAlignItemsControlProps) => {
  const targetPath = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  const alignItemsProp = React.useMemo(() => {
    return [stylePropPathMappingFn('alignItems', targetPath)]
  }, [targetPath])
  return (
    <InspectorContextMenuWrapper
      id={`alignItems-context-menu`}
      items={[unsetPropertyMenuItem('Align Items', props.onUnset)]}
      data={{}}
    >
      <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel target={alignItemsProp}>Align</PropertyLabel>
        <div
          style={{
            textAlign: 'center',
            display: 'flex',
            flexDirection: 'row',
          }}
        >
          <OptionChainControl
            id='flex.container.alignItems'
            key='flex.container.alignItems'
            testId='flex.container.alignItems'
            value={props.value}
            controlStatus={props.controlStatus}
            controlStyles={props.controlStyles}
            options={alignItemsOptions(
              props.alignDirection,
              props.alignItemsFlexStart,
              props.alignItemsFlexEnd,
            )}
            onSubmitValue={props.onSubmitValue}
          />
        </div>
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

interface FlexWrapControlProps extends FlexFieldControlProps<FlexWrap> {}

const FlexWrapOptions: OptionsType<SelectOption> = [
  {
    value: FlexWrap.NoWrap,
    label: 'No Wrap',
  },
  {
    value: FlexWrap.Wrap,
    label: 'Wrap',
  },
  {
    value: FlexWrap.WrapReverse,
    label: 'Wrap Reverse',
  },
]

export const FlexWrapControl = React.memo((props: FlexWrapControlProps) => {
  const { onSubmitValue: onSubmit } = props
  const onSubmitValue = React.useCallback(
    (newValue: SelectOption) => {
      onSubmit(newValue.value)
    },
    [onSubmit],
  )

  return (
    <InspectorContextMenuWrapper
      id={`flexWrap-context-menu`}
      items={[unsetPropertyMenuItem('Flex Wrap', props.onUnset)]}
      data={{}}
      style={{
        display: 'flex',
        overflow: 'hidden',
        width: undefined,
        marginLeft: -8, // this is Balazs hacking the UI so the text of the dropdown aligns with the rest of the rows
      }}
    >
      <PopupList
        value={FlexWrapOptions.find((option) => option.value === props.value)}
        options={FlexWrapOptions}
        onSubmitValue={onSubmitValue}
        controlStyles={props.controlStyles}
      />
    </InspectorContextMenuWrapper>
  )
})

interface FlexJustifyContentControlProps extends FlexFieldControlProps<FlexJustifyContent> {
  flexDirection: FlexDirection
  justifyFlexStart: uglyLabel
  justifyFlexEnd: uglyLabel
}

export const FlexJustifyContentControl = React.memo((props: FlexJustifyContentControlProps) => {
  return (
    <InspectorContextMenuWrapper
      id={`justifyContent-context-menu`}
      items={[unsetPropertyMenuItem('Justify Content', props.onUnset)]}
      data={{}}
      style={{
        display: 'flex',
        flexDirection: 'column',
      }}
    >
      <OptionChainControl
        id='flex.container.justifyContent'
        key='flex.container.justifyContent'
        testId='flex.container.justifyContent'
        value={props.value}
        DEPRECATED_controlOptions={{
          labelBelow: 'Justify',
        }}
        options={justifyContentOptions(
          props.flexDirection,
          props.justifyFlexStart,
          props.justifyFlexEnd,
        )}
        onSubmitValue={props.onSubmitValue}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
      />
    </InspectorContextMenuWrapper>
  )
})

export const FlexGapControl = React.memo(() => {
  const {
    value,
    useSubmitValueFactory,
    onSubmitValue,
    onUnsetValues,
    onTransientSubmitValue,
    controlStatus,
    controlStyles,
  } = useInspectorLayoutInfo('gap')
  const menuItems = [unsetPropertyMenuItem('Flex Gap', onUnsetValues)]

  const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(onSubmitValue, onUnsetValues)
  const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    onTransientSubmitValue,
    onUnsetValues,
  )

  const transformNumberToCSSNumber = React.useCallback(
    (newValue: number) => setCSSNumberValue(value, newValue),
    [value],
  )
  const [sliderSubmitValue, sliderTransientSubmitValue] = useSubmitValueFactory(
    transformNumberToCSSNumber,
  )

  const targetPath = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  const flexGapProp = React.useMemo(() => {
    return [stylePropPathMappingFn('gap', targetPath)]
  }, [targetPath])
  return (
    <InspectorContextMenuWrapper id={`gap-context-menu`} items={menuItems} data={{}}>
      <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel target={flexGapProp}>Gap</PropertyLabel>
        <SliderNumberControl
          id='flex.container.gap'
          key='flex.container.gap'
          testId='flex.container.gap'
          value={value}
          DEPRECATED_controlOptions={
            {
              minimum: 0,
              maximum: 50,
              stepSize: 1,
              origin: 0,
              filled: true,
              tooltip: 'Gap (sets margin on children)',
            } as DEPRECATEDSliderControlOptions
          }
          controlStatus={controlStatus}
          controlStyles={controlStyles}
          minimum={0}
          maximum={50}
          stepSize={1}
          defaultUnitToHide={'px'}
          numberType='LengthPercent'
          onSubmitValue={wrappedOnSubmitValue}
          onTransientSubmitValue={wrappedOnTransientSubmitValue}
          onForcedSubmitValue={wrappedOnSubmitValue}
          onSliderSubmitValue={sliderSubmitValue}
          onSliderTransientSubmitValue={sliderTransientSubmitValue}
          transformSliderValueToCSSNumber={transformNumberToCSSNumber}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

interface FlexAlignContentControlProps extends FlexFieldControlProps<FlexAlignment> {
  alignDirection: uglyLabel
  alignContentFlexStart: uglyLabel
  alignContentFlexEnd: uglyLabel
}

export const FlexAlignContentControl = React.memo((props: FlexAlignContentControlProps) => {
  return (
    <InspectorContextMenuWrapper
      id={`alignContent-context-menu`}
      items={[unsetPropertyMenuItem('Align Content', props.onUnset)]}
      data={{}}
      style={{
        display: 'flex',
        flexDirection: 'column',
        textAlign: 'center',
      }}
    >
      <OptionChainControl
        id='flex.container.alignContent'
        key='flex.container.alignContent'
        testId='flex.container.alignContent'
        value={props.value}
        options={alignContentOptions(
          props.alignDirection,
          props.alignContentFlexStart,
          props.alignContentFlexEnd,
        )}
        onSubmitValue={props.onSubmitValue}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
      />
    </InspectorContextMenuWrapper>
  )
})

const alignContentOptions = (
  alignDirection: string,
  alignContentFlexStart: uglyLabel,
  alignContentFlexEnd: uglyLabel,
) =>
  [
    {
      value: 'flex-start',
      tooltip: PrettyLabel[alignContentFlexStart],
      icon: {
        category: `layout/flex`,
        type: `alignContent-${alignDirection}-${alignContentFlexStart}`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'center',
      tooltip: 'Center',
      icon: {
        category: `layout/flex`,
        type: `alignContent-${alignDirection}-center`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'flex-end',
      tooltip: PrettyLabel[alignContentFlexEnd],
      icon: {
        category: `layout/flex`,
        type: `alignContent-${alignDirection}-${alignContentFlexEnd}`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'stretch',
      tooltip: 'Stretch',
      icon: {
        category: `layout/flex`,
        type: `alignContent-${alignDirection}-stretch`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
  ] as Array<OptionChainOption<string | number>>

const justifyContentOptions = (
  alignDirection: FlexDirection,
  justifyFlexStart: uglyLabel,
  justifyFlexEnd: uglyLabel,
) =>
  [
    {
      value: 'flex-start',
      tooltip: PrettyLabel[justifyFlexStart],
      icon: {
        category: `layout/flex`,
        type: `justifyContent-${alignDirection}-${justifyFlexStart}`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'center',
      tooltip: 'Center',
      icon: {
        category: `layout/flex`,
        type: `justifyContent-${alignDirection}-center`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'flex-end',
      tooltip: PrettyLabel[justifyFlexEnd],
      icon: {
        category: `layout/flex`,
        type: `justifyContent-${alignDirection}-${justifyFlexEnd}`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'space-between',
      tooltip: 'Space Between',
      icon: {
        category: `layout/flex`,
        type: `justifyContent-${alignDirection}-spaceBetween`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'space-around',
      tooltip: 'Space Around',
      icon: {
        category: `layout/flex`,
        type: `justifyContent-${alignDirection}-spaceAround`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
  ] as Array<OptionChainOption<string | number>>

export const alignItemsOptions = (
  alignDirection: string,
  alignItemsFlexStart: uglyLabel,
  alignItemsFlexEnd: uglyLabel,
) =>
  [
    {
      value: 'flex-start',
      tooltip: PrettyLabel[alignItemsFlexStart],
      icon: {
        category: `layout/flex`,
        type: `alignItems-${alignDirection}-${alignItemsFlexStart}`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'center',
      tooltip: 'Center',
      icon: {
        category: `layout/flex`,
        type: `alignItems-${alignDirection}-center`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'flex-end',
      tooltip: PrettyLabel[alignItemsFlexEnd],
      icon: {
        category: `layout/flex`,
        type: `alignItems-${alignDirection}-${alignItemsFlexEnd}`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'stretch',
      tooltip: 'Stretch',
      icon: {
        category: `layout/flex`,
        type: `alignItems-${alignDirection}-stretch`,
        color: 'secondary',
        width: 16,
        height: 16,
      },
    },
  ] as Array<OptionChainOption<string | number>>

export const flexDirectionOptions = (flexWrap: FlexWrap) => {
  const flexDirectionWrap = flexWrap === 'wrap' || flexWrap === 'wrap-reverse' ? 'wrap' : 'nowrap'
  const flexWrapReverse = flexWrap === 'wrap-reverse' ? 'reverse' : 'regular'

  return [
    {
      value: 'row',
      tooltip: 'Row',
      icon: {
        category: 'layout/flex',
        type: `flexDirection-row-${flexWrapReverse}-${flexDirectionWrap}`,
        color: 'main',
        width: 16,
        height: 16,
      },
    },
    {
      value: 'column',
      tooltip: 'Column',
      icon: {
        category: 'layout/flex',
        type: `flexDirection-column-${flexWrapReverse}-${flexDirectionWrap}`,
        color: 'main',
        width: 16,
        height: 16,
      },
    },
  ] as Array<OptionChainOption<string | number>>
}

export function getDirectionAwareLabels(
  flexWrap: FlexWrap,
  flexDirection: FlexDirection,
): { [K in string]: uglyLabel } {
  type NonConditionals = {
    justifyFlexStart: uglyLabel
    justifyFlexEnd: uglyLabel
    alignDirection: uglyLabel
  }

  switch (flexDirection) {
    case 'row-reverse': {
      const nonConditionals: NonConditionals = {
        justifyFlexStart: 'right',
        justifyFlexEnd: 'left',
        alignDirection: 'horizontal',
      }
      if (flexWrap === 'wrap-reverse') {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'bottom',
          alignContentFlexEnd: 'top',
          alignItemsFlexStart: 'bottom',
          alignItemsFlexEnd: 'top',
        }
      } else {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'top',
          alignContentFlexEnd: 'bottom',
          alignItemsFlexStart: 'top',
          alignItemsFlexEnd: 'bottom',
        }
      }
    }
    case 'column': {
      const nonConditionals: NonConditionals = {
        justifyFlexStart: 'top',
        justifyFlexEnd: 'bottom',
        alignDirection: 'horizontal',
      }
      if (flexWrap === 'wrap-reverse') {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'right',
          alignContentFlexEnd: 'left',
          alignItemsFlexStart: 'right',
          alignItemsFlexEnd: 'left',
        }
      } else {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'left',
          alignContentFlexEnd: 'right',
          alignItemsFlexStart: 'left',
          alignItemsFlexEnd: 'right',
        }
      }
    }
    case 'column-reverse': {
      const nonConditionals: NonConditionals = {
        justifyFlexStart: 'bottom',
        justifyFlexEnd: 'top',
        alignDirection: 'horizontal',
      }
      if (flexWrap === 'wrap-reverse') {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'right',
          alignContentFlexEnd: 'left',
          alignItemsFlexStart: 'right',
          alignItemsFlexEnd: 'left',
        }
      } else {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'left',
          alignContentFlexEnd: 'right',
          alignItemsFlexStart: 'left',
          alignItemsFlexEnd: 'right',
        }
      }
    }
    case 'row':
    default: {
      const nonConditionals: NonConditionals = {
        justifyFlexStart: 'left',
        justifyFlexEnd: 'right',
        alignDirection: 'vertical',
      }
      if (flexWrap === 'wrap-reverse') {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'bottom',
          alignContentFlexEnd: 'top',
          alignItemsFlexStart: 'bottom',
          alignItemsFlexEnd: 'top',
        }
      } else {
        return {
          ...nonConditionals,
          alignContentFlexStart: 'top',
          alignContentFlexEnd: 'bottom',
          alignItemsFlexStart: 'top',
          alignItemsFlexEnd: 'bottom',
        }
      }
    }
  }
}
