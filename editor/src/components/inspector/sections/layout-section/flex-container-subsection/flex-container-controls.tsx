import * as React from 'react'
import { FlexAlignment, FlexJustifyContent, FlexWrap } from 'utopia-api'
import {
  PopupList,
  SimpleNumberInput,
  ChainedNumberInput,
  useWrappedEmptyOnSubmitValue,
} from 'uuiui'
import { ControlStatus, ControlStyles, getControlStyles } from '../../../widgets/control-status'
import { FlexDirection } from 'utopia-api'
import { NewInspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { OptionChainControl, OptionChainOption } from '../../../controls/option-chain-control'
import { SliderControl, SliderControlOptions } from '../../../controls/slider-control'
import {
  useInspectorLayoutInfo,
  useInspectorStyleInfo,
} from '../../../new-inspector/new-inspector-hooks'
import { betterReactMemo, OnSubmitValueOrEmpty } from 'uuiui-deps'
import { SelectOption } from '../../../controls/select-control'
import { OptionsType } from 'react-select'
import { unsetPropertyMenuItem } from '../../../new-inspector/context-menu-items'
import { GridRow } from '../../../widgets/grid-row'
import { PropertyLabel } from '../../../widgets/property-label'
import { createLayoutPropertyPath } from '../../../../../core/layout/layout-helpers-new'

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

export const FlexDirectionControl = betterReactMemo(
  'FlexDirectionControl',
  (props: FlexDirectionControlProps) => {
    return (
      <NewInspectorContextMenuWrapper
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
          value={props.value}
          controlOptions={{
            labelBelow: 'Direction',
          }}
          controlStatus={props.controlStatus}
          controlStyles={props.controlStyles}
          options={flexDirectionOptions(props.flexWrap)}
          onSubmitValue={props.onSubmitValue}
        />
      </NewInspectorContextMenuWrapper>
    )
  },
)

interface FlexAlignItemsControlProps extends FlexFieldControlProps<FlexAlignment> {
  alignDirection: uglyLabel
  alignItemsFlexStart: uglyLabel
  alignItemsFlexEnd: uglyLabel
}

export const FlexAlignItemsControl = betterReactMemo(
  'FlexAlignItemsControl',
  (props: FlexAlignItemsControlProps) => {
    return (
      <div
        style={{
          textAlign: 'center',
          display: 'flex',
          flexDirection: 'row',
        }}
      >
        <NewInspectorContextMenuWrapper
          id={`alignItems-context-menu`}
          items={[unsetPropertyMenuItem('Align Items', props.onUnset)]}
          data={{}}
        >
          <OptionChainControl
            id='flex.container.alignItems'
            key='flex.container.alignItems'
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
        </NewInspectorContextMenuWrapper>
      </div>
    )
  },
)

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

export const FlexWrapControl = betterReactMemo('FlexWrapControl', (props: FlexWrapControlProps) => {
  const { onSubmitValue: onSubmit } = props
  const onSubmitValue = React.useCallback(
    (newValue: SelectOption) => {
      onSubmit(newValue.value)
    },
    [onSubmit],
  )

  return (
    <NewInspectorContextMenuWrapper
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
        containerMode='showBorderOnHover'
      />
    </NewInspectorContextMenuWrapper>
  )
})

interface FlexJustifyContentControlProps extends FlexFieldControlProps<FlexJustifyContent> {
  flexDirection: FlexDirection
  justifyFlexStart: uglyLabel
  justifyFlexEnd: uglyLabel
}

export const FlexJustifyContentControl = betterReactMemo(
  'FlexJustifyContentControl',
  (props: FlexJustifyContentControlProps) => {
    return (
      <NewInspectorContextMenuWrapper
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
          value={props.value}
          controlOptions={{
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
      </NewInspectorContextMenuWrapper>
    )
  },
)

interface FlexGapControlProps extends FlexFieldControlProps<number> {
  onSubmitValue: OnSubmitValueOrEmpty<number>
  onTransientSubmitValue: OnSubmitValueOrEmpty<number>
}

export const FlexGapControl = betterReactMemo('FlexGapControl', (props: FlexGapControlProps) => {
  const menuItems = [unsetPropertyMenuItem('Flex Gap', props.onUnset)]
  const wrappedOnSubmit = useWrappedEmptyOnSubmitValue(props.onSubmitValue, props.onUnset)
  const wrappedOnTransientSubmit = useWrappedEmptyOnSubmitValue(
    props.onTransientSubmitValue,
    props.onUnset,
  )
  return (
    <>
      <GridRow padded={false} type='<--------auto-------->|--45px--|'>
        <NewInspectorContextMenuWrapper id={`gap-context-menu`} items={menuItems} data={{}}>
          <SliderControl
            id='flex.container.gap.main'
            key='flex.container.gap.main'
            value={props.value}
            controlOptions={
              {
                minimum: 0,
                maximum: 50,
                stepSize: 1,
                origin: 0,
                filled: true,
                tooltip: 'Gap (sets margin on children)',
              } as SliderControlOptions
            }
            onSubmitValue={props.onSubmitValue}
            controlStatus={props.controlStatus}
            controlStyles={props.controlStyles}
          />
        </NewInspectorContextMenuWrapper>
        <NewInspectorContextMenuWrapper id={`gap-main-context-menu`} items={menuItems} data={{}}>
          <SimpleNumberInput
            id='flex.container.gap.main'
            key='flex.container.gap.main'
            value={props.value}
            minimum={0}
            maximum={50}
            stepSize={1}
            onSubmitValue={wrappedOnSubmit}
            onTransientSubmitValue={wrappedOnTransientSubmit}
            controlStatus={props.controlStatus}
            disabled={!props.controlStyles.interactive}
          />
        </NewInspectorContextMenuWrapper>
      </GridRow>
    </>
  )
})

interface FlexAlignContentControlProps extends FlexFieldControlProps<FlexAlignment> {
  alignDirection: uglyLabel
  alignContentFlexStart: uglyLabel
  alignContentFlexEnd: uglyLabel
}

export const FlexAlignContentControl = betterReactMemo(
  'FlexAlignContentControl',
  (props: FlexAlignContentControlProps) => {
    return (
      <NewInspectorContextMenuWrapper
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
      </NewInspectorContextMenuWrapper>
    )
  },
)

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
        color: 'darkgray',
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
        color: 'darkgray',
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
        color: 'darkgray',
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
        color: 'darkgray',
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
        color: 'darkgray',
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
        color: 'darkgray',
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
        color: 'darkgray',
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
        color: 'darkgray',
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
        color: 'darkgray',
        width: 16,
        height: 16,
      },
    },
  ] as Array<OptionChainOption<string | number>>

const alignItemsOptions = (
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
        color: 'darkgray',
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
        color: 'darkgray',
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
        color: 'darkgray',
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
        color: 'darkgray',
        width: 16,
        height: 16,
      },
    },
  ] as Array<OptionChainOption<string | number>>

const flexDirectionOptions = (flexWrap: FlexWrap) => {
  const flexDirectionWrap = flexWrap === 'wrap' || flexWrap === 'wrap-reverse' ? 'wrap' : 'nowrap'
  const flexWrapReverse = flexWrap === 'wrap-reverse' ? 'reverse' : 'regular'

  return [
    {
      value: 'row',
      tooltip: 'Row',
      icon: {
        category: 'layout/flex',
        type: `flexDirection-row-${flexWrapReverse}-${flexDirectionWrap}`,
        color: 'darkgray',
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
        color: 'darkgray',
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
