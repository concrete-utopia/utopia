import React from 'react'
import type { FlexAlignment, FlexJustifyContent } from 'utopia-api/core'
import { FlexWrap } from 'utopia-api/core'
import type { ControlStatus } from '../../../common/control-status'
import type { ControlStyles } from '../../../common/control-styles'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import type { OptionChainOption } from '../../../controls/option-chain-control'
import { OptionChainControl } from '../../../controls/option-chain-control'
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
  NumberInput,
  Icons,
} from '../../../../../uuiui'
import { useContextSelector } from 'use-context-selector'
import type { FlexDirection } from '../../../common/css-utils'
import { when } from '../../../../../utils/react-conditionals'
import { Substores, useEditorState } from '../../../../editor/store/store-hook'
import { flexDirectionSelector } from '../../../inpector-selectors'
import { FlexGapControlTestId } from '../../../../../components/canvas/controls/select-mode/flex-gap-control'
import { useOnControlHover } from '../../../../../components/canvas/controls/select-mode/controls-common'

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
  const flexDirection = useEditorState(
    Substores.metadata,
    flexDirectionSelector,
    'FlexWrapControl flexDirection',
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
      }}
    >
      <UIGridRow padded={false} variant='<-auto-><----------1fr--------->'>
        {when(flexDirection.startsWith('row'), <Icons.WrapRow />)}
        {when(flexDirection.startsWith('column'), <Icons.WrapColumn />)}
        <PopupList
          value={FlexWrapOptions.find((option) => option.value === props.value)}
          options={FlexWrapOptions}
          onSubmitValue={onSubmitValue}
          controlStyles={props.controlStyles}
          style={{ background: 'transparent' }}
        />
      </UIGridRow>
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
  const { value, onSubmitValue, onUnsetValues, onTransientSubmitValue, controlStatus } =
    useInspectorLayoutInfo('gap')
  const menuItems = [unsetPropertyMenuItem('Flex Gap', onUnsetValues)]

  const wrappedOnSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(onSubmitValue, onUnsetValues)
  const wrappedOnTransientSubmitValue = useWrappedEmptyOrUnknownOnSubmitValue(
    onTransientSubmitValue,
    onUnsetValues,
  )

  const flexDirection = useEditorState(
    Substores.metadata,
    flexDirectionSelector,
    'FlexGapControl flexDirection',
  )

  const [hoverStart, hoverEnd] = useOnControlHover(FlexGapControlTestId)

  return (
    <InspectorContextMenuWrapper id={`gap-context-menu`} items={menuItems} data={{}}>
      <UIGridRow
        padded={false}
        variant='<-------------1fr------------->'
        onMouseEnter={hoverStart}
        onMouseLeave={hoverEnd}
      >
        <NumberInput
          id='flex.container.gap'
          testId='flex.container.gap'
          key='flex.container.gap'
          value={value}
          onSubmitValue={wrappedOnSubmitValue}
          onTransientSubmitValue={wrappedOnTransientSubmitValue}
          onForcedSubmitValue={wrappedOnSubmitValue}
          controlStatus={controlStatus}
          numberType='LengthPercent'
          defaultUnitToHide={'px'}
          DEPRECATED_labelBelow={
            flexDirection.startsWith('row') ? <Icons.GapHorizontal /> : <Icons.GapVertical />
          }
          incrementControls={false}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})

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
