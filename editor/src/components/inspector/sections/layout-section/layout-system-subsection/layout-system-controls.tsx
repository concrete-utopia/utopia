import React from 'react'
import { useContextSelector } from 'use-context-selector'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { mapArrayToDictionary } from '../../../../../core/shared/array-utils'
import type { PropertyPath } from '../../../../../core/shared/project-file-types'
import { Icons } from '../../../../../uuiui'
import { useSetHoveredControlsHandlers } from '../../../../canvas/controls/select-mode/select-mode-hooks'
import type { SubduedPaddingControlProps } from '../../../../canvas/controls/select-mode/subdued-padding-control'
import { SubduedPaddingControl } from '../../../../canvas/controls/select-mode/subdued-padding-control'
import {
  EdgePieces,
  getSizeUpdateCommandsForNewPadding,
  pixelPaddingFromPadding,
} from '../../../../canvas/padding-utils'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { applyCommandsAction } from '../../../../editor/actions/action-creators'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import {
  Substores,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../../../editor/store/store-hook'
import { optionalAddOnUnsetValues } from '../../../common/context-menu-items'
import type { CanvasControlWithProps } from '../../../common/inspector-atoms'
import { useControlModeWithCycle } from '../../../common/inspector-utils'
import { useInspectorInfoLonghandShorthand } from '../../../common/longhand-shorthand-hooks'
import {
  InspectorPropsContext,
  stylePropPathMappingFn,
  useInspectorContext,
  useInspectorInfoSimpleUntyped,
  useInspectorLayoutInfo,
} from '../../../common/property-path-hooks'
import { selectedViewsSelector } from '../../../inpector-selectors'
import type {
  ControlMode,
  SplitChainedEvent,
  SplitControlValues,
} from './split-chained-number-input'
import {
  SplitChainedNumberInput,
  aggregateGroups,
  areAllSidesSet,
  getInitialMode,
  getSplitChainedNumberInputValues,
  getSplitControlValues,
  longhandShorthandEventHandler,
  splitChainedEventValueForProp,
} from './split-chained-number-input'

function buildPaddingPropsToUnset(propertyTarget: ReadonlyArray<string>): Array<PropertyPath> {
  return [
    stylePropPathMappingFn('padding', propertyTarget),
    stylePropPathMappingFn('paddingLeft', propertyTarget),
    stylePropPathMappingFn('paddingTop', propertyTarget),
    stylePropPathMappingFn('paddingRight', propertyTarget),
    stylePropPathMappingFn('paddingBottom', propertyTarget),
  ]
}

export const PaddingRow = React.memo(() => {
  const targetPath = useContextSelector(InspectorPropsContext, (contextData) => {
    return contextData.targetPath
  })
  const paddingPropsToUnset = React.useMemo(() => {
    return buildPaddingPropsToUnset(targetPath)
  }, [targetPath])
  const metadata = useInspectorInfoSimpleUntyped(
    paddingPropsToUnset,
    (v) => v,
    (v) => v,
  )

  const contextMenuLabel = React.useMemo(() => ['all paddings'], [])
  const contextMenuItems = React.useMemo(
    () =>
      optionalAddOnUnsetValues(
        metadata.propertyStatus.set,
        contextMenuLabel,
        metadata.onUnsetValues,
      ),
    [contextMenuLabel, metadata.propertyStatus.set, metadata.onUnsetValues],
  )

  const paddingControlsForHover: Array<CanvasControlWithProps<SubduedPaddingControlProps>> =
    React.useMemo(
      () =>
        EdgePieces.map((side) => ({
          control: SubduedPaddingControl,
          props: {
            side: side,
            hoveredOrFocused: 'hovered',
          },
          key: `subdued-padding-control-hovered-${side}`,
        })),
      [],
    )

  const { onMouseEnter, onMouseLeave } = useSetHoveredControlsHandlers<SubduedPaddingControlProps>()
  const onMouseEnterWithPaddingControls = React.useCallback(
    () => onMouseEnter(paddingControlsForHover),
    [onMouseEnter, paddingControlsForHover],
  )

  return (
    <InspectorContextMenuWrapper
      id='padding-subsection-context-menu'
      items={contextMenuItems}
      data={null}
    >
      <div
        onMouseEnter={onMouseEnterWithPaddingControls}
        onMouseLeave={onMouseLeave}
        style={{ margin: '8px 8px', display: 'flex', flexDirection: 'row' }}
      >
        <PaddingControl />
      </div>
    </InspectorContextMenuWrapper>
  )
})

const PaddingControlModeOrder: ControlMode[] = ['one-value', 'per-direction', 'per-side']
const PaddingControlDefaultMode: ControlMode = 'per-direction'
const PaddingControl = React.memo(() => {
  const { paddingTop, paddingRight, paddingBottom, paddingLeft } =
    useInspectorInfoLonghandShorthand(
      ['paddingTop', 'paddingRight', 'paddingBottom', 'paddingLeft'],
      'padding',
      stylePropPathMappingFn,
    )

  const shorthand = useInspectorLayoutInfo('padding')
  const dispatch = useDispatch()

  const { selectedViewsRef } = useInspectorContext()

  const canvasControlsForSides = React.useMemo(() => {
    return mapArrayToDictionary(
      ['top', 'right', 'bottom', 'left'],
      (k) => k,
      (side) => ({
        onHover: {
          control: SubduedPaddingControl,
          props: {
            side: side,
            hoveredOrFocused: 'hovered',
          },
          key: `subdued-padding-control-hovered-${side}`,
        },
        onFocus: {
          control: SubduedPaddingControl,
          props: {
            side: side,
            hoveredOrFocused: 'focused',
          },
          key: `subdued-padding-control-focused-${side}`,
        },
      }),
    )
  }, [])

  const allUnset = React.useMemo(() => {
    return (
      paddingTop.controlStatus === 'trivial-default' &&
      paddingBottom.controlStatus === 'trivial-default' &&
      paddingLeft.controlStatus === 'trivial-default' &&
      paddingRight.controlStatus === 'trivial-default'
    )
  }, [
    paddingTop.controlStatus,
    paddingBottom.controlStatus,
    paddingLeft.controlStatus,
    paddingRight.controlStatus,
  ])

  const useShorthand = React.useMemo(() => {
    return shorthand.controlStatus === 'simple' || allUnset
  }, [allUnset, shorthand.controlStatus])

  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const pathTreesRef = useRefEditorState((store) => store.editor.elementPathTree)
  const startingFrame = MetadataUtils.getFrameOrZeroRect(
    selectedViewsRef.current[0],
    metadataRef.current,
  )

  const onPaddingChange = React.useCallback(
    (e: SplitChainedEvent, aggregates: SplitControlValues) => {
      const selectedElement = selectedViewsRef.current[0]
      const metadata = metadataRef.current

      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, selectedElement)
      const elementParentBounds = elementMetadata?.specialSizeMeasurements.immediateParentBounds
      const parentWidth = elementParentBounds?.width ?? 0
      const parentHeight = elementParentBounds?.height ?? 0

      const pixelPaddingTop = pixelPaddingFromPadding(
        splitChainedEventValueForProp('T', e) ?? aggregates.top,
        parentHeight,
      )
      const pixelPaddingBottom = pixelPaddingFromPadding(
        splitChainedEventValueForProp('B', e) ?? aggregates.bottom,
        parentHeight,
      )
      const pixelPaddingRight = pixelPaddingFromPadding(
        splitChainedEventValueForProp('R', e) ?? aggregates.right,
        parentWidth,
      )
      const pixelPaddingLeft = pixelPaddingFromPadding(
        splitChainedEventValueForProp('L', e) ?? aggregates.left,
        parentWidth,
      )

      const combinedXPadding =
        pixelPaddingLeft == null || pixelPaddingRight == null
          ? null
          : pixelPaddingLeft + pixelPaddingRight
      const combinedYPadding =
        pixelPaddingTop == null || pixelPaddingBottom == null
          ? null
          : pixelPaddingTop + pixelPaddingBottom

      const adjustSizeCommand = getSizeUpdateCommandsForNewPadding(
        combinedXPadding,
        combinedYPadding,
        startingFrame,
        selectedViewsRef.current,
        metadataRef.current,
        pathTreesRef.current,
      )

      return adjustSizeCommand.properties.length > 0
        ? [applyCommandsAction([adjustSizeCommand])]
        : []
    },
    [metadataRef, pathTreesRef, selectedViewsRef, startingFrame],
  )

  const splitContolGroups = React.useMemo(
    () =>
      aggregateGroups({
        top: paddingTop,
        left: paddingLeft,
        right: paddingRight,
        bottom: paddingBottom,
      }),
    [paddingBottom, paddingLeft, paddingRight, paddingTop],
  )

  const aggregates = React.useMemo(
    () =>
      getSplitControlValues(splitContolGroups, {
        top: paddingTop,
        left: paddingLeft,
        right: paddingRight,
        bottom: paddingBottom,
      }),
    [paddingBottom, paddingLeft, paddingRight, paddingTop, splitContolGroups],
  )

  const values = React.useMemo(
    () =>
      getSplitChainedNumberInputValues(splitContolGroups, {
        top: paddingTop,
        left: paddingLeft,
        right: paddingRight,
        bottom: paddingBottom,
      }),
    [splitContolGroups, paddingBottom, paddingLeft, paddingRight, paddingTop],
  )

  const eventHandler = React.useMemo(
    () =>
      longhandShorthandEventHandler(
        'padding',
        {
          T: 'paddingTop',
          R: 'paddingRight',
          B: 'paddingBottom',
          L: 'paddingLeft',
        },
        selectedViewsRef,
        useShorthand,
        aggregates,
        allUnset,
        dispatch,
        onPaddingChange,
      ),
    [aggregates, allUnset, dispatch, onPaddingChange, selectedViewsRef, useShorthand],
  )

  const initialMode = React.useMemo(
    () =>
      getInitialMode(
        aggregates.oneValue,
        aggregates.horizontal,
        aggregates.vertical,
        areAllSidesSet(splitContolGroups.allSides),
        PaddingControlDefaultMode,
      ),
    [aggregates.horizontal, aggregates.oneValue, aggregates.vertical, splitContolGroups.allSides],
  )

  const [controlMode, cycleToNextMode, resetOverridenMode] = useControlModeWithCycle(
    initialMode,
    PaddingControlModeOrder,
  )

  useSelectorWithCallback(
    Substores.selectedViews,
    selectedViewsSelector,
    () => resetOverridenMode(),
    'PaddingControl setOveriddenMode',
  )

  const isCmdPressedRef = useRefEditorState((store) => store.editor.keysPressed.cmd === true)

  const onCycleMode = React.useCallback(
    () => cycleToNextMode(initialMode, isCmdPressedRef.current === true ? 'backward' : 'forward'),
    [cycleToNextMode, initialMode, isCmdPressedRef],
  )

  const modeToUse = controlMode ?? initialMode

  return (
    <SplitChainedNumberInput
      labels={{
        oneValue: <Icons.Padding color='on-highlight-secondary' />,
        horizontal: <Icons.PaddingHorizontal color='on-highlight-secondary' />,
        vertical: <Icons.PaddingVertical color='on-highlight-secondary' />,
        top: <Icons.PaddingTop color='on-highlight-secondary' />,
        left: <Icons.PaddingLeft color='on-highlight-secondary' />,
        bottom: <Icons.PaddingBottom color='on-highlight-secondary' />,
        right: <Icons.PaddingRight color='on-highlight-secondary' />,
      }}
      tooltips={{
        oneValue: 'Padding',
        perDirection: 'Padding per direction',
        perSide: 'Padding per side',
      }}
      name='padding'
      canvasControls={canvasControlsForSides}
      numberType={'LengthPercent'}
      eventHandler={eventHandler}
      mode={modeToUse}
      onCycleMode={onCycleMode}
      values={values}
      cycleModePosition='back'
    />
  )
})
