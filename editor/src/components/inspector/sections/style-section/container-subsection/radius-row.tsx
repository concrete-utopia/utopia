/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { mapArrayToDictionary } from '../../../../../core/shared/array-utils'
import { foldEither } from '../../../../../core/shared/either'
import { wrapValue } from '../../../../../core/shared/math-utils'
import { InspectorContextMenuItems } from '../../../../../uuiui-deps'
import { SubduedBorderRadiusControl } from '../../../../canvas/controls/select-mode/subdued-border-radius-control'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import {
  Substores,
  useRefEditorState,
  useSelectorWithCallback,
} from '../../../../editor/store/store-hook'
import type { CSSNumber } from '../../../common/css-utils'
import { useControlModeWithCycle } from '../../../common/inspector-utils'
import {
  useInspectorContext,
  useInspectorLayoutInfo,
  useInspectorStyleInfo,
} from '../../../common/property-path-hooks'
import { selectedViewsSelector } from '../../../inpector-selectors'
import { PropertyLabel } from '../../../widgets/property-label'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import type {
  ControlMode,
  SidesCSSNumber,
} from '../../layout-section/layout-system-subsection/split-chained-number-input'
import {
  aggregateGroups,
  areAllSidesSet,
  getInitialMode,
  getSplitChainedNumberInputValues,
  getSplitControlValues,
  longhandShorthandEventHandler,
  SplitChainedNumberInput,
} from '../../layout-section/layout-system-subsection/split-chained-number-input'
import { Icons } from '../../../../../uuiui/icons'

export const RadiusRow = React.memo(() => {
  const { value: borderRadiusValue, onUnsetValues } = useInspectorStyleInfo('borderRadius')
  const contextMenuItems = InspectorContextMenuItems.optionalAddOnUnsetValues(
    borderRadiusValue != null,
    ['borderRadius'],
    onUnsetValues,
  )
  const contextMenuLabel = React.useMemo(() => ['border radius'], [])
  return (
    <InspectorContextMenuWrapper
      id='borderRadius-subsection-context-menu'
      items={contextMenuItems}
      data={null}
    >
      <div
        style={{
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
          justifyContent: 'flex-start',
          padding: '8px 0',
        }}
      >
        <PropertyLabel
          target={[]}
          propNamesToUnset={contextMenuLabel}
          style={{ paddingRight: 2, flexShrink: 0 }}
        >
          Radius
        </PropertyLabel>
        <BorderRadiusControl />
      </div>
    </InspectorContextMenuWrapper>
  )
})

const BorderRadiusControlModeOrder: ControlMode[] = ['one-value', 'per-side']
const BorderRadiusControlDefaultMode: ControlMode = 'one-value'
export const BorderRadiusControl = React.memo(() => {
  const borderRadius = useInspectorStyleInfo('borderRadius')

  const shorthand = useInspectorLayoutInfo('borderRadius')

  const dispatch = useDispatch()

  const { selectedViewsRef } = useInspectorContext()

  const tl: CSSNumber = React.useMemo(
    () =>
      foldEither(
        (n) => n,
        (i) => i.tl,
        borderRadius.value,
      ),
    [borderRadius],
  )
  const tr: CSSNumber = React.useMemo(
    () =>
      foldEither(
        (n) => n,
        (i) => i.tr,
        borderRadius.value,
      ),
    [borderRadius],
  )
  const bl: CSSNumber = React.useMemo(
    () =>
      foldEither(
        (n) => n,
        (i) => i.bl,
        borderRadius.value,
      ),
    [borderRadius],
  )
  const br: CSSNumber = React.useMemo(
    () =>
      foldEither(
        (n) => n,
        (i) => i.br,
        borderRadius.value,
      ),
    [borderRadius],
  )

  const sides: SidesCSSNumber = React.useMemo(
    () => ({
      top: {
        ...borderRadius,
        value: tl,
      },
      left: {
        ...borderRadius,
        value: bl,
      },
      bottom: {
        ...borderRadius,
        value: br,
      },
      right: {
        ...borderRadius,
        value: tr,
      },
    }),
    [bl, borderRadius, br, tl, tr],
  )

  const splitContolGroups = React.useMemo(() => aggregateGroups(sides), [sides])

  const aggregates = React.useMemo(
    () => getSplitControlValues(splitContolGroups, sides),
    [sides, splitContolGroups],
  )

  const values = React.useMemo(
    () => getSplitChainedNumberInputValues(splitContolGroups, sides),
    [splitContolGroups, sides],
  )

  const canvasControlsForSides = React.useMemo(() => {
    return mapArrayToDictionary(
      ['top'],
      (k) => k,
      (side) => ({
        onHover: {
          control: SubduedBorderRadiusControl,
          props: {
            hoveredOrFocused: 'hovered',
          },
          key: `subdued-border-radius-control-hovered`,
        },
        onFocus: {
          control: SubduedBorderRadiusControl,
          props: {
            hoveredOrFocused: 'focused',
          },
          key: `subdued-border-radius-control-focused`,
        },
      }),
    )
  }, [])

  const allUnset = React.useMemo(() => {
    return borderRadius.controlStatus === 'trivial-default'
  }, [borderRadius.controlStatus])

  const useShorthand = React.useMemo(() => {
    return shorthand.controlStatus === 'simple' || allUnset
  }, [allUnset, shorthand.controlStatus])

  const eventHandler = React.useMemo(
    () =>
      longhandShorthandEventHandler(
        'borderRadius',
        {
          T: 'borderTopLeftRadius',
          R: 'borderTopRightRadius',
          B: 'borderBottomRightRadius',
          L: 'borderBottomLeftRadius',
        },
        selectedViewsRef,
        useShorthand,
        aggregates,
        allUnset,
        dispatch,
      ),
    [aggregates, allUnset, dispatch, selectedViewsRef, useShorthand],
  )

  const initialMode = React.useMemo(
    () =>
      getInitialMode(
        aggregates.oneValue,
        aggregates.horizontal,
        aggregates.vertical,
        areAllSidesSet(splitContolGroups.allSides),
        BorderRadiusControlDefaultMode,
      ),
    [aggregates.horizontal, aggregates.oneValue, aggregates.vertical, splitContolGroups.allSides],
  )

  const [controlMode, cycleToNextMode, resetControlMode] = useControlModeWithCycle(
    BorderRadiusControlDefaultMode,
    BorderRadiusControlModeOrder,
  )

  useSelectorWithCallback(
    Substores.selectedViews,
    selectedViewsSelector,
    () => resetControlMode(),
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
        top: <Icons.BorderRadiusTopLeft color='on-highlight-secondary' />,
        bottom: <Icons.BorderRadiusBottomRight color='on-highlight-secondary' />,
        left: <Icons.BorderRadiusBottomLeft color='on-highlight-secondary' />,
        right: <Icons.BorderRadiusTopRight color='on-highlight-secondary' />,
        oneValue: <Icons.BorderRadius color='on-highlight-secondary' />,
      }}
      tooltips={{
        oneValue: 'Radius',
        perSide: 'Radius per corner',
      }}
      onCycleMode={onCycleMode}
      numberType={'LengthPercent'}
      name='radius'
      mode={modeToUse}
      values={values}
      canvasControls={canvasControlsForSides}
      eventHandler={eventHandler}
    />
  )
})
