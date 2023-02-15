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
import { CSSNumber } from '../../../common/css-utils'
import {
  useInspectorContext,
  useInspectorLayoutInfo,
  useInspectorStyleInfo,
} from '../../../common/property-path-hooks'
import { selectedViewsSelector } from '../../../inpector-selectors'
import { PropertyLabel } from '../../../widgets/property-label'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import {
  aggregateGroups,
  areAllSidesSet,
  ControlMode,
  getInitialMode,
  getSplitChainedNumberInputValues,
  getSplitControlValues,
  longhandShorthandEventHandler,
  Sides,
  SidesAbstract,
  SidesCSSNumber,
  SplitChainedNumberInput,
} from '../../layout-section/layout-system-subsection/split-chained-number-input'

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
      <UIGridRow tall padded={true} variant='<---1fr--->|------172px-------|'>
        <PropertyLabel
          target={[]}
          propNamesToUnset={contextMenuLabel}
          style={{
            paddingBottom: 20,
          }}
        >
          Radius
        </PropertyLabel>
        <BorderRadiusControl />
      </UIGridRow>
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

  const isCmdPressedRef = useRefEditorState((store) => store.editor.keysPressed.cmd === true)

  const [overriddenMode, setOveriddenMode] = React.useState<ControlMode | null>(null)
  const cycleToNextMode = React.useCallback(() => {
    const delta = isCmdPressedRef.current ? -1 : 1
    const index =
      BorderRadiusControlModeOrder.indexOf(overriddenMode ?? BorderRadiusControlDefaultMode) + delta
    setOveriddenMode(
      BorderRadiusControlModeOrder[wrapValue(index, 0, BorderRadiusControlModeOrder.length - 1)],
    )
  }, [isCmdPressedRef, overriddenMode])

  useSelectorWithCallback(
    Substores.selectedViews,
    selectedViewsSelector,
    () => setOveriddenMode(null),
    'aa aaaa',
  )

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
        dispatch,
      ),
    [aggregates, dispatch, selectedViewsRef, useShorthand],
  )

  const mode = React.useMemo(
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

  return (
    <SplitChainedNumberInput
      labels={{
        top: 'TL',
        bottom: 'BR',
        left: 'BL',
        right: 'TR',
      }}
      tooltips={{
        oneValue: 'Radius',
        perSide: 'Radius per corner',
      }}
      overrideModeCallback={cycleToNextMode}
      numberType={'LengthPercent'}
      name='radius'
      mode={overriddenMode ?? mode}
      values={values}
      canvasControls={canvasControlsForSides}
      eventHandler={eventHandler}
    />
  )
})
