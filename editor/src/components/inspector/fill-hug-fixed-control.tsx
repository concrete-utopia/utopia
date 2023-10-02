/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { createSelector } from 'reselect'
import { optionalMap } from '../../core/shared/optional-utils'
import { intersection } from '../../core/shared/set-utils'
import { assertNever } from '../../core/shared/utils'
import { NumberInput, PopupList, useColorTheme, UtopiaTheme } from '../../uuiui'
import type {
  ControlStatus,
  ControlStyles,
  OnSubmitValueOrUnknownOrEmpty,
  SelectOption,
} from '../../uuiui-deps'
import { getControlStyles, InspectorRowHoverCSS } from '../../uuiui-deps'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import type { CSSNumber, CSSNumberType, UnknownOrEmptyInput } from './common/css-utils'
import { cssNumber } from './common/css-utils'
import {
  metadataSelector,
  pathTreesSelector,
  selectedViewsSelector,
  useComputedSizeRef,
} from './inpector-selectors'
import type { Axis, FixedHugFill, FixedHugFillMode } from './inspector-common'
import {
  detectFillHugFixedStateMultiselect,
  getFixedFillHugOptionsForElement,
  isFixedHugFillEqual,
} from './inspector-common'
import {
  setPropFillStrategies,
  setPropFixedStrategies,
  setPropHugStrategies,
} from './inspector-strategies/inspector-strategies'
import type { InspectorStrategy } from './inspector-strategies/inspector-strategy'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import type { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import type { ElementPath } from '../../core/shared/project-file-types'
import { treatElementAsGroupLike } from '../canvas/canvas-strategies/strategies/group-helpers'
import * as EP from '../../core/shared/element-path'
import * as PP from '../../core/shared/property-path'
import type { GridRowVariant } from './widgets/ui-grid-row'
import { UIGridRow } from './widgets/ui-grid-row'
import { mapDropNulls, safeIndex, uniqBy } from '../../core/shared/array-utils'
import { fixedSizeDimensionHandlingText } from '../text-editor/text-handling'
import { when } from '../../utils/react-conditionals'
import { isLayoutPinnedProp, type LayoutPinnedProp } from '../../core/layout/layout-helpers-new'
import type { AllElementProps } from '../editor/store/editor-state'
import { jsExpressionValue, emptyComments } from '../../core/shared/element-template'
import type { EditorDispatch, EditorAction } from '../editor/action-types'
import { unsetProperty, setProp_UNSAFE } from '../editor/actions/action-creators'
import { FlexCol } from 'utopia-api'
import { PinControl } from './controls/pin-control'
import type { FramePinsInfo } from './common/layout-property-path-hooks'
import { MixedPlaceholder } from '../../uuiui/inputs/base-input'

export const FillFixedHugControlId = (segment: 'width' | 'height'): string =>
  `hug-fixed-fill-${segment}`

export const FillContainerLabel = 'Fill container' as const
export const FixedLabel = 'Fixed' as const
export const HugContentsLabel = 'Hug contents' as const
export const HugGroupContentsLabel = 'Hug contents' as const
export const ComputedLabel = 'Computed' as const
export const DetectedLabel = 'Detected' as const

export function selectOptionLabel(mode: FixedHugFillMode): string {
  switch (mode) {
    case 'fill':
      return FillContainerLabel
    case 'fixed':
      return FixedLabel
    case 'hug':
      return HugContentsLabel
    case 'hug-group':
      return HugGroupContentsLabel
    case 'computed':
      return ComputedLabel
    case 'detected':
      return DetectedLabel
    default:
      assertNever(mode)
  }
}

function selectOption(mode: FixedHugFillMode): SelectOption {
  return {
    value: mode,
    label: selectOptionLabel(mode),
  }
}

interface FillHugFixedControlProps {}

const fixedHugFillOptionsSelector = createSelector(
  metadataSelector,
  pathTreesSelector,
  selectedViewsSelector,
  (metadata, pathTrees, selectedViews) => {
    const applicableOptions: Array<FixedHugFillMode> = [
      ...intersection(
        selectedViews.map((selectedView) =>
          getFixedFillHugOptionsForElement(metadata, pathTrees, selectedView),
        ),
      ),
    ]

    return applicableOptions.map(selectOption)
  },
)

export const FillHugFixedControl = React.memo<FillHugFixedControlProps>((props) => {
  const dispatch = useDispatch()
  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const elementOrParentGroupRef = useRefEditorState(anySelectedElementGroupOrChildOfGroup)
  const groupChildrenSelectedRef = useRefEditorState(allElementsAreGroupChildren)

  const widthCurrentValue = useEditorState(
    Substores.metadata,
    (store) =>
      detectFillHugFixedStateMultiselect(
        'horizontal',
        metadataSelector(store),
        selectedViewsSelector(store),
      ),
    'FillHugFixedControl widthCurrentValue',
    isFixedHugFillEqual,
  )

  const widthControlStyles = React.useMemo(
    () => getControlStyles(widthCurrentValue.controlStatus),
    [widthCurrentValue],
  )
  const widthInputControlStatus = React.useMemo(
    () =>
      isNumberInputEnabled(widthCurrentValue.fixedHugFill)
        ? widthCurrentValue.controlStatus
        : 'disabled',
    [widthCurrentValue],
  )

  const fillsContainerHorizontallyRef = useRefEditorState(
    (store) =>
      detectFillHugFixedStateMultiselect(
        'horizontal',
        metadataSelector(store),
        selectedViewsSelector(store),
      ).fixedHugFill?.type === 'fill',
  )

  const widthComputedValueRef = useComputedSizeRef('width')

  const heightCurrentValue = useEditorState(
    Substores.metadata,
    (store) =>
      detectFillHugFixedStateMultiselect(
        'vertical',
        metadataSelector(store),
        selectedViewsSelector(store),
      ),
    'FillHugFixedControl heightCurrentValue',
    isFixedHugFillEqual,
  )

  const heightControlStyles = React.useMemo(
    () => getControlStyles(heightCurrentValue.controlStatus),
    [heightCurrentValue],
  )
  const heightInputControlStatus = React.useMemo(
    () =>
      isNumberInputEnabled(heightCurrentValue.fixedHugFill)
        ? heightCurrentValue.controlStatus
        : 'disabled',
    [heightCurrentValue],
  )

  const fillsContainerVerticallyRef = useRefEditorState(
    (store) =>
      detectFillHugFixedStateMultiselect(
        'vertical',
        metadataSelector(store),
        selectedViewsSelector(store),
      ).fixedHugFill?.type === 'fill',
  )

  const heightComputedValueRef = useComputedSizeRef('height')

  const onSubmitFixedFillHugType = React.useMemo(() => {
    const onSubmitFFH =
      (axis: Axis) =>
      ({ value: anyValue }: SelectOption) => {
        const value = anyValue as FixedHugFillMode

        const currentComputedValue =
          (axis === 'horizontal'
            ? widthComputedValueRef.current
            : heightComputedValueRef.current) ?? 0

        const otherAxisSetToFill =
          axis === 'horizontal'
            ? fillsContainerVerticallyRef.current
            : fillsContainerHorizontallyRef.current

        const firstSelectedView = safeIndex(selectedViewsRef.current, 0)
        if (firstSelectedView != null) {
          const valueToUse =
            axis === 'horizontal'
              ? fixedSizeDimensionHandlingText(
                  metadataRef.current,
                  elementPathTreeRef.current,
                  firstSelectedView,
                  currentComputedValue,
                )
              : currentComputedValue
          const strategy = strategyForChangingFillFixedHugType(
            valueToUse,
            axis,
            value,
            otherAxisSetToFill,
          )
          executeFirstApplicableStrategy(
            dispatch,
            metadataRef.current,
            selectedViewsRef.current,
            elementPathTreeRef.current,
            allElementPropsRef.current,
            strategy,
          )
        }
      }

    return {
      width: onSubmitFFH('horizontal'),
      height: onSubmitFFH('vertical'),
    }
  }, [
    allElementPropsRef,
    dispatch,
    fillsContainerHorizontallyRef,
    fillsContainerVerticallyRef,
    widthComputedValueRef,
    heightComputedValueRef,
    metadataRef,
    elementPathTreeRef,
    selectedViewsRef,
  ])

  const onAdjustHeight = React.useCallback(
    (value: UnknownOrEmptyInput<CSSNumber>) => {
      if (
        'type' in value &&
        (value.type === 'EMPTY_INPUT_VALUE' || value.type === 'UNKNOWN_INPUT')
      ) {
        return
      }
      if (elementOrParentGroupRef.current) {
        if (value.unit != null && value.unit !== 'px') {
          // if the element or its parent is a group, we only allow setting the size to Fixed pixels to avoid inconsistent behavior
          return
        }
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFixedStrategies('always', 'vertical', value),
        )
        return
      }
      if (heightCurrentValue.fixedHugFill?.type === 'fill') {
        if (value.unit != null && value.unit !== '%') {
          // fill mode only accepts percentage or valueless numbers
          return
        }
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFillStrategies('vertical', value.value, false),
        )
      }
      if (
        heightCurrentValue.fixedHugFill?.type === 'fixed' ||
        heightCurrentValue.fixedHugFill?.type === 'computed'
      ) {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFixedStrategies('always', 'vertical', value),
        )
      }
    },
    [
      allElementPropsRef,
      dispatch,
      heightCurrentValue.fixedHugFill?.type,
      metadataRef,
      elementPathTreeRef,
      selectedViewsRef,
      elementOrParentGroupRef,
    ],
  )

  const onAdjustWidth = React.useCallback(
    (value: UnknownOrEmptyInput<CSSNumber>) => {
      if (
        'type' in value &&
        (value.type === 'EMPTY_INPUT_VALUE' || value.type === 'UNKNOWN_INPUT')
      ) {
        return
      }
      if (elementOrParentGroupRef.current) {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFixedStrategies('always', 'horizontal', value),
        )
        return
      }
      if (widthCurrentValue.fixedHugFill?.type === 'fill') {
        if (value.unit != null && value.unit !== '%') {
          // fill mode only accepts percentage or valueless numbers
          return
        }
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFillStrategies('horizontal', value.value, false),
        )
      }
      if (
        widthCurrentValue.fixedHugFill?.type === 'fixed' ||
        widthCurrentValue.fixedHugFill?.type === 'computed'
      ) {
        executeFirstApplicableStrategy(
          dispatch,
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          setPropFixedStrategies('always', 'horizontal', value),
        )
      }
    },
    [
      allElementPropsRef,
      dispatch,
      metadataRef,
      selectedViewsRef,
      elementPathTreeRef,
      widthCurrentValue.fixedHugFill?.type,
      elementOrParentGroupRef,
    ],
  )

  const groupChildConstraints = useEditorState(
    Substores.metadata,
    (store) => ({
      width: checkGroupChildConstraint(
        'width',
        store.editor.selectedViews,
        store.editor.allElementProps,
      ),
      height: checkGroupChildConstraint(
        'height',
        store.editor.selectedViews,
        store.editor.allElementProps,
      ),
      top: checkGroupChildConstraint(
        'top',
        store.editor.selectedViews,
        store.editor.allElementProps,
      ),
      left: checkGroupChildConstraint(
        'left',
        store.editor.selectedViews,
        store.editor.allElementProps,
      ),
      bottom: checkGroupChildConstraint(
        'bottom',
        store.editor.selectedViews,
        store.editor.allElementProps,
      ),
      right: checkGroupChildConstraint(
        'right',
        store.editor.selectedViews,
        store.editor.allElementProps,
      ),
    }),
    'FillHugFixedControl constraints',
  )

  const handleGroupConstraintPinMouseDown = React.useCallback(
    (dimension: LayoutPinnedProp) => {
      return setGroupChildConstraint(
        dispatch,
        'toggle',
        selectedViewsRef.current,
        allElementPropsRef.current,
        dimension,
      )
    },
    [dispatch, selectedViewsRef, allElementPropsRef],
  )

  const framePoints: FramePinsInfo = React.useMemo(() => {
    const ignore = { isPrimaryPosition: false, isRelativePosition: false }
    return {
      left: {
        isPrimaryPosition: groupChildConstraints.left !== 'not-constrained',
        isRelativePosition: false,
      },
      top: {
        isPrimaryPosition: groupChildConstraints.top !== 'not-constrained',
        isRelativePosition: false,
      },
      bottom: {
        isPrimaryPosition: groupChildConstraints.bottom !== 'not-constrained',
        isRelativePosition: false,
      },
      right: {
        isPrimaryPosition: groupChildConstraints.right !== 'not-constrained',
        isRelativePosition: false,
      },
      width: ignore,
      height: ignore,
      centerX: ignore,
      centerY: ignore,
    }
  }, [groupChildConstraints])

  const widthValue = optionalMap(pickFixedValue, widthCurrentValue.fixedHugFill) ?? null
  const heightValue = optionalMap(pickFixedValue, heightCurrentValue.fixedHugFill) ?? null

  return (
    <FlexCol css={{ paddingBottom: UtopiaTheme.layout.rowHorizontalPadding }}>
      <UIGridRow padded variant='<-auto-><----------1fr--------->'>
        {when(
          groupChildrenSelectedRef.current,
          <PinControl
            handlePinMouseDown={handleGroupConstraintPinMouseDown}
            framePoints={framePoints}
            controlStatus='simple'
            exclude={{ center: true }}
            name='group-child-controls'
          />,
        )}
        <FlexCol css={{ gap: 0 }}>
          <DimensionRow
            dimension='width'
            dimensionValue={widthValue}
            dimensionControlStatus={widthInputControlStatus}
            dimensionNumberType={pickNumberType(widthCurrentValue.fixedHugFill)}
            fixedHugSelectOption={
              optionalMap(selectOption, widthCurrentValue.fixedHugFill?.type) ?? null
            }
            fixedHugControlStyles={widthControlStyles}
            onAdjustDimension={onAdjustWidth}
            onSubmitFixedHug={onSubmitFixedFillHugType['width']}
            isGroupChild={groupChildrenSelectedRef.current}
            groupChildConstraint={groupChildConstraints.width}
          />
          <DimensionRow
            dimension='height'
            dimensionValue={heightValue}
            dimensionControlStatus={heightInputControlStatus}
            dimensionNumberType={pickNumberType(heightCurrentValue.fixedHugFill)}
            fixedHugSelectOption={
              optionalMap(selectOption, heightCurrentValue.fixedHugFill?.type) ?? null
            }
            fixedHugControlStyles={heightControlStyles}
            onAdjustDimension={onAdjustHeight}
            onSubmitFixedHug={onSubmitFixedFillHugType['height']}
            isGroupChild={groupChildrenSelectedRef.current}
            groupChildConstraint={groupChildConstraints.height}
          />
        </FlexCol>
      </UIGridRow>
    </FlexCol>
  )
})

const DimensionRow = React.memo(
  ({
    dimension,
    dimensionValue,
    dimensionControlStatus,
    dimensionNumberType,
    fixedHugSelectOption,
    fixedHugControlStyles,
    onAdjustDimension,
    onSubmitFixedHug,
    isGroupChild,
    groupChildConstraint,
  }: {
    dimension: 'width' | 'height'
    dimensionValue: CSSNumber | null
    dimensionControlStatus: ControlStatus
    dimensionNumberType: CSSNumberType
    fixedHugSelectOption: SelectOption | null
    fixedHugControlStyles: ControlStyles
    onAdjustDimension: OnSubmitValueOrUnknownOrEmpty<CSSNumber>
    onSubmitFixedHug: (option: SelectOption) => void
    isGroupChild: boolean
    groupChildConstraint: GroupChildConstraintOptionType | 'mixed'
  }) => {
    const fixedHugFillOptions = useEditorState(
      Substores.metadata,
      fixedHugFillOptionsSelector,
      'DimensionRow options',
    )

    const gridTemplate = React.useMemo((): GridRowVariant => {
      return isGroupChild ? '|--67px--|<--------1fr-------->' : '<--1fr--><--1fr-->'
    }, [isGroupChild])

    const labelInner = React.useMemo(() => {
      return dimension.charAt(0).toUpperCase()
    }, [dimension])

    return (
      <UIGridRow padded={false} variant={gridTemplate} css={InspectorRowHoverCSS}>
        <NumberInput
          labelInner={labelInner}
          id={FillFixedHugControlId(dimension)}
          testId={FillFixedHugControlId(dimension)}
          value={dimensionValue}
          onSubmitValue={onAdjustDimension}
          onTransientSubmitValue={onAdjustDimension}
          onForcedSubmitValue={onAdjustDimension}
          controlStatus={dimensionControlStatus}
          numberType={dimensionNumberType}
          incrementControls={true}
          stepSize={1}
          minimum={0}
          maximum={Infinity}
          defaultUnitToHide={null}
          focusOnMount={false}
        />
        {isGroupChild ? (
          <GroupConstraintSelect dimension={dimension} type={groupChildConstraint} />
        ) : (
          <PopupList
            value={fixedHugSelectOption ?? undefined}
            options={fixedHugFillOptions}
            onSubmitValue={onSubmitFixedHug}
            controlStyles={fixedHugControlStyles}
          />
        )}
      </UIGridRow>
    )
  },
)

DimensionRow.displayName = 'DimensionRow'

const GroupConstraintSelect = React.memo(
  ({
    dimension,
    type,
  }: {
    dimension: LayoutPinnedProp
    type: GroupChildConstraintOptionType | 'mixed'
  }) => {
    const dispatch = useDispatch()
    const colorTheme = useColorTheme()

    const editorRef = useRefEditorState((store) => ({
      selectedViews: store.editor.selectedViews,
      allElementProps: store.editor.allElementProps,
    }))

    function optionFromType(value: GroupChildConstraintOptionType | 'mixed') {
      switch (value) {
        case 'constrained':
          return groupChildConstraintOptionValues.constrained
        case 'not-constrained':
          return groupChildConstraintOptionValues.notConstrained
        case 'mixed':
          return groupChildConstraintOptionValues.mixed
        default:
          assertNever(value)
      }
    }

    const listValue = React.useMemo(() => {
      return optionFromType(type)
    }, [type])

    const mainColor = React.useMemo(() => {
      switch (type) {
        case 'constrained':
          return colorTheme.fg0.value
        case 'mixed':
          return colorTheme.fg6Opacity50.value
        case 'not-constrained':
          return colorTheme.subduedForeground.value
        default:
          assertNever(type)
      }
    }, [type, colorTheme])

    const onSubmitValue = React.useCallback(
      (option: SelectOption) => {
        return setGroupChildConstraint(
          dispatch,
          option.value,
          editorRef.current.selectedViews,
          editorRef.current.allElementProps,
          dimension,
        )
      },
      [dispatch, editorRef, dimension],
    )

    return (
      <PopupList
        id={`group-child-resize-${dimension}`}
        onSubmitValue={onSubmitValue}
        value={listValue}
        options={groupChildConstraintOptions}
        style={{
          position: 'relative',
          fontSize: listValue.value === 'not-constrained' ? 9 : 'inherit',
        }}
        containerMode={type === 'constrained' ? 'default' : 'showBorderOnHover'}
        controlStyles={{
          ...getControlStyles('simple'),
          mainColor: mainColor,
        }}
      />
    )
  },
)

GroupConstraintSelect.displayName = 'GroupConstraintSelect'

function strategyForChangingFillFixedHugType(
  fixedValue: number,
  axis: Axis,
  mode: FixedHugFillMode,
  otherAxisSetToFill: boolean,
): Array<InspectorStrategy> {
  switch (mode) {
    case 'fill':
      return setPropFillStrategies(axis, 'default', otherAxisSetToFill)
    case 'hug':
      return setPropHugStrategies(axis)
    case 'fixed':
    case 'detected':
    case 'computed':
    case 'hug-group':
      return setPropFixedStrategies('always', axis, cssNumber(fixedValue, null))
    default:
      assertNever(mode)
  }
}

function pickFixedValue(value: FixedHugFill): CSSNumber | undefined {
  switch (value.type) {
    case 'computed':
    case 'detected':
    case 'fixed':
    case 'fill':
    case 'hug-group':
      return value.value
    case 'hug':
      return undefined
    default:
      assertNever(value)
  }
}

function pickNumberType(value: FixedHugFill | null): CSSNumberType {
  if (value?.type === 'fixed') {
    return 'AnyValid'
  }
  if (value?.type === 'fill') {
    return value.value.unit === '%' ? 'Percent' : 'Unitless'
  }
  return 'Unitless'
}

function isNumberInputEnabled(value: FixedHugFill | null): boolean {
  return value?.type === 'fixed' || value?.type === 'fill' || value?.type === 'hug-group'
}

const anySelectedElementGroupOrChildOfGroup = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  selectedViewsSelector,
  (metadata, pathTrees, selectedViews): boolean => {
    function elementOrAnyChildGroup(path: ElementPath) {
      return (
        // is the element a Group
        treatElementAsGroupLike(metadata, path) ||
        // or is the parent a group
        treatElementAsGroupLike(metadata, EP.parentPath(path))
      )
    }
    return selectedViews.some(elementOrAnyChildGroup)
  },
)

const allElementsAreGroupChildren = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  selectedViewsSelector,
  (metadata, pathTrees, selectedViews): boolean => {
    if (selectedViews.length === 0) {
      return false
    }
    function elementOrAnyChildGroup(path: ElementPath) {
      return treatElementAsGroupLike(metadata, EP.parentPath(path))
    }
    return selectedViews.every(elementOrAnyChildGroup)
  },
)

function checkGroupChildConstraint(
  dimension: LayoutPinnedProp,
  selectedViews: ElementPath[],
  allElementProps: AllElementProps,
): GroupChildConstraintOptionType | 'mixed' {
  const constrained = selectedViews.filter((path) =>
    getSafeGroupChildConstraintsArray(allElementProps, path).includes(dimension),
  ).length
  if (constrained === selectedViews.length) {
    return 'constrained'
  } else if (constrained === 0) {
    return 'not-constrained'
  } else {
    return 'mixed'
  }
}

const groupChildConstraintOptionValues = {
  constrained: groupChildConstraintOption('constrained'),
  notConstrained: groupChildConstraintOption('not-constrained'),
  mixed: { value: 'mixed', label: MixedPlaceholder },
}

const groupChildConstraintOptions: Array<SelectOption> = [
  groupChildConstraintOptionValues.constrained,
  groupChildConstraintOptionValues.notConstrained,
]

type GroupChildConstraintOptionType = 'constrained' | 'not-constrained'

function groupChildConstraintOption(type: GroupChildConstraintOptionType): SelectOption {
  switch (type) {
    case 'constrained':
      return { value: 'constrained', label: 'Fixed' }
    case 'not-constrained':
      return { value: 'not-constrained', label: 'Scale with parent' }
    default:
      assertNever(type)
  }
}

export function getSafeGroupChildConstraintsArray(
  allElementProps: AllElementProps,
  path: ElementPath,
): LayoutPinnedProp[] {
  const value = allElementProps[EP.toString(path)]?.['data-constraints'] ?? []
  if (!Array.isArray(value)) {
    return []
  }
  return value.filter((v) => typeof v === 'string' && isLayoutPinnedProp(v))
}

export type ConstraintsMode = 'add' | 'remove'

function setGroupChildConstraint(
  dispatch: EditorDispatch,
  option: GroupChildConstraintOptionType | 'toggle',
  selectedViews: ElementPath[],
  allElementProps: AllElementProps,
  dimension: LayoutPinnedProp,
) {
  function getMode(): ConstraintsMode {
    if (option !== 'toggle') {
      return option === 'constrained' ? 'add' : 'remove'
    }
    const notAllContainDimension = selectedViews.some(
      (path) => !getSafeGroupChildConstraintsArray(allElementProps, path).includes(dimension),
    )
    return notAllContainDimension ? 'add' : 'remove'
  }
  const mode = getMode()

  const prop = PP.create('data-constraints')

  const actions: EditorAction[] = mapDropNulls((path) => {
    const constraints = getSafeGroupChildConstraintsArray(allElementProps, path)
    const newProps = makeUpdatedConstraintsPropArray(constraints, mode, dimension)
    return newProps.length === 0
      ? unsetProperty(path, prop)
      : setProp_UNSAFE(path, prop, jsExpressionValue(newProps, emptyComments))
  }, selectedViews)

  dispatch(actions)
}

export function makeUpdatedConstraintsPropArray(
  constraints: LayoutPinnedProp[],
  mode: ConstraintsMode,
  dimension: LayoutPinnedProp,
): LayoutPinnedProp[] {
  const newProps = new Set(constraints)
  switch (mode) {
    case 'add':
      newProps.add(dimension)
      break
    case 'remove':
      newProps.delete(dimension)
      break
    default:
      assertNever(mode)
  }

  if (mode === 'add') {
    switch (dimension) {
      case 'width':
        maybeRemoveOldestComplement(newProps, constraints, ['left', 'right'])
        break
      case 'height':
        maybeRemoveOldestComplement(newProps, constraints, ['top', 'bottom'])
        break
      case 'top':
        maybeRemoveComplementaryDimension(newProps, 'bottom', 'height')
        break
      case 'left':
        maybeRemoveComplementaryDimension(newProps, 'right', 'width')
        break
      case 'bottom':
        maybeRemoveComplementaryDimension(newProps, 'top', 'height')
        break
      case 'right':
        maybeRemoveComplementaryDimension(newProps, 'left', 'width')
        break
      default:
        assertNever(dimension)
    }
  }

  return Array.from(newProps)
}

function maybeRemoveOldestComplement(
  set: Set<LayoutPinnedProp>,
  constraints: LayoutPinnedProp[],
  complements: [LayoutPinnedProp, LayoutPinnedProp],
) {
  if (set.has(complements[0]) && set.has(complements[1])) {
    const oldest =
      constraints.indexOf(complements[0]) < constraints.indexOf(complements[1])
        ? complements[0]
        : complements[1]
    set.delete(oldest)
  }
}

function maybeRemoveComplementaryDimension(
  set: Set<LayoutPinnedProp>,
  complement: LayoutPinnedProp,
  dimensionToDelete: LayoutPinnedProp,
) {
  if (set.has(complement)) {
    set.delete(dimensionToDelete)
  }
}
