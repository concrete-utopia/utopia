/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import createCachedSelector from 're-reselect'
import React from 'react'
import { createSelector } from 'reselect'
import { FlexCol } from 'utopia-api'
import type { LayoutPinnedPropIncludingCenter } from '../../core/layout/layout-helpers-new'
import { type LayoutPinnedProp } from '../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { mapDropNulls, safeIndex, strictEvery } from '../../core/shared/array-utils'
import * as EP from '../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { emptyComments, jsExpressionValue } from '../../core/shared/element-template'
import { optionalMap } from '../../core/shared/optional-utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { intersection } from '../../core/shared/set-utils'
import { assertNever } from '../../core/shared/utils'
import { NumberInput, PopupList, useColorTheme, UtopiaTheme } from '../../uuiui'
import type { SelectOption } from '../../uuiui-deps'
import { getControlStyles, InspectorRowHoverCSS } from '../../uuiui-deps'
import { MixedPlaceholder } from '../../uuiui/inputs/base-input'
import {
  convertGroupToFrameCommands,
  getInstanceForGroupToFrameConversion,
  isConversionForbidden,
} from '../canvas/canvas-strategies/strategies/group-conversion-helpers'
import { treatElementAsGroupLike } from '../canvas/canvas-strategies/strategies/group-helpers'
import { notice } from '../common/notice'
import type { EditorAction, EditorDispatch } from '../editor/action-types'
import {
  applyCommandsAction,
  setProp_UNSAFE,
  showToast,
  unsetProperty,
} from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import type { AllElementProps } from '../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import type { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import { fixedSizeDimensionHandlingText } from '../text-editor/text-handling'
import type { CSSNumber, CSSNumberType, UnknownOrEmptyInput } from './common/css-utils'
import { cssNumber } from './common/css-utils'
import type { FramePinsInfo } from './common/layout-property-path-hooks'
import { PinControl } from './controls/pin-control'
import {
  metadataSelector,
  pathTreesSelector,
  selectedViewsSelector,
  useNonRoundedComputedSizeRef,
} from './inpector-selectors'
import type { Axis, FixedHugFill, FixedHugFillMode } from './inspector-common'
import {
  detectFillHugFixedStateMultiselect,
  getConstraintsIncludingImplicitForElement,
  getFixedFillHugOptionsForElement,
  isFixedHugFillEqual,
} from './inspector-common'
import {
  setPropFillStrategies,
  setPropFixedSizeStrategies,
  setPropHugStrategies,
} from './inspector-strategies/inspector-strategies'
import type { InspectorStrategy } from './inspector-strategies/inspector-strategy'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import type { GridRowVariant } from './widgets/ui-grid-row'
import { UIGridRow } from './widgets/ui-grid-row'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'

export const FillFixedHugControlId = (segment: 'width' | 'height'): string =>
  `hug-fixed-fill-${segment}`

export const FillContainerLabel = 'Fill container' as const
export const FixedLabel = 'Fixed' as const
export const ScaledLabel = 'Scaled' as const
export const HugContentsLabel = 'Hug contents' as const
export const SqueezeContentsLabel = 'Squeeze' as const
export const CollapsedLabel = 'Collapsed' as const
export const HugGroupContentsLabel = 'Hug contents' as const
export const ComputedLabel = 'Computed' as const
export const DetectedLabel = 'Detected' as const

export function selectOptionLabel(mode: FixedHugFillMode): string {
  switch (mode) {
    case 'fill':
      return FillContainerLabel
    case 'fixed':
      return FixedLabel
    case 'scaled':
      return ScaledLabel
    case 'hug':
      return HugContentsLabel
    case 'squeeze':
      return SqueezeContentsLabel
    case 'collapsed':
      return CollapsedLabel
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

export function selectOptionIconType(
  mode: FixedHugFillMode,
  dimension: 'width' | 'height',
): string {
  switch (mode) {
    case 'fill':
      return `fill-${dimension}`
    case 'fixed':
    case 'scaled': // TODO: needs separate icon
      return `fixed-${dimension}`
    case 'hug':
    case 'squeeze':
    case 'collapsed':
      return `hug-${dimension}` // TODO: needs separate icon
    case 'hug-group':
      return `hug-${dimension}`
    case 'computed':
      return `fixed-${dimension}`
    case 'detected':
      return `fixed-${dimension}`
    default:
      assertNever(mode)
  }
}

const selectOption =
  (dimension: 'width' | 'height') =>
  (mode: FixedHugFillMode): SelectOption => {
    return {
      value: mode,
      label: selectOptionLabel(mode),
      icon: {
        category: 'layout/commands',
        type: selectOptionIconType(mode, dimension),
      },
    }
  }

const fixedHugFillOptionsSelector = createCachedSelector(
  metadataSelector,
  pathTreesSelector,
  selectedViewsSelector,
  (_: MetadataSubstate, dimension: 'width' | 'height') => dimension,
  (metadata, pathTrees, selectedViews, dimension) => {
    const applicableOptions: Array<FixedHugFillMode> = [
      ...intersection(
        selectedViews.map((selectedView) =>
          getFixedFillHugOptionsForElement(metadata, pathTrees, selectedView),
        ),
      ),
    ]

    return applicableOptions.map(selectOption(dimension))
  },
)((_, dimension: 'width' | 'height') => dimension)

function useOnSubmitFixedFillHugType(dimension: 'width' | 'height') {
  const axis = dimension === 'width' ? 'horizontal' : 'vertical'

  const dispatch = useDispatch()
  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const currentValueRef = useNonRoundedComputedSizeRef(dimension)

  const fillsOtherAxisRef = useRefEditorState(
    (store) =>
      detectFillHugFixedStateMultiselect(
        axis === 'vertical' ? 'horizontal' : 'vertical',
        metadataSelector(store),
        selectedViewsSelector(store),
      ).fixedHugFill?.type === 'fill',
  )

  const onSubmitFixedFillHugType = React.useMemo(() => {
    return ({ value: anyValue }: SelectOption) => {
      const value = anyValue as FixedHugFillMode

      const currentComputedValue = currentValueRef.current ?? 0

      const firstSelectedView = safeIndex(selectedViewsRef.current, 0)
      if (firstSelectedView != null) {
        // changing value for a group means converting its contract
        if (treatElementAsGroupLike(metadataRef.current, firstSelectedView) && value === 'fixed') {
          const conversion = getInstanceForGroupToFrameConversion(
            metadataRef.current,
            elementPathTreeRef.current,
            allElementPropsRef.current,
            firstSelectedView,
          )
          if (isConversionForbidden(conversion)) {
            dispatch([showToast(notice(conversion.reason, 'ERROR'))])
          } else {
            dispatch([
              applyCommandsAction(
                convertGroupToFrameCommands(
                  metadataRef.current,
                  elementPathTreeRef.current,
                  allElementPropsRef.current,
                  firstSelectedView,
                ),
              ),
              showToast(notice('Converted to frame')),
            ])
          }
          return
        }

        const valueToUse =
          dimension === 'width'
            ? fixedSizeDimensionHandlingText(
                metadataRef.current,
                elementPathTreeRef.current,
                firstSelectedView,
                currentComputedValue,
              )
            : currentComputedValue
        const strategy = strategyForChangingFillFixedHugType(
          metadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
          valueToUse,
          axis,
          value,
          fillsOtherAxisRef.current,
        )
        executeFirstApplicableStrategy(
          dispatch,

          strategy,
        )
      }
    }
  }, [
    dimension,
    axis,
    allElementPropsRef,
    dispatch,
    metadataRef,
    elementPathTreeRef,
    selectedViewsRef,
    currentValueRef,
    fillsOtherAxisRef,
  ])

  return onSubmitFixedFillHugType
}

export const getFixedHugDropdownId = (dimension: 'width' | 'height') =>
  `fixed-hug-dropdown-${dimension}`

export const FixedHugDropdown = React.memo((props: { dimension: 'width' | 'height' }) => {
  const { dimension } = props

  const currentValue = useEditorState(
    Substores.metadata,
    (store) =>
      detectFillHugFixedStateMultiselect(
        dimension === 'width' ? 'horizontal' : 'vertical',
        metadataSelector(store),
        selectedViewsSelector(store),
      ),
    'FillHugFixedControl currentValue',
    isFixedHugFillEqual,
  )

  const fixedHugFillOptions = useEditorState(
    Substores.metadata,
    (store) => fixedHugFillOptionsSelector(store, dimension),
    'FixedHugDropdown fixedHugFillOptions',
  )

  const onSubmitFixedFillHugType = useOnSubmitFixedFillHugType(dimension)

  return (
    <PopupList
      id={getFixedHugDropdownId(dimension)}
      value={optionalMap(selectOption(dimension), currentValue.fixedHugFill?.type) ?? undefined}
      options={fixedHugFillOptions}
      onSubmitValue={onSubmitFixedFillHugType}
      controlStyles={getControlStyles(currentValue.controlStatus)}
      style={{ background: 'transparent' }}
    />
  )
})
FixedHugDropdown.displayName = 'FixedHugDropdown'

// TODO DELETE ME once 'Simplified Layout Section' is deleted
export const GroupConstraintSelect = React.memo(
  ({ dimension }: { dimension: LayoutPinnedProp }) => {
    const dispatch = useDispatch()
    const colorTheme = useColorTheme()

    const constraintType = useEditorState(
      Substores.metadata,
      (store) =>
        checkGroupChildConstraint(
          dimension,
          store.editor.selectedViews,
          store.editor.jsxMetadata,
          store.editor.allElementProps,
        ),
      'GroupConstraintSelect constraintType',
    )

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
      return optionFromType(constraintType)
    }, [constraintType])

    const mainColor = React.useMemo(() => {
      switch (constraintType) {
        case 'constrained':
          return colorTheme.fg0.value
        case 'mixed':
          return colorTheme.fg6Opacity50.value
        case 'not-constrained':
          return colorTheme.subduedForeground.value
        default:
          assertNever(constraintType)
      }
    }, [constraintType, colorTheme])

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
        containerMode={constraintType === 'constrained' ? 'default' : 'showBorderOnHover'}
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
  metadata: ElementInstanceMetadataMap,
  selectedElements: ElementPath[],
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  fixedValue: number,
  axis: Axis,
  mode: FixedHugFillMode,
  otherAxisSetToFill: boolean,
): Array<InspectorStrategy> {
  switch (mode) {
    case 'fill':
      return setPropFillStrategies(metadata, selectedElements, axis, 'default', otherAxisSetToFill)
    case 'hug':
    case 'squeeze':
    case 'collapsed':
      return setPropHugStrategies(metadata, selectedElements, elementPathTree, axis)
    case 'fixed':
    case 'scaled':
    case 'detected':
    case 'computed':
    case 'hug-group':
      return setPropFixedSizeStrategies(
        'always',
        metadata,
        selectedElements,
        axis,
        cssNumber(fixedValue, null),
      )
    default:
      assertNever(mode)
  }
}

function pickFixedValue(value: FixedHugFill): CSSNumber | undefined {
  switch (value.type) {
    case 'computed':
    case 'detected':
    case 'fixed':
    case 'scaled':
    case 'fill':
    case 'hug-group':
      return value.value
    case 'hug':
    case 'squeeze':
    case 'collapsed':
      return undefined
    default:
      assertNever(value)
  }
}

function pickNumberType(value: FixedHugFill | null): CSSNumberType {
  if (value?.type === 'fixed') {
    return 'AnyValid'
  }
  if (value?.type === 'fill' || value?.type === 'scaled') {
    return value.value.unit === '%' ? 'Percent' : 'Unitless'
  }
  return 'Unitless'
}

function isNumberInputEnabled(value: FixedHugFill | null): boolean {
  return (
    value?.type === 'fixed' ||
    value?.type === 'fill' ||
    value?.type === 'hug-group' ||
    value?.type === 'scaled'
  )
}

export const anySelectedElementGroupOrChildOfGroup = createSelector(
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

export const allElementsAreGroupChildren = createSelector(
  metadataSelector,
  selectedViewsSelector,
  (metadata, selectedViews): boolean => {
    return strictEvery(selectedViews, (path: ElementPath) => {
      return treatElementAsGroupLike(metadata, EP.parentPath(path))
    })
  },
)

export const allElementsArePositionedAbsolutelySelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  (metadata, selectedViews): boolean => {
    return strictEvery(selectedViews, (path) => {
      return MetadataUtils.isPositionAbsolute(
        MetadataUtils.findElementByElementPath(metadata, path),
      )
    })
  },
)

function checkGroupChildConstraint(
  dimension: LayoutPinnedProp,
  selectedViews: ElementPath[],
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): GroupChildConstraintOptionType | 'mixed' {
  const constrained = selectedViews.filter((path) =>
    getConstraintsIncludingImplicitForElement(
      metadata,
      allElementProps,
      path,
      'only-explicit-constraints',
    ).includes(dimension),
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

export type ConstraintsMode = 'add' | 'remove'

export function setGroupChildConstraint(
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
      (path) =>
        !getConstraintsIncludingImplicitForElement(
          {},
          allElementProps,
          path,
          'only-explicit-constraints',
        ).includes(dimension),
    )
    return notAllContainDimension ? 'add' : 'remove'
  }
  const mode = getMode()

  const prop = PP.create('data-constraints')

  const actions: EditorAction[] = mapDropNulls((path) => {
    const constraints = getConstraintsIncludingImplicitForElement(
      {},
      allElementProps,
      path,
      'only-explicit-constraints',
    )
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
