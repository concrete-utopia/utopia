import React from 'react'
import type { LayoutPinnedProp } from '../../../../core/layout/layout-helpers-new'
import { mapDropNulls, uniqBy } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import { emptyComments, jsExpressionValue } from '../../../../core/shared/element-template'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import { assertNever } from '../../../../core/shared/utils'
import { FlexRow, InspectorSubsectionHeader, PopupList, useColorTheme } from '../../../../uuiui'
import type { EditorAction, EditorDispatch } from '../../../editor/action-types'
import { setProp_UNSAFE, unsetProperty } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import type { AllElementProps } from '../../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { getControlStyles } from '../../common/control-styles'
import { PinControl } from '../../controls/pin-control'
import type { SelectOption } from '../../controls/select-control'
import type { FramePinsInfo } from '../../common/layout-property-path-hooks'

type ConstraintOptionType = 'constrained' | 'not-constrained'

function constraintOption(type: ConstraintOptionType): SelectOption {
  switch (type) {
    case 'constrained':
      return { value: 'constrained', label: 'Constrained' }
    case 'not-constrained':
      return { value: 'not-constrained', label: 'Not constrained' }
    default:
      assertNever(type)
  }
}

const optionValues = {
  constrained: constraintOption('constrained'),
  notConstrained: constraintOption('not-constrained'),
  mixed: { value: 'mixed', label: 'Mixed' },
}

const options: Array<SelectOption> = [optionValues.constrained, optionValues.notConstrained]

function getSafeConstraintsArray(allElementProps: AllElementProps, path: ElementPath): any[] {
  const value = allElementProps[EP.toString(path)]?.['data-constraints'] ?? []
  if (!Array.isArray(value)) {
    return []
  }
  return value
}

function checkConstraint(
  dimension: LayoutPinnedProp,
  selectedViews: ElementPath[],
  allElementProps: AllElementProps,
): ConstraintOptionType | 'mixed' {
  const constrained = selectedViews.filter((path) =>
    getSafeConstraintsArray(allElementProps, path).includes(dimension),
  ).length
  return constrained === selectedViews.length
    ? 'constrained'
    : constrained === 0
    ? 'not-constrained'
    : 'mixed'
}

function makeNewProps(dimension: LayoutPinnedProp, current: any[], mode: 'add' | 'remove'): any[] {
  switch (mode) {
    case 'add':
      return [...current, dimension]
    case 'remove':
      return current.filter((other) => other !== dimension)
    default:
      assertNever(mode)
  }
}

function setConstraint(
  dispatch: EditorDispatch,
  option: ConstraintOptionType | 'toggle',
  selectedViews: ElementPath[],
  allElementProps: AllElementProps,
  dimension: LayoutPinnedProp,
) {
  function getMode(): 'add' | 'remove' {
    if (option !== 'toggle') {
      return option === 'constrained' ? 'add' : 'remove'
    }
    const notAllContainDimension = selectedViews.some(
      (path) => !getSafeConstraintsArray(allElementProps, path).includes(dimension),
    )
    return notAllContainDimension ? 'add' : 'remove'
  }
  const mode = getMode()

  const prop = PP.create('data-constraints')

  const actions: EditorAction[] = mapDropNulls((path) => {
    const constraints = getSafeConstraintsArray(allElementProps, path)
    const newProps = makeNewProps(dimension, constraints, mode)
    const uniqueNewProps = uniqBy(newProps, (a, b) => a === b)
    return uniqueNewProps.length === 0
      ? unsetProperty(path, prop)
      : setProp_UNSAFE(path, prop, jsExpressionValue(uniqueNewProps, emptyComments))
  }, selectedViews)

  dispatch(actions)
}

export const GroupChildResizeSection = React.memo(() => {
  const dispatch = useDispatch()

  const editorRef = useRefEditorState((store) => ({
    selectedViews: store.editor.selectedViews,
    allElementProps: store.editor.allElementProps,
  }))

  const constraints = useEditorState(
    Substores.metadata,
    (store) => ({
      width: checkConstraint('width', store.editor.selectedViews, store.editor.allElementProps),
      height: checkConstraint('height', store.editor.selectedViews, store.editor.allElementProps),
      top: checkConstraint('top', store.editor.selectedViews, store.editor.allElementProps),
      left: checkConstraint('left', store.editor.selectedViews, store.editor.allElementProps),
      bottom: checkConstraint('bottom', store.editor.selectedViews, store.editor.allElementProps),
      right: checkConstraint('right', store.editor.selectedViews, store.editor.allElementProps),
    }),
    'GroupChildResizeSection constraints',
  )

  const handlePinMouseDown = React.useCallback(
    (dimension: LayoutPinnedProp) => {
      return setConstraint(
        dispatch,
        'toggle',
        editorRef.current.selectedViews,
        editorRef.current.allElementProps,
        dimension,
      )
    },
    [dispatch, editorRef],
  )

  const framePoints: FramePinsInfo = React.useMemo(() => {
    const ignore = { isPrimaryPosition: false, isRelativePosition: false }
    return {
      left: {
        isPrimaryPosition: constraints.left !== 'not-constrained',
        isRelativePosition: false,
      },
      top: {
        isPrimaryPosition: constraints.top !== 'not-constrained',
        isRelativePosition: false,
      },
      bottom: {
        isPrimaryPosition: constraints.bottom !== 'not-constrained',
        isRelativePosition: false,
      },
      right: {
        isPrimaryPosition: constraints.right !== 'not-constrained',
        isRelativePosition: false,
      },
      width: ignore,
      height: ignore,
      centerX: ignore,
      centerY: ignore,
    }
  }, [constraints])

  return (
    <>
      <InspectorSubsectionHeader>
        <FlexRow style={{ flexGrow: 1, gap: 8, height: 42 }}>
          <span>Resizing</span>
        </FlexRow>
      </InspectorSubsectionHeader>
      <div
        style={{ display: 'flex', gap: 10, paddingLeft: 10, paddingRight: 0, paddingBottom: 10 }}
      >
        <PinControl
          handlePinMouseDown={handlePinMouseDown}
          framePoints={framePoints}
          controlStatus='simple'
          exclude={{ center: true }}
          name='group-child-controls'
        />
        <div style={{ flex: 1, display: 'flex', flexDirection: 'column', gap: 8 }}>
          <ConstraintSelect label='Width' dimension='width' type={constraints.width} />
          <ConstraintSelect label='Height' dimension='height' type={constraints.height} />
        </div>
      </div>
    </>
  )
})

GroupChildResizeSection.displayName = 'GroupChildResizeSection'

const ConstraintSelect = React.memo(
  ({
    label,
    dimension,
    type,
  }: {
    label: string
    dimension: LayoutPinnedProp
    type: ConstraintOptionType | 'mixed'
  }) => {
    const dispatch = useDispatch()
    const colorTheme = useColorTheme()

    const editorRef = useRefEditorState((store) => ({
      selectedViews: store.editor.selectedViews,
      allElementProps: store.editor.allElementProps,
    }))

    function optionFromType(value: ConstraintOptionType | 'mixed') {
      return value === 'constrained'
        ? optionValues.constrained
        : value === 'not-constrained'
        ? optionValues.notConstrained
        : optionValues.mixed
    }

    const listValue = React.useMemo(() => {
      return optionFromType(type)
    }, [type])

    const mainColor = React.useMemo(() => {
      switch (type) {
        case 'constrained':
          return colorTheme.brandNeonPink.value
        case 'mixed':
          return colorTheme.primary.value
        case 'not-constrained':
          return colorTheme.subduedForeground.value
        default:
          assertNever(type)
      }
    }, [type, colorTheme])

    const onSubmitValue = React.useCallback(
      (option: SelectOption) => {
        return setConstraint(
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
      <FlexRow>
        <span style={{ flex: 1 }}>{label}</span>
        <div style={{ width: 120 }}>
          <PopupList
            id={`group-child-resize-${dimension}`}
            onSubmitValue={onSubmitValue}
            value={listValue}
            options={options}
            style={{ position: 'relative', left: -8 }}
            containerMode={type === 'constrained' ? 'default' : 'showBorderOnHover'}
            controlStyles={{
              ...getControlStyles('simple'),
              mainColor: mainColor,
            }}
          />
        </div>
      </FlexRow>
    )
  },
)

ConstraintSelect.displayName = 'ConstraintSelect'
