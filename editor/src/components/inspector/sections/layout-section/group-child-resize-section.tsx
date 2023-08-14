import React from 'react'
import * as EP from '../../../../core/shared/element-path'
import { emptyComments, jsExpressionValue } from '../../../../core/shared/element-template'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import { assertNever } from '../../../../core/shared/utils'
import { FlexRow, InspectorSubsectionHeader, PopupList, useColorTheme } from '../../../../uuiui'
import type { EditorAction } from '../../../editor/action-types'
import { setProp_UNSAFE, unsetProperty } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import type { AllElementProps } from '../../../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import { getControlStyles } from '../../common/control-status'
import type { SelectOption } from '../../controls/select-control'
import { uniqBy } from '../../../../core/shared/array-utils'

type ConstraintOptionType = 'constrained' | 'not-constrained'
type Dimension = 'width' | 'height'

export function constraintOption(type: ConstraintOptionType): SelectOption {
  switch (type) {
    case 'constrained':
      return { value: 'constrained', label: 'Constrained' }
    case 'not-constrained':
      return { value: 'not-constrained', label: 'Not constrained' }
    default:
      assertNever(type)
  }
}

const Constrained = constraintOption('constrained')
const NotConstrained = constraintOption('not-constrained')
const Mixed = { value: 'mixed', label: 'Mixed' }

const Options: Array<SelectOption> = [Constrained, NotConstrained]

function getSafeConstraintsArray(allElementProps: AllElementProps, path: ElementPath): any[] {
  const value = allElementProps[EP.toString(path)]?.['data-constraints'] ?? []
  if (!Array.isArray(value)) {
    return []
  }
  return value
}

function checkConstraint(
  dimension: Dimension,
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

export const GroupChildResizeSection = React.memo(() => {
  const constraints = useEditorState(
    Substores.metadata,
    (store) => ({
      width: checkConstraint('width', store.editor.selectedViews, store.editor.allElementProps),
      height: checkConstraint('height', store.editor.selectedViews, store.editor.allElementProps),
    }),
    '',
  )

  return (
    <>
      <InspectorSubsectionHeader>
        <FlexRow
          style={{
            flexGrow: 1,
            gap: 8,
            height: 42,
          }}
        >
          <span>Resizing</span>
        </FlexRow>
      </InspectorSubsectionHeader>
      <div
        style={{
          paddingLeft: 10,
          paddingRight: 0,
          paddingBottom: 10,
          display: 'flex',
          gap: 8,
          flexDirection: 'column',
        }}
      >
        <ConstraintSelect label='Width' dimension='width' type={constraints.width} />
        <ConstraintSelect label='Height' dimension='height' type={constraints.height} />
      </div>
    </>
  )
})

const ConstraintSelect = React.memo(
  ({
    label,
    dimension,
    type,
  }: {
    label: string
    dimension: Dimension
    type: ConstraintOptionType | 'mixed'
  }) => {
    const dispatch = useDispatch()
    const colorTheme = useColorTheme()

    const editorRef = useRefEditorState((store) => ({
      selectedViews: store.editor.selectedViews,
      allElementProps: store.editor.allElementProps,
    }))

    const setConstraint = React.useCallback(
      (option: SelectOption) => {
        const optionValue: ConstraintOptionType = option.value
        const prop = PP.create('data-constraints')

        const actions: EditorAction[] = editorRef.current.selectedViews.map((path) => {
          const constraints = getSafeConstraintsArray(editorRef.current.allElementProps, path)
          const newProps = makeNewProps(
            dimension,
            constraints,
            optionValue === 'constrained' ? 'add' : 'remove',
          )
          const uniqueNewProps = uniqBy(newProps, (a, b) => a === b)
          return uniqueNewProps.length === 0
            ? unsetProperty(path, prop)
            : setProp_UNSAFE(path, prop, jsExpressionValue(uniqueNewProps, emptyComments))
        })

        dispatch(actions)
      },
      [dispatch, editorRef, dimension],
    )

    function valueToOption(value: ConstraintOptionType | 'mixed') {
      return value === 'constrained'
        ? Constrained
        : value === 'not-constrained'
        ? NotConstrained
        : Mixed
    }

    const value = React.useMemo(() => {
      return valueToOption(type)
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

    return (
      <FlexRow>
        <span style={{ flex: 1 }}>{label}</span>
        <div style={{ width: 150 }}>
          <PopupList
            onSubmitValue={setConstraint}
            value={value}
            options={Options}
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

function makeNewProps(dimension: Dimension, current: any[], mode: 'add' | 'remove'): any[] {
  switch (mode) {
    case 'add':
      return [...current, dimension]
    case 'remove':
      return current.filter((other) => other !== dimension)
    default:
      assertNever(mode)
  }
}
