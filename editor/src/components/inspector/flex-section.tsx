/** @jsxRuntime classic */
/** @jsxFrag React.Fragment */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { createSelector } from 'reselect'
import { when } from '../../utils/react-conditionals'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { AddRemoveLayoutSystemControl } from './add-remove-layout-system-control'
import { FlexDirectionToggle } from './flex-direction-control'
import { selectedViewsSelector, metadataSelector } from './inpector-selectors'
import { NineBlockControl } from './nine-block-controls'
import { UIGridRow } from './widgets/ui-grid-row'
import { SpacedPackedControl } from './spaced-packed-control'
import { ThreeBarControl } from './three-bar-control'
import { FlexGapControl } from './sections/layout-section/flex-container-subsection/flex-container-controls'
import { FlexContainerControls } from './sections/layout-section/flex-container-subsection/flex-container-subsection'
import { FlexCol } from 'utopia-api'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { strictEvery } from '../../core/shared/array-utils'
import { useDispatch } from '../editor/store/dispatch-context'
import type { DetectedLayoutSystem } from 'utopia-shared/src/types'
import { NO_OP } from '../../core/shared/utils'
import { assertNever } from '../../core/shared/utils'
import {
  FlexRow,
  Icons,
  InspectorSectionIcons,
  NumberInput,
  SquareButton,
  StringInput,
  Subdued,
  Tooltip,
} from '../../uuiui'
import type {
  CSSKeyword,
  CSSNumber,
  GridAutoFlow,
  GridCSSKeyword,
  GridCSSMinmax,
  GridDiscreteDimension,
  UnknownOrEmptyInput,
  ValidGridDimensionKeyword,
} from './common/css-utils'
import {
  cssKeyword,
  cssNumber,
  cssNumberToString,
  gridAutoFlowIcon,
  gridCSSKeyword,
  gridCSSNumber,
  isCSSKeyword,
  isCSSNumber,
  isEmptyInputValue,
  isGridCSSKeyword,
  isGridCSSNumber,
  isGridCSSRepeat,
  isValidGridDimensionKeyword,
  parseCSSNumber,
  parseGridCSSMinmaxOrRepeat,
  printArrayGridDimensions,
  printGridDimension,
  type GridDimension,
} from './common/css-utils'
import { applyCommandsAction, transientActions } from '../editor/actions/action-creators'
import type { PropertyToUpdate } from '../canvas/commands/set-property-command'
import {
  propertyToDelete,
  propertyToSet,
  updateBulkProperties,
  setProperty,
} from '../canvas/commands/set-property-command'
import * as PP from '../../core/shared/property-path'
import type {
  GridAutoOrTemplateBase,
  GridContainerProperties,
  GridPosition,
} from '../../core/shared/element-template'
import {
  gridPositionValue,
  type ElementInstanceMetadata,
  type GridElementProperties,
} from '../../core/shared/element-template'
import {
  expandGridDimensions,
  removeGridTemplateDimensionAtIndex,
  replaceGridTemplateDimensionAtIndex,
  setGridPropsCommands,
} from '../canvas/canvas-strategies/strategies/grid-helpers'
import { type CanvasCommand } from '../canvas/commands/commands'
import type { DropdownMenuItem } from '../../uuiui/radix-components'
import {
  DropdownMenu,
  RadixSelect,
  regularDropdownMenuItem,
  regularRadixSelectOption,
  separatorRadixSelectOption,
} from '../../uuiui/radix-components'
import { useInspectorLayoutInfo, useInspectorStyleInfo } from './common/property-path-hooks'
import { NumberOrKeywordControl } from '../../uuiui/inputs/number-or-keyword-control'
import { optionalMap } from '../../core/shared/optional-utils'
import { cssNumberEqual } from '../canvas/controls/select-mode/controls-common'
import type { EditorAction } from '../editor/action-types'
import type { CanvasControlWithProps } from './common/inspector-atoms'
import type { SubduedGridGapControlProps } from '../canvas/controls/select-mode/subdued-grid-gap-controls'
import { SubduedGridGapControl } from '../canvas/controls/select-mode/subdued-grid-gap-controls'
import {
  useSetFocusedControlsHandlers,
  useSetHoveredControlsHandlers,
} from '../canvas/controls/select-mode/select-mode-hooks'
import type { Axis } from '../canvas/gap-utils'
import { isRight } from '../../core/shared/either'

const axisDropdownMenuButton = 'axisDropdownMenuButton'

function getLayoutSystem(
  layoutSystem: DetectedLayoutSystem | null | undefined,
): 'grid' | 'flex' | null {
  if (layoutSystem === 'grid' || layoutSystem === 'flex') {
    return layoutSystem
  }

  return null
}

export const layoutSystemSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  (metadata, selectedViews) => {
    const detectedLayoutSystems = selectedViews.map((path) =>
      getLayoutSystem(
        MetadataUtils.findElementByElementPath(metadata, path)?.specialSizeMeasurements
          .layoutSystemForChildren,
      ),
    )

    const allLayoutSystemsTheSame = strictEvery(
      detectedLayoutSystems,
      (e) => e === detectedLayoutSystems[0],
    )

    if (allLayoutSystemsTheSame) {
      return detectedLayoutSystems[0]
    }

    return null
  },
)

export const FlexSection = React.memo(() => {
  const layoutSystem = useEditorState(
    Substores.metadata,
    layoutSystemSelector,
    'FlexSection areAllElementsInFlexLayout',
  )

  const grid = useEditorState(
    Substores.metadata,
    (store) =>
      layoutSystem === 'grid' && store.editor.selectedViews.length === 1
        ? MetadataUtils.findElementByElementPath(
            store.editor.jsxMetadata,
            store.editor.selectedViews[0],
          )
        : null,
    'FlexSection grid',
  )

  return (
    <div>
      <AddRemoveLayoutSystemControl />
      {when(
        layoutSystem != null,
        <FlexCol css={{ gap: 10, paddingBottom: 10 }}>
          {when(
            layoutSystem === 'grid',
            <UIGridRow padded tall={false} variant={'<-------------1fr------------->'}>
              {grid != null ? (
                <div style={{ display: 'flex', flexDirection: 'column', gap: 10 }}>
                  <GapRowColumnControl />
                  <AutoFlowControl />
                  <TemplateDimensionControl axis={'column'} grid={grid} title='Columns' />
                  <TemplateDimensionControl axis={'row'} grid={grid} title='Rows' />
                </div>
              ) : null}
            </UIGridRow>,
          )}
          {when(
            layoutSystem === 'flex',
            <React.Fragment>
              <UIGridRow padded variant='<--1fr--><--1fr-->'>
                <UIGridRow padded={false} variant='<-------------1fr------------->'>
                  <NineBlockControl />
                  <ThreeBarControl />
                </UIGridRow>
                <FlexCol css={{ gap: 10 }}>
                  <FlexDirectionToggle />
                  <FlexContainerControls seeMoreVisible={true} />
                  <FlexGapControl />
                </FlexCol>
              </UIGridRow>
              <UIGridRow padded={false} variant='<-------------1fr------------->'>
                <SpacedPackedControl />
              </UIGridRow>
            </React.Fragment>,
          )}
        </FlexCol>,
      )}
    </div>
  )
})

const TemplateDimensionControl = React.memo(
  ({
    grid,
    axis,
    title,
  }: {
    grid: ElementInstanceMetadata
    axis: 'column' | 'row'
    title: string
  }) => {
    const dispatch = useDispatch()

    const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)

    const values = React.useMemo((): GridDimension[] => {
      // TODO: handle gridAutoRows/Cols too
      switch (axis) {
        case 'row': {
          const { gridTemplateRows } = grid.specialSizeMeasurements.containerGridPropertiesFromProps

          return gridTemplateRows?.type === 'DIMENSIONS' ? gridTemplateRows.dimensions : []
        }
        case 'column': {
          const { gridTemplateColumns } =
            grid.specialSizeMeasurements.containerGridPropertiesFromProps

          return gridTemplateColumns?.type === 'DIMENSIONS' ? gridTemplateColumns.dimensions : []
        }
        default:
          assertNever(axis)
      }
    }, [grid, axis])

    const template = React.useMemo(() => {
      const fromProps =
        axis === 'column'
          ? grid.specialSizeMeasurements.containerGridPropertiesFromProps.gridTemplateColumns
          : grid.specialSizeMeasurements.containerGridPropertiesFromProps.gridTemplateRows
      if (fromProps?.type === 'DIMENSIONS' && fromProps.dimensions.length === 0) {
        return { type: 'DIMENSIONS', dimensions: values }
      }
      return fromProps
    }, [grid, axis, values])

    const onUpdateDimension = React.useCallback(
      (index: number) => (newValue: GridDimension) => {
        if (template?.type !== 'DIMENSIONS') {
          return
        }
        const left = template.dimensions.slice(0, index)
        const right = template.dimensions.slice(index + 1)

        const newDimensions = [...left, newValue, ...right]

        dispatch([
          applyCommandsAction([
            setProperty(
              'always',
              grid.elementPath,
              PP.create('style', axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows'),
              printArrayGridDimensions(newDimensions),
            ),
          ]),
        ])
      },
      [template, dispatch, axis, grid],
    )

    const onUpdateNumberOrKeyword = React.useCallback(
      (index: number) =>
        (value: UnknownOrEmptyInput<CSSNumber | CSSKeyword<ValidGridDimensionKeyword>>) => {
          function getNewValue() {
            const gridValueAtIndex = values[index]
            if (isCSSNumber(value)) {
              const maybeUnit = isGridCSSNumber(gridValueAtIndex)
                ? gridValueAtIndex.value.unit
                : null
              return gridCSSNumber(
                cssNumber(value.value, value.unit ?? maybeUnit),
                gridValueAtIndex.areaName,
              )
            } else if (isCSSKeyword(value)) {
              return gridCSSKeyword(value, gridValueAtIndex.areaName)
            } else if (isEmptyInputValue(value)) {
              return gridCSSKeyword(cssKeyword('auto'), gridValueAtIndex.areaName)
            } else {
              return null
            }
          }
          const newValue = getNewValue()
          if (newValue == null) {
            return
          }

          onUpdateDimension(index)(newValue)
        },
      [values, onUpdateDimension],
    )

    const onRemove = React.useCallback(
      (index: number) => () => {
        if (template?.type !== 'DIMENSIONS') {
          return
        }

        const left = template.dimensions.slice(0, index)
        const right = template.dimensions.slice(index + 1)

        const newValues = [...left, ...right]

        let commands: CanvasCommand[] = [
          setProperty(
            'always',
            grid.elementPath,
            PP.create('style', axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows'),
            printArrayGridDimensions(newValues),
          ),
        ]

        // adjust the position of the elements if they need to be moved
        const adjustedGridTemplate = removeTemplateValueAtIndex(
          grid.specialSizeMeasurements.containerGridPropertiesFromProps,
          axis,
          index,
        )

        const gridIndex = index + 1 // grid boundaries are 1-based

        const children = MetadataUtils.getChildrenUnordered(metadataRef.current, grid.elementPath)
        for (const child of children) {
          let updated: Partial<GridElementProperties> = {
            ...child.specialSizeMeasurements.elementGridPropertiesFromProps,
          }

          function needsAdjusting(pos: GridPosition | null, bound: number) {
            return pos != null &&
              !isCSSKeyword(pos) &&
              pos.numericalPosition != null &&
              pos.numericalPosition >= bound
              ? pos.numericalPosition
              : null
          }

          const position = child.specialSizeMeasurements.elementGridPropertiesFromProps
          if (axis === 'column') {
            const adjustColumnStart = needsAdjusting(position.gridColumnStart, gridIndex)
            const adjustColumnEnd = needsAdjusting(position.gridColumnEnd, gridIndex + 1)
            if (adjustColumnStart != null) {
              updated.gridColumnStart = gridPositionValue(adjustColumnStart - 1)
            }
            if (adjustColumnEnd != null) {
              updated.gridColumnEnd = gridPositionValue(adjustColumnEnd - 1)
            }
          } else {
            const adjustRowStart = needsAdjusting(position.gridRowStart, gridIndex)
            const adjustRowEnd = needsAdjusting(position.gridRowEnd, gridIndex + 1)
            if (adjustRowStart != null) {
              updated.gridRowStart = gridPositionValue(adjustRowStart - 1)
            }
            if (adjustRowEnd != null) {
              updated.gridRowEnd = gridPositionValue(adjustRowEnd - 1)
            }
          }

          commands.push(...setGridPropsCommands(child.elementPath, adjustedGridTemplate, updated))
        }

        dispatch([applyCommandsAction(commands)])
      },
      [grid, dispatch, axis, metadataRef, template],
    )

    const onAppend = React.useCallback(() => {
      if (template?.type !== 'DIMENSIONS') {
        return
      }

      const newValues = [...template.dimensions, gridCSSNumber(cssNumber(1, 'fr'), null)]

      dispatch([
        applyCommandsAction([
          setProperty(
            'always',
            grid.elementPath,
            PP.create('style', axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows'),
            printArrayGridDimensions(newValues),
          ),
        ]),
      ])
    }, [dispatch, grid, axis, template])

    const onRename = React.useCallback(
      (index: number) => () => {
        if (template?.type !== 'DIMENSIONS') {
          return
        }
        const container = grid.specialSizeMeasurements.containerGridPropertiesFromProps
        const dimensions =
          axis === 'column' ? container.gridTemplateColumns : container.gridTemplateRows
        if (dimensions?.type !== 'DIMENSIONS') {
          return
        }
        const currentAreaName = dimensions.dimensions[index]?.areaName ?? undefined

        const rawNewAreaName = window.prompt('Area name:', currentAreaName)?.trim()
        if (rawNewAreaName == null) {
          return
        }

        const newAreaName: string | null =
          rawNewAreaName.length === 0 ? null : sanitizeAreaName(rawNewAreaName)

        const left = template.dimensions.slice(0, index)
        const right = template.dimensions.slice(index + 1)

        const newValues = [
          ...left,
          { ...values[index], areaName: newAreaName } as GridDimension,
          ...right,
        ]

        let commands: CanvasCommand[] = [
          setProperty(
            'always',
            grid.elementPath,
            PP.create('style', axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows'),
            printArrayGridDimensions(newValues),
          ),
        ]

        // replace the area name in the template and update the grid children so they
        // reference the new area name, if they used to reference the previous one
        const adjustedGridTemplate = renameAreaInTemplateAtIndex(
          container,
          axis,
          index,
          newAreaName,
        )
        const children = MetadataUtils.getChildrenUnordered(metadataRef.current, grid.elementPath)
        for (const child of children) {
          commands.push(
            ...setGridPropsCommands(
              child.elementPath,
              adjustedGridTemplate,
              child.specialSizeMeasurements.elementGridPropertiesFromProps,
            ),
          )
        }

        dispatch([applyCommandsAction(commands)])
      },
      [grid, axis, values, dispatch, metadataRef, template],
    )

    const dropdownMenuItems = React.useCallback(
      (index: number): DropdownMenuItem[] => {
        return [
          regularDropdownMenuItem({
            id: `rename-${axis}`,
            label: 'Rename',
            onSelect: onRename(index),
          }),
          regularDropdownMenuItem({
            id: `remove-${axis}`,
            label: 'Delete',
            onSelect: onRemove(index),
            danger: true,
          }),
        ]
      },
      [onRemove, axis, onRename],
    )

    const openDropdown = React.useCallback(
      (isOpen: boolean) => (
        <SquareButton
          data-testid={'openDropdown'}
          onClick={NO_OP}
          spotlight={isOpen ? true : false}
          highlight={false}
        >
          <Icons.Threedots color={isOpen ? 'main' : 'subdued'} />
        </SquareButton>
      ),
      [],
    )

    const dimensionsWithGeneratedIndexes = useGeneratedIndexesFromGridDimensions(values)

    return (
      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          gap: 6,
        }}
      >
        <div style={{ fontWeight: 600, display: 'flex', alignItems: 'center' }}>
          <div style={{ flex: 1 }}>{title}</div>
          <SquareButton highlight>
            <Icons.SmallPlus onClick={onAppend} />
          </SquareButton>
        </div>
        {dimensionsWithGeneratedIndexes.map((value, index) => (
          <AxisDimensionControl
            key={index}
            value={value.dimension}
            index={index}
            generatedIndexFrom={value.generatedIndexFrom}
            generatedIndexTo={value.generatedIndexTo}
            axis={axis}
            onUpdateNumberOrKeyword={onUpdateNumberOrKeyword}
            onUpdateDimension={onUpdateDimension}
            items={dropdownMenuItems(index)}
            opener={openDropdown}
          />
        ))}
      </div>
    )
  },
)
TemplateDimensionControl.displayName = 'TemplateDimensionControl'

function AxisDimensionControl({
  value,
  index,
  generatedIndexFrom: indexFrom,
  generatedIndexTo: indexTo,
  items,
  axis,
  onUpdateNumberOrKeyword,
  onUpdateDimension,
  opener,
}: {
  value: GridDimension
  index: number
  generatedIndexFrom: number
  generatedIndexTo: number
  items: DropdownMenuItem[]
  axis: 'column' | 'row'
  onUpdateNumberOrKeyword: (
    index: number,
  ) => (value: UnknownOrEmptyInput<CSSNumber | CSSKeyword<ValidGridDimensionKeyword>>) => void
  onUpdateDimension: (index: number) => (value: GridDimension) => void
  opener: (isOpen: boolean) => React.ReactElement
}) {
  const testId = `grid-dimension-${axis}-${index}`
  const [isOpen, setIsOpen] = React.useState(false)
  const onOpenChange = React.useCallback((isDropdownOpen: boolean) => {
    setIsOpen(isDropdownOpen)
  }, [])

  const title = React.useMemo(() => {
    if (indexFrom === indexTo) {
      return value.areaName ?? indexFrom
    }
    return value.areaName ?? `${indexFrom} → ${indexTo}`
  }, [value, indexFrom, indexTo])

  return (
    <div
      key={`col-${value}-${index}`}
      style={{ display: 'flex', alignItems: 'center', gap: 6 }}
      css={{
        [`& > .${axisDropdownMenuButton}`]: {
          visibility: isOpen ? 'visible' : 'hidden',
        },
        ':hover': {
          [`& > .${axisDropdownMenuButton}`]: {
            visibility: 'visible',
          },
        },
      }}
    >
      <div style={{ display: 'flex', alignItems: 'center', gap: 6, flex: 1 }}>
        <Subdued
          style={{
            width: 60,
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            whiteSpace: 'nowrap',
          }}
          title={value.areaName ?? undefined}
        >
          {title}
        </Subdued>

        <GridFunctionInput
          testId={testId}
          value={value}
          onUpdateNumberOrKeyword={onUpdateNumberOrKeyword(index)}
          onUpdateDimension={onUpdateDimension(index)}
        />
      </div>
      <SquareButton className={axisDropdownMenuButton}>
        <DropdownMenu align='end' items={items} opener={opener} onOpenChange={onOpenChange} />
      </SquareButton>
    </div>
  )
}

const GridFunctionInput = React.memo(
  ({
    testId,
    value,
    onUpdateNumberOrKeyword,
    onUpdateDimension,
  }: {
    testId: string
    value: GridDimension // This can be extended to support more function types
    onUpdateNumberOrKeyword: (v: CSSNumber | CSSKeyword<ValidGridDimensionKeyword>) => void
    onUpdateDimension: (v: GridDimension) => void
  }) => {
    const [printValue, setPrintValue] = React.useState<string>(printGridDimension(value))
    React.useEffect(() => setPrintValue(printGridDimension(value)), [value])

    const onChange = React.useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
      setPrintValue(e.target.value)
    }, [])

    const onKeyDown = React.useCallback(
      (e: React.KeyboardEvent) => {
        if (e.key === 'Enter') {
          if (isValidGridDimensionKeyword(printValue)) {
            return onUpdateNumberOrKeyword(cssKeyword(printValue))
          }

          const defaultUnit = isGridCSSNumber(value) ? value.value.unit : 'px'
          const maybeNumber = parseCSSNumber(printValue, 'AnyValid', defaultUnit)
          if (isRight(maybeNumber)) {
            return onUpdateNumberOrKeyword(maybeNumber.value)
          }

          const maybeMinmax = parseGridCSSMinmaxOrRepeat(printValue)
          if (maybeMinmax != null) {
            return onUpdateDimension({ ...maybeMinmax, areaName: value.areaName } as GridDimension)
          }

          if (printValue === '') {
            return onUpdateNumberOrKeyword(cssKeyword('auto'))
          }

          setPrintValue(printGridDimension(value))
        }
      },
      [printValue, onUpdateNumberOrKeyword, onUpdateDimension, value],
    )

    return (
      <StringInput testId={testId} value={printValue} onChange={onChange} onKeyDown={onKeyDown} />
    )
  },
)
GridFunctionInput.displayName = 'GridFunctionInput'

function removeTemplateValueAtIndex(
  original: GridContainerProperties,
  axis: 'column' | 'row',
  index: number,
): GridContainerProperties {
  function removeDimension(dimensions: GridDimension[]) {
    return dimensions.filter((_, idx) => idx !== index)
  }

  const gridTemplateRows =
    axis === 'row' && original.gridTemplateRows?.type === 'DIMENSIONS'
      ? {
          ...original.gridTemplateRows,
          dimensions: removeDimension(original.gridTemplateRows.dimensions),
        }
      : original.gridTemplateRows

  const gridTemplateColumns =
    axis === 'column' && original.gridTemplateColumns?.type === 'DIMENSIONS'
      ? {
          ...original.gridTemplateColumns,
          dimensions: removeDimension(original.gridTemplateColumns.dimensions),
        }
      : original.gridTemplateColumns

  return {
    ...original,
    gridTemplateRows: gridTemplateRows,
    gridTemplateColumns: gridTemplateColumns,
  }
}

function renameAreaInTemplateAtIndex(
  original: GridContainerProperties,
  axis: 'column' | 'row',
  index: number,
  newAreaName: string | null,
): GridContainerProperties {
  function renameDimension(dimension: GridDimension, idx: number): GridDimension {
    return idx === index
      ? ({
          ...dimension,
          areaName: dimension.type === 'REPEAT' ? null : newAreaName,
        } as GridDimension)
      : dimension
  }

  const gridTemplateRows =
    axis === 'row' && original.gridTemplateRows?.type === 'DIMENSIONS'
      ? {
          ...original.gridTemplateRows,
          dimensions: original.gridTemplateRows.dimensions.map(renameDimension),
        }
      : original.gridTemplateRows

  const gridTemplateColumns =
    axis === 'column' && original.gridTemplateColumns?.type === 'DIMENSIONS'
      ? {
          ...original.gridTemplateColumns,
          dimensions: original.gridTemplateColumns.dimensions.map(renameDimension),
        }
      : original.gridTemplateColumns

  return {
    ...original,
    gridTemplateRows: gridTemplateRows,
    gridTemplateColumns: gridTemplateColumns,
  }
}

const reAlphanumericDashUnderscore = /[^0-9a-z\-_]+/gi

function sanitizeAreaName(areaName: string): string {
  return areaName.replace(reAlphanumericDashUnderscore, '-')
}

function serializeValue(v: CSSNumber) {
  return v.unit == null || v.unit === 'px' ? v.value : cssNumberToString(v)
}

type GridGapControlSplitState = 'unified' | 'split'

function getGridGapControlsForHoverAndFocused(
  hoveredOrFocused: 'hovered' | 'focused',
  axis: Axis | 'both',
): Array<CanvasControlWithProps<SubduedGridGapControlProps>> {
  return [
    {
      control: SubduedGridGapControl,
      props: {
        hoveredOrFocused: hoveredOrFocused,
        axis: axis,
      },
      key: `subdued-grid-gap-${axis}-control-${hoveredOrFocused}`,
    },
  ]
}

const GapRowColumnControl = React.memo(() => {
  const dispatch = useDispatch()

  const grid = useEditorState(
    Substores.metadata,
    (store) => {
      if (store.editor.selectedViews.length !== 1) {
        return null
      }
      const element = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        store.editor.selectedViews[0],
      )
      if (!MetadataUtils.isGridLayoutedContainer(element)) {
        return null
      }
      return element
    },
    'GapRowColumnControl grid',
  )

  const gap = useInspectorLayoutInfo('gap') // also matches gridGap
  const columnGap = useInspectorLayoutInfo('columnGap')
  const rowGap = useInspectorLayoutInfo('rowGap')

  const { onMouseEnter, onMouseLeave } = useSetHoveredControlsHandlers<SubduedGridGapControlProps>()

  const onMouseEnterBothAxesWithGridGapControls = React.useCallback(
    () => onMouseEnter(getGridGapControlsForHoverAndFocused('hovered', 'both')),
    [onMouseEnter],
  )
  const onMouseEnterRowAxisWithGridGapControls = React.useCallback(
    () => onMouseEnter(getGridGapControlsForHoverAndFocused('hovered', 'row')),
    [onMouseEnter],
  )
  const onMouseEnterColumnAxisWithGridGapControls = React.useCallback(
    () => onMouseEnter(getGridGapControlsForHoverAndFocused('hovered', 'column')),
    [onMouseEnter],
  )

  const { onFocus, onBlur } = useSetFocusedControlsHandlers<SubduedGridGapControlProps>()

  const bothAxesInputProps = React.useMemo(
    () => ({
      onFocus: () => onFocus(getGridGapControlsForHoverAndFocused('focused', 'both')),
      onBlur: onBlur,
    }),
    [onFocus, onBlur],
  )

  const rowAxisInputProps = React.useMemo(
    () => ({
      onFocus: () => onFocus(getGridGapControlsForHoverAndFocused('focused', 'row')),
      onBlur: onBlur,
    }),
    [onBlur, onFocus],
  )

  const columnAxisInputProps = React.useMemo(
    () => ({
      onFocus: () => onFocus(getGridGapControlsForHoverAndFocused('focused', 'column')),
      onBlur: onBlur,
    }),
    [onBlur, onFocus],
  )

  const [controlSplitState, setControlSplitState] = React.useState<GridGapControlSplitState>(
    cssNumberEqual(columnGap.value, rowGap.value) ? 'unified' : 'split',
  )

  const cycleControlSplitState = React.useCallback(() => {
    setControlSplitState((state) => {
      switch (state) {
        case 'split':
          return 'unified'
        case 'unified':
          return 'split'
        default:
          assertNever(state)
      }
    })
  }, [])

  const onSubmitSplitValue = React.useCallback(
    (target: 'columnGap' | 'rowGap') =>
      (value: UnknownOrEmptyInput<CSSNumber>, transient?: boolean) => {
        if (grid == null) {
          return
        }

        if (isCSSNumber(value)) {
          let updates: PropertyToUpdate[] = []

          // if there's a gap, it needs to be stripped out and replaced with explicit
          // rowGap/colGap pairs.
          if (gap.controlStatus !== 'unset') {
            // clean up the other gaps
            updates.push(
              propertyToDelete(PP.create('style', 'gap')),
              propertyToDelete(PP.create('style', 'gridGap')),
            )

            // set the counterpart props
            updates.push(
              propertyToSet(
                PP.create('style', 'columnGap'),
                serializeValue(target === 'columnGap' ? value : gap.value),
              ),
            )
            updates.push(
              propertyToSet(
                PP.create('style', 'rowGap'),
                serializeValue(target === 'rowGap' ? value : gap.value),
              ),
            )
          } else {
            // set the new value on the target prop
            if (target === 'columnGap') {
              columnGap.onSubmitValue(value, transient)
            } else {
              rowGap.onSubmitValue(value, transient)
            }
          }

          dispatch([
            applyCommandsAction([updateBulkProperties('always', grid.elementPath, updates)]),
          ])
        }
      },
    [dispatch, grid, gap, rowGap, columnGap],
  )

  const onSubmitUnifiedValue = React.useCallback(
    (value: UnknownOrEmptyInput<CSSNumber>, transient?: boolean) => {
      if (grid == null || !isCSSNumber(value)) {
        return
      }

      const transientWrapper = (actions: EditorAction[]) =>
        transient ? [transientActions(actions)] : actions

      dispatch(
        transientWrapper([
          applyCommandsAction([
            updateBulkProperties('always', grid.elementPath, [
              propertyToDelete(PP.create('style', 'columnGap')),
              propertyToDelete(PP.create('style', 'rowGap')),
              propertyToDelete(PP.create('style', 'gap')),
              propertyToSet(PP.create('style', 'gridGap'), serializeValue(value)),
            ]),
          ]),
        ]),
      )
    },
    [dispatch, grid],
  )

  const tooltipTitle =
    controlSplitState === 'unified'
      ? 'Longhand gap'
      : controlSplitState === 'split'
      ? 'Shorthand gap'
      : assertNever(controlSplitState)

  const modeIcon = React.useMemo(() => {
    switch (controlSplitState) {
      case 'split':
        return <InspectorSectionIcons.SplitFull />
      case 'unified':
        return <InspectorSectionIcons.SplitHalf />
      default:
        assertNever(controlSplitState)
    }
  }, [controlSplitState])

  if (grid == null) {
    return null
  }

  return (
    <FlexRow style={{ justifyContent: 'space-between', gap: 8 }}>
      {when(
        controlSplitState === 'unified',
        <UIGridRow
          padded={false}
          variant='<--1fr--><--1fr-->'
          onMouseEnter={onMouseEnterBothAxesWithGridGapControls}
          onMouseLeave={onMouseLeave}
        >
          <NumberInput
            value={columnGap.value}
            numberType={'Length'}
            onSubmitValue={onSubmitUnifiedValue}
            onTransientSubmitValue={onSubmitUnifiedValue}
            onForcedSubmitValue={onSubmitUnifiedValue}
            defaultUnitToHide={'px'}
            testId={'grid-column-gap'}
            inputProps={bothAxesInputProps}
            innerLabel={<Icons.GapHorizontal color='on-highlight-secondary' />}
          />
        </UIGridRow>,
      )}
      {when(
        controlSplitState === 'split',
        <UIGridRow padded={false} variant='<--1fr--><--1fr-->' onMouseLeave={onMouseLeave}>
          <NumberInput
            value={columnGap.value}
            numberType={'Length'}
            onSubmitValue={onSubmitSplitValue('columnGap')}
            onTransientSubmitValue={onSubmitSplitValue('columnGap')}
            onForcedSubmitValue={onSubmitSplitValue('columnGap')}
            defaultUnitToHide={'px'}
            testId={'grid-column-gap'}
            inputProps={columnAxisInputProps}
            onMouseEnter={onMouseEnterColumnAxisWithGridGapControls}
            onMouseLeave={onMouseLeave}
            innerLabel={<Icons.GapHorizontal color='on-highlight-secondary' />}
          />
          <NumberInput
            value={rowGap.value}
            numberType={'Length'}
            onSubmitValue={onSubmitSplitValue('rowGap')}
            onTransientSubmitValue={onSubmitSplitValue('rowGap')}
            onForcedSubmitValue={onSubmitSplitValue('rowGap')}
            defaultUnitToHide={'px'}
            testId={'grid-row-gap'}
            inputProps={rowAxisInputProps}
            onMouseEnter={onMouseEnterRowAxisWithGridGapControls}
            onMouseLeave={onMouseLeave}
            innerLabel={<Icons.GapVertical color='on-highlight-secondary' />}
          />
        </UIGridRow>,
      )}
      <Tooltip title={tooltipTitle}>
        <SquareButton
          data-testid={`grid-gap-cycle-mode`}
          onClick={cycleControlSplitState}
          highlight
        >
          {modeIcon}
        </SquareButton>
      </Tooltip>
    </FlexRow>
  )
})
GapRowColumnControl.displayName = 'GapRowColumnControl'

const AutoFlowPopupId = 'auto-flow-control'

function selectOption(value: GridAutoFlow) {
  return regularRadixSelectOption({
    label: value,
    value: value,
    icon: gridAutoFlowIcon(value),
  })
}

const unsetSelectOption = regularRadixSelectOption({
  label: 'unset',
  value: 'unset',
  placeholder: true,
})

// `row dense` is omitted from here, because it turns out that the when it's
// written to the DOM, it's turned into `dense`. This surfaced as a bug, because
// even though `row dense` was set in the style prop, the DOM walker put `dense`
// into the specialSizeMeasurements entry for `grid-auto-flow`. We suspect that
// `row` is implicit in `dense`, and the browser omits the `row` to keep things
// simple
const RESTRICTED_GRID_AUTO_FLOW_VALUES: GridAutoFlow[] = ['dense', 'row', 'column', 'column dense']

const autoflowOptions = [
  unsetSelectOption,
  separatorRadixSelectOption(),
  ...RESTRICTED_GRID_AUTO_FLOW_VALUES.map(selectOption),
]

const AutoFlowControl = React.memo(() => {
  const dispatch = useDispatch()
  const selectededViewsRef = useRefEditorState((store) => store.editor.selectedViews)

  const gridAutoFlowValue = useEditorState(
    Substores.metadata,
    (store) => {
      const gridAutoFlowValues = store.editor.selectedViews.map(
        (view) =>
          MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, view)
            ?.specialSizeMeasurements.containerGridProperties.gridAutoFlow ?? null,
      )
      const allValuesMatch = strictEvery(gridAutoFlowValues, (v) => v === gridAutoFlowValues.at(0))
      if (allValuesMatch) {
        return gridAutoFlowValues.at(0) ?? null
      }
      return null
    },
    'AutoFlowControl gridAutoFlowValue',
  )

  const { controlStatus } = useInspectorStyleInfo('gridAutoFlow')

  const currentValue = React.useMemo(
    () =>
      controlStatus === 'detected'
        ? unsetSelectOption
        : optionalMap(selectOption, gridAutoFlowValue) ?? undefined,
    [controlStatus, gridAutoFlowValue],
  )

  const onSubmit = React.useCallback(
    (value: string) => {
      if (selectededViewsRef.current.length === 0) {
        return
      }
      dispatch(
        selectededViewsRef.current.map((path) =>
          applyCommandsAction([
            updateBulkProperties('always', path, [
              value === 'unset'
                ? propertyToDelete(PP.create('style', 'gridAutoFlow'))
                : propertyToSet(PP.create('style', 'gridAutoFlow'), value),
            ]),
          ]),
        ),
      )
    },
    [dispatch, selectededViewsRef],
  )

  return (
    <UIGridRow variant='|--60px--|<--1fr-->|22px|' padded={false}>
      <div>Auto Flow</div>
      <RadixSelect
        id={AutoFlowPopupId}
        value={currentValue ?? null}
        options={autoflowOptions}
        onValueChange={onSubmit}
      />
    </UIGridRow>
  )
})
AutoFlowControl.displayName = 'AutoFlowControl'

interface GridDimensionWithGeneratedIndexes {
  generatedIndexFrom: number
  generatedIndexTo: number
  dimension: GridDimension
}

function useGeneratedIndexesFromGridDimensions(
  dimensions: Array<GridDimension>,
): Array<GridDimensionWithGeneratedIndexes> {
  return React.useMemo(() => {
    let nextIndexFrom = 1
    let result: Array<GridDimensionWithGeneratedIndexes> = []

    dimensions.forEach((dim) => {
      if (dim.type !== 'REPEAT') {
        result.push({
          generatedIndexFrom: nextIndexFrom,
          generatedIndexTo: nextIndexFrom,
          dimension: dim,
        })
        nextIndexFrom += 1
      } else {
        // TODO: handle auto-fill and auto-fit in value.times
        const shift = !isCSSKeyword(dim.times) ? dim.times * dim.value.length : dim.value.length
        result.push({
          generatedIndexFrom: nextIndexFrom,
          generatedIndexTo: nextIndexFrom + shift - 1,
          dimension: dim,
        })
        nextIndexFrom += shift
      }
    })
    return result
  }, [dimensions])
}
