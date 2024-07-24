/** @jsxRuntime classic */
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
import { LayoutSystemControl } from '../../components/inspector/sections/layout-section/layout-system-subsection/layout-system-controls'
import { SpacedPackedControl } from './spaced-packed-control'
import { ThreeBarControl } from './three-bar-control'
import { FlexGapControl } from './sections/layout-section/flex-container-subsection/flex-container-controls'
import { FlexContainerControls } from './sections/layout-section/flex-container-subsection/flex-container-subsection'
import { FlexCol } from 'utopia-api'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { strictEvery } from '../../core/shared/array-utils'
import { useDispatch } from '../editor/store/dispatch-context'
import {
  addFlexLayoutStrategies,
  addGridLayoutStrategies,
} from './inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import type { DetectedLayoutSystem } from 'utopia-shared/src/types'
import { assertNever } from '../../core/shared/utils'
import { Icons, NumberInput, SquareButton, Subdued } from '../../uuiui'
import type { CSSNumber, GridCSSNumberUnit, UnknownOrEmptyInput } from './common/css-utils'
import { gridCSSNumber, isCSSNumber, type GridCSSNumber } from './common/css-utils'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { setProperty } from '../canvas/commands/set-property-command'
import * as PP from '../../core/shared/property-path'
import type { GridContainerProperties, GridPosition } from '../../core/shared/element-template'
import {
  gridPositionValue,
  type ElementInstanceMetadata,
  type GridElementProperties,
} from '../../core/shared/element-template'
import { setGridPropsCommands } from '../canvas/canvas-strategies/strategies/grid-helpers'
import { type CanvasCommand } from '../canvas/commands/commands'

export const layoutSystemSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  (metadata, selectedViews) => {
    const detectedLayoutSystems = selectedViews.map(
      (path) =>
        MetadataUtils.findElementByElementPath(metadata, path)?.specialSizeMeasurements
          .layoutSystemForChildren ?? null,
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

  const dispatch = useDispatch()
  const elementMetadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const addFlexLayoutSystem = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        addFlexLayoutStrategies(
          elementMetadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
        ),
      ),
    [allElementPropsRef, dispatch, elementMetadataRef, elementPathTreeRef, selectedViewsRef],
  )

  const addGridLayoutSystem = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        addGridLayoutStrategies(
          elementMetadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
        ),
      ),
    [allElementPropsRef, dispatch, elementMetadataRef, elementPathTreeRef, selectedViewsRef],
  )

  const onLayoutSystemChange = React.useCallback(
    (value: DetectedLayoutSystem) => {
      switch (value) {
        case 'flex':
          return addFlexLayoutSystem()
        case 'grid':
          return addGridLayoutSystem()
        case 'flow':
        case 'none':
          return
        default:
          assertNever(value)
      }
    },
    [addFlexLayoutSystem, addGridLayoutSystem],
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

  const columns: GridCSSNumber[] = React.useMemo(() => {
    if (grid == null) {
      return []
    }

    const fromProps =
      grid.specialSizeMeasurements.containerGridPropertiesFromProps.gridTemplateColumns
    const calculated = grid.specialSizeMeasurements.containerGridProperties.gridTemplateColumns

    if (fromProps?.type !== 'DIMENSIONS' && calculated?.type !== 'DIMENSIONS') {
      return []
    }

    let dimensions: GridCSSNumber[] = []

    if (calculated?.type === 'DIMENSIONS') {
      dimensions.push(...calculated.dimensions)
    }
    if (fromProps?.type === 'DIMENSIONS') {
      if (dimensions.length === 0) {
        return fromProps.dimensions
      } else if (dimensions.length === fromProps.dimensions.length) {
        return fromProps.dimensions
      }
    }

    return dimensions
  }, [grid])

  const rows: GridCSSNumber[] = React.useMemo(() => {
    if (grid == null) {
      return []
    }

    const fromProps = grid.specialSizeMeasurements.containerGridPropertiesFromProps.gridTemplateRows
    const calculated = grid.specialSizeMeasurements.containerGridProperties.gridTemplateRows

    if (fromProps?.type !== 'DIMENSIONS' && calculated?.type === 'DIMENSIONS') {
      return []
    }

    let merged: GridCSSNumber[] = []

    if (calculated?.type === 'DIMENSIONS') {
      merged.push(...calculated.dimensions)
    }

    if (fromProps?.type === 'DIMENSIONS') {
      if (merged.length === 0) {
        return fromProps.dimensions
      } else if (merged.length === fromProps.dimensions.length) {
        return fromProps.dimensions
      }
    }

    return merged
  }, [grid])

  return (
    <div>
      <AddRemoveLayoutSystemControl />
      <FlexCol css={{ gap: 10, paddingBottom: 10 }}>
        {when(
          layoutSystem === 'grid' || layoutSystem === 'flex',
          <UIGridRow padded={true} variant='<-------------1fr------------->'>
            <LayoutSystemControl
              layoutSystem={layoutSystem}
              providesCoordinateSystemForChildren={false}
              onChange={onLayoutSystemChange}
            />
          </UIGridRow>,
        )}
        {when(
          layoutSystem === 'grid',
          <UIGridRow padded tall={false} variant={'<-------------1fr------------->'}>
            <div style={{ display: 'flex', flexDirection: 'column', gap: 10 }}>
              {grid != null ? (
                <React.Fragment>
                  <TemplateDimensionControl
                    axis={'column'}
                    grid={grid}
                    values={columns}
                    title='Columns'
                  />
                  <TemplateDimensionControl axis={'row'} grid={grid} values={rows} title='Rows' />
                </React.Fragment>
              ) : null}
            </div>
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
      </FlexCol>
    </div>
  )
})

const TemplateDimensionControl = React.memo(
  ({
    grid,
    values,
    axis,
    title,
  }: {
    grid: ElementInstanceMetadata
    values: GridCSSNumber[]
    axis: 'column' | 'row'
    title: string
  }) => {
    const dispatch = useDispatch()

    const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)

    const onUpdate = React.useCallback(
      (index: number) => (value: UnknownOrEmptyInput<CSSNumber>) => {
        if (!isCSSNumber(value)) {
          return
        }

        const newValues = [...values]
        newValues[index] = gridCSSNumber(
          value.value,
          (value.unit as GridCSSNumberUnit) ?? values[index].unit,
          values[index].areaName,
        )

        dispatch([
          applyCommandsAction([
            setProperty(
              'always',
              grid.elementPath,
              PP.create('style', axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows'),
              gridNumbersToTemplateString(newValues),
            ),
          ]),
        ])
      },
      [grid, values, dispatch, axis],
    )

    const onRemove = React.useCallback(
      (index: number) => () => {
        const newValues = values.filter((_, idx) => idx !== index)

        let commands: CanvasCommand[] = [
          setProperty(
            'always',
            grid.elementPath,
            PP.create('style', axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows'),
            gridNumbersToTemplateString(newValues),
          ),
        ]

        // adjust the position of the elements if they need to be moved
        const adjustedGridTemplate = removeTemplateValueAtIndex(
          grid.specialSizeMeasurements.containerGridProperties,
          axis,
          index,
        )

        const gridIndex = index + 1 // grid boundaries are 1-based

        const children = MetadataUtils.getChildrenUnordered(metadataRef.current, grid.elementPath)
        for (const child of children) {
          let updated: Partial<GridElementProperties> = {
            ...child.specialSizeMeasurements.elementGridProperties,
          }

          function needsAdjusting(pos: GridPosition | null, bound: number) {
            return pos != null &&
              pos !== 'auto' &&
              pos.numericalPosition != null &&
              pos.numericalPosition >= bound
              ? pos.numericalPosition
              : null
          }

          const position = child.specialSizeMeasurements.elementGridProperties
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
      [grid, values, dispatch, axis, metadataRef],
    )

    const onAdd = React.useCallback(() => {
      const newValues = values.concat(gridCSSNumber(1, 'fr', null))
      dispatch([
        applyCommandsAction([
          setProperty(
            'always',
            grid.elementPath,
            PP.create('style', axis === 'column' ? 'gridTemplateColumns' : 'gridTemplateRows'),
            gridNumbersToTemplateString(newValues),
          ),
        ]),
      ])
    }, [dispatch, grid, axis, values])

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
          <SquareButton>
            <Icons.Plus width={12} height={12} onClick={onAdd} />
          </SquareButton>
        </div>
        {values.map((col, index) => {
          return (
            <div
              key={`col-${col}-${index}`}
              style={{ display: 'flex', alignItems: 'center', gap: 6 }}
              css={{
                '& > .removeButton': {
                  visibility: 'hidden',
                },
                ':hover': {
                  '& > .removeButton': {
                    visibility: 'visible',
                  },
                },
              }}
            >
              <div style={{ display: 'flex', alignItems: 'center', gap: 6, flex: 1 }}>
                <Subdued
                  style={{
                    width: 40,
                    overflow: 'hidden',
                    textOverflow: 'ellipsis',
                    whiteSpace: 'nowrap',
                  }}
                  title={col.areaName ?? undefined}
                >
                  {col.areaName ?? index + 1}
                </Subdued>
                <NumberInput
                  style={{ flex: 1 }}
                  value={col}
                  numberType={'Length'}
                  onSubmitValue={onUpdate(index)}
                  onTransientSubmitValue={onUpdate(index)}
                  onForcedSubmitValue={onUpdate(index)}
                  defaultUnitToHide={null}
                  testId={`col-${col}-${index}`}
                />
              </div>
              <SquareButton className='removeButton'>
                {/* TODO replace this with a context menu! */}
                <Icons.Cross onClick={onRemove(index)} />
              </SquareButton>
            </div>
          )
        })}
      </div>
    )
  },
)
TemplateDimensionControl.displayName = 'TemplateDimensionControl'

function removeTemplateValueAtIndex(
  original: GridContainerProperties,
  axis: 'column' | 'row',
  index: number,
): GridContainerProperties {
  function removeDimension(dimensions: GridCSSNumber[]) {
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

function gridNumbersToTemplateString(values: GridCSSNumber[]) {
  return values
    .map((v) => {
      const areaName = v.areaName != null ? `[${v.areaName}] ` : ''
      const unit = v.unit != null ? `${v.unit}` : ''
      return areaName + `${v.value}` + unit
    })
    .join(' ')
}
