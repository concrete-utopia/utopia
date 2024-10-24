import React from 'react'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../../core/shared/array-utils'
import * as EP from '../../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  GridContainerProperties,
  GridElementProperties,
  GridPositionValue,
  ValidGridPositionKeyword,
} from '../../../../../core/shared/element-template'
import {
  gridPositionValue,
  isValidGridPositionKeyword,
  type GridPosition,
} from '../../../../../core/shared/element-template'
import { unless, when } from '../../../../../utils/react-conditionals'
import {
  FlexRow,
  Icons,
  InspectorSectionIcons,
  InspectorSubsectionHeader,
  NumberInput,
  SquareButton,
} from '../../../../../uuiui'
import type { KeywordForControl } from '../../../../../uuiui/inputs/number-or-keyword-control'
import { NumberOrKeywordControl } from '../../../../../uuiui/inputs/number-or-keyword-control'
import { setGridPropsCommands } from '../../../../canvas/canvas-strategies/strategies/grid-helpers'
import { applyCommandsAction } from '../../../../editor/actions/action-creators'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../../../editor/store/store-hook'
import type { CSSKeyword, CSSNumber, UnknownOrEmptyInput } from '../../../common/css-utils'
import {
  cssKeyword,
  cssNumber,
  isCSSKeyword,
  isCSSNumber,
  isEmptyInputValue,
} from '../../../common/css-utils'
import { UIGridRow } from '../../../widgets/ui-grid-row'

type CellAdjustMode = 'dimensions' | 'boundaries'

export const GridPlacementSubsection = React.memo(() => {
  const dispatch = useDispatch()

  const [adjustMode, setAdjustMode] = React.useState<CellAdjustMode>('dimensions')

  const cell = useEditorState(
    Substores.metadata,
    (store) =>
      MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        store.editor.selectedViews[0],
      ),
    'GridPlacementSubsection cell',
  )

  const gridTemplate = useEditorState(
    Substores.metadata,
    (store) => {
      if (cell == null) {
        return null
      }
      return MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        EP.parentPath(cell.elementPath),
      )?.specialSizeMeasurements.containerGridProperties
    },
    'GridPlacementSubsection gridTemplate',
  )

  const toggleMoveMode = React.useCallback(() => {
    setAdjustMode((current) => (current === 'boundaries' ? 'dimensions' : 'boundaries'))
  }, [])

  const isAllDefaults = React.useMemo(() => {
    if (cell == null) {
      return false
    }
    const data = cell.specialSizeMeasurements.elementGridPropertiesFromProps
    return (
      data.gridColumnStart == null &&
      data.gridColumnEnd == null &&
      data.gridRowStart == null &&
      data.gridRowEnd == null
    )
  }, [cell])

  const writeDefaults = React.useCallback(() => {
    if (cell == null || gridTemplate == null) {
      return
    }

    const commands = setGridPropsCommands(
      cell.elementPath,
      gridTemplate,
      cell.specialSizeMeasurements.elementGridProperties,
    )
    dispatch([applyCommandsAction(commands)])
  }, [dispatch, cell, gridTemplate])

  if (cell == null || gridTemplate == null) {
    return null
  }

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
          <span>Grid Placement</span>
        </FlexRow>
        {when(
          isAllDefaults,
          <SquareButton highlight onClick={writeDefaults}>
            <Icons.SmallPlus />
          </SquareButton>,
        )}
      </InspectorSubsectionHeader>
      {unless(
        isAllDefaults,
        <UIGridRow variant='<--------auto-------->||22px|' padded style={{ minHeight: 68 }}>
          <div>
            {when(
              adjustMode === 'dimensions',
              <DimensionsControls cell={cell} gridTemplate={gridTemplate} />,
            )}
            {when(
              adjustMode === 'boundaries',
              <BoundariesControls cell={cell} gridTemplate={gridTemplate} />,
            )}
          </div>
          <div
            style={{
              alignSelf: 'start',
              height: 34,
              display: 'flex',
              alignItems: 'center',
            }}
          >
            <SquareButton highlight>
              {adjustMode === 'dimensions' ? (
                <InspectorSectionIcons.SplitFull onClick={toggleMoveMode} />
              ) : (
                <InspectorSectionIcons.SplitHalf onClick={toggleMoveMode} />
              )}
            </SquareButton>
          </div>
        </UIGridRow>,
      )}
    </>
  )
})
GridPlacementSubsection.displayName = 'GridPlacementSubsection'

const DimensionsControls = React.memo(
  ({
    cell,
    gridTemplate,
  }: {
    cell: ElementInstanceMetadata
    gridTemplate: GridContainerProperties
  }) => {
    const dispatch = useDispatch()

    const width = React.useMemo(() => {
      return getWidthOrHeight(cell.specialSizeMeasurements.elementGridProperties, 'width')
    }, [cell])

    const height = React.useMemo(() => {
      return getWidthOrHeight(cell.specialSizeMeasurements.elementGridProperties, 'height')
    }, [cell])

    const { columnLabels, rowLabels } = React.useMemo(() => {
      return getLabelsFromTemplate(gridTemplate)
    }, [gridTemplate])

    const onSubmitPosition = React.useCallback(
      (dimension: 'gridRowStart' | 'height' | 'gridColumnStart' | 'width') =>
        (e: UnknownOrEmptyInput<CSSNumber | CSSKeyword<ValidGridPositionKeyword>>) => {
          let value: GridPosition = isEmptyInputValue(e)
            ? cssKeyword('auto')
            : isCSSNumber(e)
            ? gridPositionValue(e.value)
            : cssKeyword(e.value)

          const maybeLineValue = maybeValueFromLineName(
            value,
            dimension === 'gridColumnStart' || dimension === 'width' ? columnLabels : rowLabels,
          )
          if (maybeLineValue != null) {
            value = maybeLineValue
          }

          let newValues = {
            ...cell.specialSizeMeasurements.elementGridProperties,
          }

          switch (dimension) {
            case 'gridColumnStart':
              newValues.gridColumnStart = value
              if (
                !isCSSKeyword(newValues.gridColumnStart) &&
                newValues.gridColumnStart.numericalPosition != null &&
                !isCSSKeyword(value)
              ) {
                newValues.gridColumnEnd = gridPositionValue((value.numericalPosition ?? 0) + width)
              }
              break
            case 'gridRowStart':
              newValues.gridRowStart = value
              if (
                !isCSSKeyword(newValues.gridRowStart) &&
                newValues.gridRowStart?.numericalPosition != null &&
                isCSSNumber(e)
              ) {
                newValues.gridRowEnd = gridPositionValue(e.value + height)
              }
              break
            case 'width':
              if (
                !isCSSKeyword(newValues.gridColumnStart) &&
                newValues.gridColumnStart?.numericalPosition != null &&
                isCSSNumber(e)
              ) {
                newValues.gridColumnEnd = gridPositionValue(
                  newValues.gridColumnStart.numericalPosition + e.value,
                )
              }
              break
            case 'height':
              if (
                !isCSSKeyword(newValues.gridRowStart) &&
                newValues.gridRowStart?.numericalPosition != null &&
                isCSSNumber(e)
              ) {
                newValues.gridRowEnd = gridPositionValue(
                  newValues.gridRowStart.numericalPosition + e.value,
                )
              }
              break
          }
          const commands = setGridPropsCommands(cell.elementPath, gridTemplate, newValues)

          dispatch([applyCommandsAction(commands)])
        },
      [dispatch, cell, gridTemplate, width, height, columnLabels, rowLabels],
    )

    const columnStartValue = React.useMemo(() => {
      return getValueWithAlias(
        cell.specialSizeMeasurements.elementGridProperties.gridColumnStart,
        columnLabels,
      )
    }, [cell, columnLabels])

    const rowStartValue = React.useMemo(() => {
      return getValueWithAlias(
        cell.specialSizeMeasurements.elementGridProperties.gridRowStart,
        rowLabels,
      )
    }, [cell, rowLabels])

    return (
      <>
        <UIGridRow padded={false} variant='|--50px--|<--------1fr-------->'>
          <div>Position</div>
          <UIGridRow padded={false} variant='<--1fr--><--1fr-->'>
            <NumberOrKeywordControl
              testId='input-position-column-start'
              onSubmitValue={onSubmitPosition('gridColumnStart')}
              value={columnStartValue.value}
              valueAlias={columnStartValue.alias}
              keywords={keywordsForPosition(columnLabels.map((l) => l.lineName))}
              keywordTypeCheck={isValidGridPositionKeyword(columnLabels.map((l) => l.lineName))}
              labelInner={{
                category: 'inspector-element',
                type: 'gridColumn',
                color: 'on-highlight-secondary',
              }}
            />
            <NumberOrKeywordControl
              testId='input-position-row-start'
              onSubmitValue={onSubmitPosition('gridRowStart')}
              value={rowStartValue.value}
              valueAlias={rowStartValue.alias}
              keywords={keywordsForPosition(rowLabels.map((l) => l.lineName))}
              keywordTypeCheck={isValidGridPositionKeyword(rowLabels.map((l) => l.lineName))}
              labelInner={{
                category: 'inspector-element',
                type: 'gridRow',
                color: 'on-highlight-secondary',
              }}
            />
          </UIGridRow>
        </UIGridRow>
        <UIGridRow padded={false} variant='|--50px--|<--------1fr-------->'>
          <div>Size</div>
          <UIGridRow padded={false} variant='<--1fr--><--1fr-->'>
            <NumberInput
              value={cssNumber(width)}
              numberType='Unitless'
              defaultUnitToHide={null}
              testId='grid-cell-row-width'
              onSubmitValue={onSubmitPosition('width')}
              inputProps={{ placeholder: 'auto' }}
              innerLabel={<Icons.RowSpan color='on-highlight-secondary' />}
              descriptionLabel={width === 1 ? 'Col' : 'Cols'}
            />
            <NumberInput
              value={cssNumber(height)}
              numberType='Unitless'
              defaultUnitToHide={null}
              testId='grid-cell-row-height'
              onSubmitValue={onSubmitPosition('height')}
              inputProps={{ placeholder: 'auto' }}
              innerLabel={<Icons.ColumnSpan color='on-highlight-secondary' />}
              descriptionLabel={height === 1 ? 'Row' : 'Rows'}
            />
          </UIGridRow>
        </UIGridRow>
      </>
    )
  },
)
DimensionsControls.displayName = 'DimensionsControls'

const BoundariesControls = React.memo(
  ({
    cell,
    gridTemplate,
  }: {
    cell: ElementInstanceMetadata
    gridTemplate: GridContainerProperties
  }) => {
    const dispatch = useDispatch()

    const { columnLabels, rowLabels } = React.useMemo(() => {
      return getLabelsFromTemplate(gridTemplate)
    }, [gridTemplate])

    const onSubmitPosition = React.useCallback(
      (dimension: 'gridRowStart' | 'gridRowEnd' | 'gridColumnStart' | 'gridColumnEnd') =>
        (e: UnknownOrEmptyInput<CSSNumber | CSSKeyword<ValidGridPositionKeyword>>) => {
          let value: GridPosition = isEmptyInputValue(e)
            ? cssKeyword('auto')
            : isCSSNumber(e)
            ? gridPositionValue(e.value)
            : cssKeyword(e.value)

          const maybeLineValue = maybeValueFromLineName(
            value,
            dimension === 'gridColumnStart' || dimension === 'gridColumnEnd'
              ? columnLabels
              : rowLabels,
          )
          if (maybeLineValue != null) {
            value = maybeLineValue
          }

          const newValues = {
            ...cell.specialSizeMeasurements.elementGridProperties,
            [dimension]: value,
          }
          const commands = setGridPropsCommands(cell.elementPath, gridTemplate, newValues)

          dispatch([applyCommandsAction(commands)])
        },
      [dispatch, cell, gridTemplate, columnLabels, rowLabels],
    )

    const columnStartValue = React.useMemo(() => {
      return getValueWithAlias(
        cell.specialSizeMeasurements.elementGridProperties.gridColumnStart,
        columnLabels,
      )
    }, [cell, columnLabels])

    const columnEndValue = React.useMemo(() => {
      return getValueWithAlias(
        cell.specialSizeMeasurements.elementGridProperties.gridColumnEnd,
        columnLabels,
      )
    }, [cell, columnLabels])

    const rowStartValue = React.useMemo(() => {
      return getValueWithAlias(
        cell.specialSizeMeasurements.elementGridProperties.gridRowStart,
        rowLabels,
      )
    }, [cell, rowLabels])

    const rowEndValue = React.useMemo(() => {
      return getValueWithAlias(
        cell.specialSizeMeasurements.elementGridProperties.gridRowEnd,
        rowLabels,
      )
    }, [cell, rowLabels])

    return (
      <>
        <UIGridRow padded={false} variant='|--50px--|<--------1fr-------->'>
          <div>Start</div>
          <UIGridRow padded={false} variant='<--1fr--><--1fr-->'>
            <NumberOrKeywordControl
              value={columnStartValue.value}
              valueAlias={columnStartValue.alias}
              testId='grid-cell-column-start'
              onSubmitValue={onSubmitPosition('gridColumnStart')}
              labelInner={{
                category: 'inspector-element',
                type: 'gridColumn-start',
                color: 'on-highlight-secondary',
              }}
              keywords={keywordsForPosition(columnLabels.map((l) => l.lineName))}
              keywordTypeCheck={isValidGridPositionKeyword(columnLabels.map((l) => l.lineName))}
            />
            <NumberOrKeywordControl
              value={rowStartValue.value}
              valueAlias={rowStartValue.alias}
              testId='grid-cell-row-start'
              onSubmitValue={onSubmitPosition('gridRowStart')}
              labelInner={{
                category: 'inspector-element',
                type: 'gridRow-start',
                color: 'on-highlight-secondary',
              }}
              keywords={keywordsForPosition(rowLabels.map((l) => l.lineName))}
              keywordTypeCheck={isValidGridPositionKeyword(rowLabels.map((l) => l.lineName))}
            />
          </UIGridRow>
        </UIGridRow>
        <UIGridRow padded={false} variant='|--50px--|<--------1fr-------->'>
          <div>End</div>
          <UIGridRow padded={false} variant='<--1fr--><--1fr-->'>
            <NumberOrKeywordControl
              value={columnEndValue.value}
              valueAlias={columnEndValue.alias}
              testId='grid-cell-column-end'
              onSubmitValue={onSubmitPosition('gridColumnEnd')}
              labelInner={{
                category: 'inspector-element',
                type: 'gridColumn-end',
                color: 'on-highlight-secondary',
              }}
              keywords={keywordsForPosition(columnLabels.map((l) => l.lineName))}
              keywordTypeCheck={isValidGridPositionKeyword(columnLabels.map((l) => l.lineName))}
            />
            <NumberOrKeywordControl
              value={rowEndValue.value}
              valueAlias={rowEndValue.alias}
              testId='grid-cell-row-end'
              onSubmitValue={onSubmitPosition('gridRowEnd')}
              labelInner={{
                category: 'inspector-element',
                type: 'gridRow-end',
                color: 'on-highlight-secondary',
              }}
              keywords={keywordsForPosition(rowLabels.map((l) => l.lineName))}
              keywordTypeCheck={isValidGridPositionKeyword(rowLabels.map((l) => l.lineName))}
            />
          </UIGridRow>
        </UIGridRow>
      </>
    )
  },
)
BoundariesControls.displayName = 'BoundariesControls'

function getValue(pos: GridPosition | null): CSSNumber | null {
  if (pos == null || isCSSKeyword(pos) || pos.numericalPosition == null) {
    return null
  }
  return cssNumber(pos.numericalPosition)
}

function getWidthOrHeight(props: GridElementProperties, dimension: 'width' | 'height') {
  const start = getValue(dimension === 'width' ? props.gridColumnStart : props.gridRowStart)
  const end = getValue(dimension === 'width' ? props.gridColumnEnd : props.gridRowEnd)
  if (start == null || end == null) {
    return 1
  }
  return end.value - start.value
}

function getLabelsFromTemplate(gridTemplate: GridContainerProperties) {
  const getAxisLabels = (axis: 'gridTemplateColumns' | 'gridTemplateRows') => {
    const template = gridTemplate[axis]
    if (template?.type !== 'DIMENSIONS') {
      return []
    }
    return mapDropNulls((d, index) => {
      if (d.lineName == null) {
        return null
      }
      return { lineName: d.lineName, position: index + 1 }
    }, template.dimensions)
  }
  const columnLabels = getAxisLabels('gridTemplateColumns')

  const rowLabels = getAxisLabels('gridTemplateRows')

  return { columnLabels, rowLabels }
}

function keywordsForPosition(labels: string[]) {
  let items: KeywordForControl<string>[] = [{ label: 'Auto', value: cssKeyword('auto') }]
  if (labels.length > 0) {
    items.push('separator')
    items.push(
      ...labels.map((r) => ({
        label: r,
        value: cssKeyword(r),
      })),
    )
  }
  return items
}

function maybeValueFromLineName(
  value: GridPosition,
  labels: { lineName: string; position: number }[],
): GridPositionValue | null {
  if (!isCSSKeyword(value)) {
    return null
  }
  const lineMatch = labels.find((l) => l.lineName === value.value)
  if (lineMatch != null) {
    return gridPositionValue(lineMatch.position)
  }
  return null
}

function getValueWithAlias(
  position: GridPosition | null,
  labels: { lineName: string; position: number }[],
) {
  const value = getValue(position) ?? cssKeyword('auto')
  if (isCSSKeyword(value)) {
    return { value: value }
  }
  const lineName = labels.find((l) => l.position === value.value)
  return { value: value, alias: lineName?.lineName }
}
