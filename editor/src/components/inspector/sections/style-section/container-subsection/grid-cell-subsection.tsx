import React from 'react'
import {
  FlexRow,
  InspectorSectionIcons,
  InspectorSubsectionHeader,
  NumberInput,
  SquareButton,
} from '../../../../../uuiui'
import { Substores, useEditorState } from '../../../../editor/store/store-hook'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import type {
  ElementInstanceMetadata,
  GridContainerProperties,
  GridElementProperties,
} from '../../../../../core/shared/element-template'
import { gridPositionValue, type GridPosition } from '../../../../../core/shared/element-template'
import type { CSSNumber, UnknownOrEmptyInput } from '../../../common/css-utils'
import { cssNumber, isCSSNumber, isEmptyInputValue } from '../../../common/css-utils'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { applyCommandsAction } from '../../../../editor/actions/action-creators'
import { setGridPropsCommands } from '../../../../canvas/canvas-strategies/strategies/grid-helpers'
import * as EP from '../../../../../core/shared/element-path'
import { when } from '../../../../../utils/react-conditionals'

type CellAdjustMode = 'dimensions' | 'boundaries'

export const GridPlacementSubsection = React.memo(() => {
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
      </InspectorSubsectionHeader>
      <div style={{ display: 'grid', gridTemplateColumns: '1fr 27px', gridTemplateRows: '1fr' }}>
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
          <SquareButton style={{ padding: 0 }}>
            {adjustMode === 'dimensions' ? (
              <InspectorSectionIcons.SplitFull onClick={toggleMoveMode} />
            ) : (
              <InspectorSectionIcons.SplitHalf onClick={toggleMoveMode} />
            )}
          </SquareButton>
        </div>
      </div>
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

    const onSubmitPosition = React.useCallback(
      (dimension: 'gridRowStart' | 'height' | 'gridColumnStart' | 'width') =>
        (e: UnknownOrEmptyInput<CSSNumber>) => {
          const value: GridPosition =
            isEmptyInputValue(e) || typeof e.value === 'string'
              ? 'auto'
              : gridPositionValue(e.value)

          let newValues = {
            ...cell.specialSizeMeasurements.elementGridProperties,
          }

          switch (dimension) {
            case 'gridColumnStart':
              newValues.gridColumnStart = value
              if (
                newValues.gridColumnStart !== 'auto' &&
                newValues.gridColumnStart?.numericalPosition != null &&
                isCSSNumber(e)
              ) {
                newValues.gridColumnEnd = gridPositionValue(e.value + width)
              }
              break
            case 'gridRowStart':
              newValues.gridRowStart = value
              if (
                newValues.gridRowStart !== 'auto' &&
                newValues.gridRowStart?.numericalPosition != null &&
                isCSSNumber(e)
              ) {
                newValues.gridRowEnd = gridPositionValue(e.value + height)
              }
              break
            case 'width':
              if (
                newValues.gridColumnStart !== 'auto' &&
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
                newValues.gridRowStart !== 'auto' &&
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
      [dispatch, cell, gridTemplate, width, height],
    )

    return (
      <>
        <UIGridRow padded variant='|--80px--|<--------1fr-------->'>
          <div>Position</div>
          <UIGridRow padded={false} variant='<--1fr--><--1fr-->'>
            <NumberInput
              value={getValue(cell.specialSizeMeasurements.elementGridProperties.gridColumnStart)}
              numberType='Unitless'
              defaultUnitToHide={null}
              testId='grid-cell-column-start'
              onSubmitValue={onSubmitPosition('gridColumnStart')}
              inputProps={{ placeholder: 'auto' }}
              labelInner={{
                category: 'inspector-element',
                type: 'gridColumn',
                color: 'subdued',
              }}
            />
            <NumberInput
              value={getValue(cell.specialSizeMeasurements.elementGridProperties.gridRowStart)}
              numberType='Unitless'
              defaultUnitToHide={null}
              testId='grid-cell-row-start'
              onSubmitValue={onSubmitPosition('gridRowStart')}
              inputProps={{ placeholder: 'auto' }}
              labelInner={{
                category: 'inspector-element',
                type: 'gridRow',
                color: 'subdued',
              }}
            />
          </UIGridRow>
        </UIGridRow>
        <UIGridRow padded variant='|--80px--|<--------1fr-------->'>
          <div>Size</div>
          <FlexRow style={{ gap: 4 }}>
            <NumberInput
              value={cssNumber(width)}
              numberType='Unitless'
              defaultUnitToHide={null}
              testId='grid-cell-row-width'
              onSubmitValue={onSubmitPosition('width')}
              inputProps={{ placeholder: 'auto' }}
              labelInner={{
                category: 'inspector-element',
                type: 'rowSpan',
                color: 'subdued',
              }}
            />
            <NumberInput
              value={cssNumber(height)}
              numberType='Unitless'
              defaultUnitToHide={null}
              testId='grid-cell-row-height'
              onSubmitValue={onSubmitPosition('height')}
              inputProps={{ placeholder: 'auto' }}
              labelInner={{
                category: 'inspector-element',
                type: 'columnSpan',
                color: 'subdued',
              }}
            />
          </FlexRow>
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

    const onSubmitPosition = React.useCallback(
      (dimension: 'gridRowStart' | 'gridRowEnd' | 'gridColumnStart' | 'gridColumnEnd') =>
        (e: UnknownOrEmptyInput<CSSNumber>) => {
          const value: GridPosition =
            isEmptyInputValue(e) || typeof e.value === 'string'
              ? 'auto'
              : gridPositionValue(e.value)

          const newValues = {
            ...cell.specialSizeMeasurements.elementGridProperties,
            [dimension]: value,
          }
          const commands = setGridPropsCommands(cell.elementPath, gridTemplate, newValues)

          dispatch([applyCommandsAction(commands)])
        },
      [dispatch, cell, gridTemplate],
    )

    return (
      <>
        <UIGridRow padded variant='|--80px--|<--------1fr-------->'>
          <div>Start</div>
          <FlexRow style={{ gap: 4 }}>
            <NumberInput
              value={getValue(cell.specialSizeMeasurements.elementGridProperties.gridColumnStart)}
              numberType='Unitless'
              defaultUnitToHide={null}
              testId='grid-cell-column-start'
              onSubmitValue={onSubmitPosition('gridColumnStart')}
              inputProps={{ placeholder: 'auto' }}
              labelInner={{
                category: 'inspector-element',
                type: 'gridColumn-start',
                color: 'subdued',
              }}
            />
            <NumberInput
              value={getValue(cell.specialSizeMeasurements.elementGridProperties.gridRowStart)}
              numberType='Unitless'
              defaultUnitToHide={null}
              testId='grid-cell-row-start'
              onSubmitValue={onSubmitPosition('gridRowStart')}
              inputProps={{ placeholder: 'auto' }}
              labelInner={{
                category: 'inspector-element',
                type: 'gridRow-start',
                color: 'subdued',
              }}
            />
          </FlexRow>
        </UIGridRow>
        <UIGridRow padded variant='|--80px--|<--------1fr-------->'>
          <div>End</div>
          <FlexRow style={{ gap: 4 }}>
            <NumberInput
              value={getValue(cell.specialSizeMeasurements.elementGridProperties.gridColumnEnd)}
              numberType='Unitless'
              defaultUnitToHide={null}
              testId='grid-cell-column-end'
              onSubmitValue={onSubmitPosition('gridColumnEnd')}
              inputProps={{ placeholder: 'auto' }}
              labelInner={{
                category: 'inspector-element',
                type: 'gridColumn-end',
                color: 'subdued',
              }}
            />
            <NumberInput
              value={getValue(cell.specialSizeMeasurements.elementGridProperties.gridRowEnd)}
              numberType='Unitless'
              defaultUnitToHide={null}
              testId='grid-cell-row-end'
              onSubmitValue={onSubmitPosition('gridRowEnd')}
              inputProps={{ placeholder: 'auto' }}
              labelInner={{
                category: 'inspector-element',
                type: 'gridRow-end',
                color: 'subdued',
              }}
            />
          </FlexRow>
        </UIGridRow>
      </>
    )
  },
)
BoundariesControls.displayName = 'BoundariesControls'

function getValue(pos: GridPosition | null): CSSNumber | null {
  if (pos == null || pos === 'auto' || pos.numericalPosition == null) {
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
