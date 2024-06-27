import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { isFiniteRectangle } from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'

export function getGridPegs(view: ElementPath, metadata: ElementInstanceMetadataMap) {
  const element = MetadataUtils.findElementByElementPath(metadata, view)
  const parent = MetadataUtils.findElementByElementPath(metadata, EP.parentPath(view))

  const targetGridContainer = MetadataUtils.isGridLayoutedContainer(element)
    ? element
    : MetadataUtils.isGridLayoutedContainer(parent)
    ? parent
    : null

  if (
    targetGridContainer == null ||
    targetGridContainer.globalFrame == null ||
    !isFiniteRectangle(targetGridContainer.globalFrame)
  ) {
    return null
  }

  const gap = targetGridContainer.specialSizeMeasurements.gap
  const padding = targetGridContainer.specialSizeMeasurements.padding
  const gridTemplateColumns =
    targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateColumns
  const gridTemplateRows =
    targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateRows

  function getSillyCellsCount(template: string | null) {
    return template == null ? 0 : template.trim().split(/\s+/).length
  }
  const columns = getSillyCellsCount(
    targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateColumns,
  )
  const rows = getSillyCellsCount(
    targetGridContainer.specialSizeMeasurements.containerGridProperties.gridTemplateRows,
  )

  return {
    elementPath: targetGridContainer.elementPath,
    frame: targetGridContainer.globalFrame,
    gridTemplateColumns: gridTemplateColumns,
    gridTemplateRows: gridTemplateRows,
    gap: gap,
    padding: padding,
    cells: rows * columns,
  }
}
