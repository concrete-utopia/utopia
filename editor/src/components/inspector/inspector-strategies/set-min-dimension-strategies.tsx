import { CSSNumber, parseCSSLengthPercent, printCSSNumber } from '../common/css-utils'
import { InspectorStrategy } from './inspector-strategy'
import * as PP from '../../../core/shared/property-path'
import { setProperty } from '../../canvas/commands/set-property-command'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import { assertNever } from '../../../core/shared/utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { cssNumberEqual } from '../../canvas/controls/select-mode/controls-common'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { defaultEither } from '../../../core/shared/either'

function cssNumberOrNullEqual(left: CSSNumber | null, right: CSSNumber | null): boolean {
  if (left == null && right == null) {
    return true
  }
  if (left == null || right == null) {
    return false
  }
  return cssNumberEqual(left, right)
}

const detectMinMaxDimensionForSingleElement =
  (dimension: MinMaxDimension) =>
  (metadata: ElementInstanceMetadataMap, selectedElement: ElementPath): CSSNumber | null => {
    const element = MetadataUtils.findElementByElementPath(metadata, selectedElement)
    return defaultEither(
      null,
      parseCSSLengthPercent(element?.computedStyle?.[cssPropFromMinMaxDimension(dimension)]),
    )
  }

export const detectMinMaxDimension =
  (dimension: MinMaxDimension) =>
  (
    metadata: ElementInstanceMetadataMap,
    selectedElements: Array<ElementPath>,
  ): CSSNumber | null => {
    const measurements = selectedElements.map((e) =>
      detectMinMaxDimensionForSingleElement(dimension)(metadata, e),
    )
    const allEqual =
      measurements.length > 0 && measurements.every((e) => cssNumberOrNullEqual(e, measurements[0]))
    return allEqual ? measurements[0] : null
  }

export type MinMaxDimension = `${'min' | 'max'}-${'width' | 'height'}`

function cssPropFromMinMaxDimension(dimension: MinMaxDimension): string {
  switch (dimension) {
    case 'max-height':
      return 'maxHeight'
    case 'min-height':
      return 'minHeight'
    case 'max-width':
      return 'maxWidth'
    case 'min-width':
      return 'minWidth'
    default:
      assertNever(dimension)
  }
}

export const setMinMaxDimensionBasicStrategy = (
  dimension: MinMaxDimension,
  value: CSSNumber,
): InspectorStrategy => ({
  name: `Set ${dimension}`,
  strategy: (metadata, elementPaths) =>
    elementPaths.map((elementPath) =>
      setProperty(
        'always',
        elementPath,
        PP.create('style', cssPropFromMinMaxDimension(dimension)),
        printCSSNumber(value, null),
      ),
    ),
})

export const unsetMinMaxDimensionBasicStrategy = (
  dimension: MinMaxDimension,
): InspectorStrategy => ({
  name: `Set ${dimension}`,
  strategy: (metadata, elementPaths) =>
    elementPaths.map((elementPath) =>
      deleteProperties('always', elementPath, [
        PP.create('style', cssPropFromMinMaxDimension(dimension)),
      ]),
    ),
})
