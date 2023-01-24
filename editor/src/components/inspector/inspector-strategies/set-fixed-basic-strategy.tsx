import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import { assertNever } from '../../../core/shared/utils'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import { setProperty } from '../../canvas/commands/set-property-command'
import { CSSNumber, FlexDirection, printCSSNumber } from '../common/css-utils'
import { Axis, nullOrNonEmpty } from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'
import * as PP from '../../../core/shared/property-path'
import { CanvasCommand, WhenToRun } from '../../canvas/commands/commands'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { CSSProperties } from 'twind'

const styleProp = (prop: keyof CSSProperties) => PP.create(['style', prop])

function getMinMaxPropertiesToDelete(axis: Axis, elementMetadata: ElementInstanceMetadata) {
  if (elementMetadata.specialSizeMeasurements.parentLayoutSystem !== 'flex') {
    return []
  }

  switch (axis) {
    case 'horizontal':
      return [styleProp('minWidth'), styleProp('maxWidth')]
    case 'vertical':
      return [styleProp('minHeight'), styleProp('maxHeight')]
    default:
      assertNever(axis)
  }
}

function getFlexPropsToDelete(axis: Axis, elementMetadata: ElementInstanceMetadata) {
  if (elementMetadata.specialSizeMeasurements.parentLayoutSystem !== 'flex') {
    return []
  }
  const flexDirection: FlexDirection =
    elementMetadata.specialSizeMeasurements.parentFlexDirection ?? 'row'

  if (
    (flexDirection.startsWith('row') && axis === 'horizontal') ||
    (flexDirection.startsWith('column') && axis === 'vertical')
  ) {
    return [
      styleProp('flex'),
      styleProp('flexBasis'),
      styleProp('flexShrink'),
      styleProp('flexGrow'),
    ]
  }

  return []
}

function setElementToFixedCommands(
  axis: Axis,
  whenToRun: WhenToRun,
  value: CSSNumber,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return []
  }

  const propertiesToDelete: Array<PropertyPath> = [
    ...getMinMaxPropertiesToDelete(axis, element),
    ...getFlexPropsToDelete(axis, element),
  ]

  switch (axis) {
    case 'horizontal':
      return [
        deleteProperties(whenToRun, elementPath, propertiesToDelete),
        setProperty(
          whenToRun,
          elementPath,
          PP.create(['style', 'width']),
          printCSSNumber(value, null),
        ),
      ]
    case 'vertical':
      return [
        deleteProperties(whenToRun, elementPath, propertiesToDelete),
        setProperty(
          whenToRun,
          elementPath,
          PP.create(['style', 'height']),
          printCSSNumber(value, null),
        ),
      ]
    default:
      assertNever(axis)
  }
}

export const setFixedBasicStrategy = (
  whenToRun: WhenToRun,
  axis: Axis,
  value: CSSNumber,
): InspectorStrategy => ({
  name: 'Set to Fixed',
  strategy: (metadata, elementPaths) => {
    if (elementPaths.length === 0) {
      return null
    }

    const commands = elementPaths.flatMap((path) =>
      setElementToFixedCommands(axis, whenToRun, value, metadata, path),
    )
    return nullOrNonEmpty(commands)
  },
})
