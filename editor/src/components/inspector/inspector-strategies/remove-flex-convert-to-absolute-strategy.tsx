import { CSSProperties } from 'react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { CanvasCommand } from '../../canvas/commands/commands'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import { setProperty } from '../../canvas/commands/set-property-command'
import { filterKeepFlexContainers, nullOrNonEmpty } from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'

const styleP = (prop: keyof CSSProperties) => PP.create(['style', prop])

const flexContainerProps = [
  styleP('flexDirection'),
  styleP('flexWrap'),
  styleP('gap'),
  styleP('display'),
  styleP('alignItems'),
  styleP('justifyContent'),
]

const flexChildProps = [styleP('flex'), styleP('flexGrow'), styleP('flexShrink')]

function pruneFlexPropsCommands(
  props: PropertyPath[],
  elementPath: ElementPath,
): Array<CanvasCommand> {
  return [deleteProperties('always', elementPath, props)]
}

function positionAbsoluteCommands(elementPath: ElementPath): Array<CanvasCommand> {
  return [setProperty('always', elementPath, styleP('position'), 'absolute')]
}

function sizeToVisualDimensions(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return []
  }

  const left = element.specialSizeMeasurements.offset.x
  const top = element.specialSizeMeasurements.offset.y
  const width = element.specialSizeMeasurements.clientWidth
  const height = element.specialSizeMeasurements.clientHeight

  return [
    setProperty('always', elementPath, styleP('left'), left),
    setProperty('always', elementPath, styleP('top'), top),
    setProperty('always', elementPath, styleP('width'), width),
    setProperty('always', elementPath, styleP('height'), height),
  ]
}

function removeFlexConvertToAbsoluteOne(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const children = MetadataUtils.getChildrenPaths(metadata, elementPath)
  return [
    ...pruneFlexPropsCommands(flexContainerProps, elementPath), // flex-related stuff is pruned
    ...children.flatMap((c) => positionAbsoluteCommands(c)), // all children are converted to absolute,
    ...children.flatMap((c) => sizeToVisualDimensions(metadata, c)), // with width/height based on measured dimensions
    ...children.flatMap((c) => pruneFlexPropsCommands(flexChildProps, c)),
    ...sizeToVisualDimensions(metadata, elementPath), // container is sized to keep its visual dimensions
  ]
}

export const removeFlexConvertToAbsolute: InspectorStrategy = (metadata, elementPaths) => {
  const commands = filterKeepFlexContainers(metadata, elementPaths).flatMap((path) =>
    removeFlexConvertToAbsoluteOne(metadata, path),
  )
  return nullOrNonEmpty(commands)
}
