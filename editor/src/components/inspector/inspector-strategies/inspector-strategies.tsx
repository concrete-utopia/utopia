import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { CanvasCommand } from '../../canvas/commands/commands'
import { setProperty } from '../../canvas/commands/set-property-command'
import { EditorDispatch } from '../../editor/action-types'
import {
  fillContainerApplicable,
  filterKeepFlexContainers,
  FlexAlignment,
  FlexJustifyContent,
  hugContentsApplicable,
} from '../inspector-common'
import { applyCommandsAction } from '../../editor/actions/action-creators'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import { CSSNumber, FlexDirection, printCSSNumber } from '../common/css-utils'
import { InspectorStrategy } from './inspector-strategy'

export const setFlexAlignJustifyContentStrategies = (
  flexAlignment: FlexAlignment,
  justifyContent: FlexJustifyContent,
): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = filterKeepFlexContainers(metadata, elementPaths)

    if (elements.length === 0) {
      return null
    }

    return elements.flatMap((path) => [
      setProperty('always', path, PP.create(['style', 'alignItems']), flexAlignment),
      setProperty('always', path, PP.create(['style', 'justifyContent']), justifyContent),
    ])
  },
]

export const removeFlexDirectionStrategies = (): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = filterKeepFlexContainers(metadata, elementPaths)

    if (elements.length === 0) {
      return null
    }

    return elements.map((path) =>
      deleteProperties('always', path, [PP.create(['style', 'flexDirection'])]),
    )
  },
]

export const updateFlexDirectionStrategies = (
  flexDirection: FlexDirection,
): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = filterKeepFlexContainers(metadata, elementPaths)

    if (elements.length === 0) {
      return null
    }

    return elements.map((path) =>
      setProperty('always', path, PP.create(['style', 'flexDirection']), flexDirection),
    )
  },
]

export const addFlexLayoutStrategies: Array<InspectorStrategy> = [
  (metadata, elementPaths) => {
    return elementPaths.map((path) =>
      setProperty('always', path, PP.create(['style', 'display']), 'flex'),
    )
  },
]

export const removeFlexLayoutStrategies: Array<InspectorStrategy> = [
  (metadata, elementPaths) => {
    const elements = filterKeepFlexContainers(metadata, elementPaths)

    if (elements.length === 0) {
      return null
    }

    return elements.map((path) =>
      deleteProperties('always', path, [PP.create(['style', 'display'])]),
    )
  },
]

export const setPropFillStrategies = (prop: 'width' | 'height'): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = elementPaths.filter(fillContainerApplicable)

    if (elements.length === 0) {
      return null
    }

    return elements.map((path) => setProperty('always', path, PP.create(['style', prop]), '100%'))
  },
]

export const setPropFixedStrategies = (
  prop: 'width' | 'height',
  value: CSSNumber,
): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    if (elementPaths.length === 0) {
      return null
    }

    return elementPaths.map((path) =>
      setProperty('always', path, PP.create(['style', prop]), printCSSNumber(value, null)),
    )
  },
]

export const setPropHugStrategies = (prop: 'width' | 'height'): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = elementPaths.filter((path) => hugContentsApplicable(metadata, path))

    if (elements.length === 0) {
      return null
    }

    return elements.map((path) =>
      setProperty('always', path, PP.create(['style', prop]), 'min-content'),
    )
  },
]
