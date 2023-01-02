import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { CanvasCommand } from '../canvas/commands/commands'
import { setProperty } from '../canvas/commands/set-property-command'
import { EditorDispatch } from '../editor/action-types'
import {
  fillContainerApplicable,
  filterKeepFlexContainers,
  FlexAlignment,
  FlexJustifyContent,
  hugContentsApplicable,
} from './inspector-common'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { deleteProperties } from '../canvas/commands/delete-properties-command'
import { CSSNumber, FlexDirection, printCSSNumber } from './common/css-utils'
import { MetadataUtils } from '../../core/model/element-metadata-utils'

export type InspectorStrategy = (
  metadata: ElementInstanceMetadataMap,
  selectedElementPaths: Array<ElementPath>,
) => Array<CanvasCommand> | null

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

export type Axis = 'horizontal' | 'vertical'

function widthHeightFromAxis(axis: Axis): 'width' | 'height' {
  switch (axis) {
    case 'horizontal':
      return 'width'
    case 'vertical':
      return 'height'
  }
}

export const setPropFillStrategies = (axis: Axis): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = elementPaths.filter(fillContainerApplicable)

    if (elements.length === 0) {
      return null
    }

    return elements.map((path) =>
      setProperty('always', path, PP.create(['style', widthHeightFromAxis(axis)]), '100%'),
    )
  },
]

export const setPropFixedStrategies = (axis: Axis, value: CSSNumber): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    if (elementPaths.length === 0) {
      return null
    }

    return elementPaths.map((path) =>
      setProperty(
        'always',
        path,
        PP.create(['style', widthHeightFromAxis(axis)]),
        printCSSNumber(value, null),
      ),
    )
  },
]

export const setPropHugStrategies = (axis: Axis): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = elementPaths.filter((path) => hugContentsApplicable(metadata, path))

    if (elements.length === 0) {
      return null
    }

    return elements.map((path) =>
      setProperty('always', path, PP.create(['style', widthHeightFromAxis(axis)]), 'min-content'),
    )
  },
]

export function runStrategies(
  dispatch: EditorDispatch,
  metadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
  strategies: InspectorStrategy[],
): void {
  for (const strategy of strategies) {
    const commands = strategy(metadata, selectedViews)
    if (commands != null) {
      dispatch([applyCommandsAction(commands)])
    }
    return
  }
}
