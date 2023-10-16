import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { NodeModules, ElementPath } from '../../../core/shared/project-file-types'
import type { IndexPosition } from '../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../assets'
import { autoLayoutParentAbsoluteOrStatic } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-strategy-parent-lookup'
import type { PathToReparent } from '../../canvas/canvas-strategies/strategies/reparent-utils'
import type { AllElementProps } from '../store/editor-state'
import type { InsertionPath } from '../store/insertion-path'
import type { MoveInspectorStrategy } from './convert-to-absolute-and-move'
import { createAbsoluteReparentAndOffsetCommands } from '../../canvas/canvas-strategies/strategies/absolute-reparent-strategy'

export const moveAsAbsoluteStrategy = (
  element: PathToReparent,
  parentInsertionPath: InsertionPath,
  indexPosition: IndexPosition,
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
): MoveInspectorStrategy => ({
  name: 'Move as absolute',
  strategy: (
    metadata: ElementInstanceMetadataMap,
    _: Array<ElementPath>,
    elementPathTree: ElementPathTrees,
    allElementProps: AllElementProps,
  ) => {
    const shouldReparentAsAbsoluteOrStatic = autoLayoutParentAbsoluteOrStatic(
      metadata,
      allElementProps,
      elementPathTree,
      EP.parentPath(element.target),
      'prefer-absolute',
    )

    if (shouldReparentAsAbsoluteOrStatic !== 'REPARENT_AS_ABSOLUTE') {
      return null
    }

    const result = createAbsoluteReparentAndOffsetCommands(
      element.target,
      parentInsertionPath,
      indexPosition,
      metadata,
      elementPathTree,
      allElementProps,
      builtInDependencies,
      projectContents,
      nodeModules,
      'force-pins',
    )

    if (result == null) {
      return null
    }

    return {
      commands: result.commands,
      data: result.newPath,
    }
  },
})
