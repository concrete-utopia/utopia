import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  getLocalRectangleInNewParentContext,
  isFiniteRectangle,
} from '../../../core/shared/math-utils'
import type { NodeModules, ElementPath } from '../../../core/shared/project-file-types'
import { front } from '../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../assets'
import { autoLayoutParentAbsoluteOrStatic } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-strategy-parent-lookup'
import type { PathToReparent } from '../../canvas/canvas-strategies/strategies/reparent-utils'
import { getReparentOutcome } from '../../canvas/canvas-strategies/strategies/reparent-utils'
import { updateSelectedViews } from '../../canvas/commands/update-selected-views-command'
import { getConvertIndividualElementToAbsoluteCommands } from '../../inspector/inspector-common'
import type { CustomInspectorStrategy } from '../../inspector/inspector-strategies/inspector-strategy'
import type { AllElementProps } from '../store/editor-state'
import type { InsertionPath } from '../store/insertion-path'

export type MoveInspectorStrategy = CustomInspectorStrategy<ElementPath>

export const convertToAbsoluteAndMoveStrategy = (
  element: PathToReparent,
  parentInsertionPath: InsertionPath,
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
): MoveInspectorStrategy => ({
  name: 'Convert to absolute and move',
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

    if (shouldReparentAsAbsoluteOrStatic !== 'REPARENT_AS_STATIC') {
      return null
    }

    const result = getReparentOutcome(
      metadata,
      elementPathTree,
      allElementProps,
      builtInDependencies,
      projectContents,
      nodeModules,
      element,
      parentInsertionPath,
      'always',
      front(),
    )

    if (result == null) {
      return null
    }

    const targetMetadata = MetadataUtils.findElementByElementPath(metadata, element.target)
    const parentMetadata = MetadataUtils.findElementByElementPath(
      metadata,
      parentInsertionPath.intendedParentPath,
    )

    const escapeHatchCommands =
      parentMetadata?.globalFrame != null &&
      targetMetadata?.globalFrame != null &&
      isFiniteRectangle(targetMetadata.globalFrame) &&
      isFiniteRectangle(parentMetadata.globalFrame)
        ? getConvertIndividualElementToAbsoluteCommands(
            element.target,
            metadata,
            elementPathTree,
            getLocalRectangleInNewParentContext(
              parentMetadata.globalFrame,
              targetMetadata.globalFrame,
            ),
            null,
          )
        : []

    return {
      commands: [...escapeHatchCommands, ...result.commands],
      data: result.newPath,
    }
  },
})
