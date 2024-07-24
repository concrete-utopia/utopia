import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  getLocalRectangleInNewParentContext,
  isFiniteRectangle,
} from '../../../core/shared/math-utils'
import type { NodeModules } from '../../../core/shared/project-file-types'
import type { IndexPosition } from '../../../utils/utils'
import { front } from '../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../assets'
import { autoLayoutParentAbsoluteOrStatic } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-strategy-parent-lookup'
import type { PathToReparent } from '../../canvas/canvas-strategies/strategies/reparent-utils'
import { getReparentOutcome } from '../../canvas/canvas-strategies/strategies/reparent-utils'
import { reorderElement } from '../../canvas/commands/reorder-element-command'
import { getConvertIndividualElementToAbsoluteCommands } from '../../inspector/inspector-common'
import type { AllElementProps } from '../store/editor-state'
import type { InsertionPath } from '../store/insertion-path'
import type { UnwrapInspectorStrategy } from './unwrap-strategies-common'

export const convertToAbsoluteAndReparentToUnwrapStrategy = (
  element: PathToReparent,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  parentInsertionPath: InsertionPath,
  indexPosition: IndexPosition,
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
): UnwrapInspectorStrategy => ({
  name: 'Convert to absolute and reparent to unwrap',
  strategy: () => {
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
      commands: [
        ...escapeHatchCommands,
        ...result.commands,
        reorderElement('on-complete', result.newPath, indexPosition),
      ],
      data: {
        newPath: result.newPath,
      },
    }
  },
})
