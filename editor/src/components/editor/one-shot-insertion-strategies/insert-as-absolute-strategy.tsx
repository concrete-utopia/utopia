import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { NodeModules } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { front } from '../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../assets'
import { getAbsoluteReparentPropertyChanges } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-property-changes'
import { autoLayoutParentAbsoluteOrStatic } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-strategy-parent-lookup'
import type { ElementToReparent } from '../../canvas/canvas-strategies/strategies/reparent-utils'
import { getReparentOutcome } from '../../canvas/canvas-strategies/strategies/reparent-utils'
import { foldAndApplyCommandsInner } from '../../canvas/commands/commands'
import { setProperty } from '../../canvas/commands/set-property-command'
import { updateFunctionCommand } from '../../canvas/commands/update-function-command'
import { updateSelectedViews } from '../../canvas/commands/update-selected-views-command'
import type { InspectorStrategy } from '../../inspector/inspector-strategies/inspector-strategy'
import type { AllElementProps } from '../store/editor-state'
import type { InsertionPath } from '../store/insertion-path'

export const insertAsAbsoluteStrategy = (
  element: ElementToReparent,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  parentInsertionPath: InsertionPath,
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
): InspectorStrategy => ({
  name: 'Insert as absolute',
  strategy: () => {
    const shouldReparentAsAbsoluteOrStatic = autoLayoutParentAbsoluteOrStatic(
      metadata,
      allElementProps,
      elementPathTree,
      parentInsertionPath.intendedParentPath,
      'prefer-absolute',
    )
    if (shouldReparentAsAbsoluteOrStatic !== 'REPARENT_AS_ABSOLUTE') {
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

    return [
      ...result.commands,
      updateFunctionCommand('always', (state, commandLifecycle) => {
        return foldAndApplyCommandsInner(
          state,
          [],
          [
            ...getAbsoluteReparentPropertyChanges(
              result.newPath,
              EP.parentPath(result.newPath),
              metadata,
              metadata,
              state.projectContents,
              'force-pins',
              'should-not-add',
            ),
            setProperty('always', result.newPath, PP.create('style', 'position'), 'absolute'),
          ],
          commandLifecycle,
        ).statePatches
      }),
      updateSelectedViews('always', [result.newPath]),
    ]
  },
})
