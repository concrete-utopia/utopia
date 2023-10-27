import type { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { CanvasPoint } from '../../../core/shared/math-utils'
import type { NodeModules, ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { IndexPosition } from '../../../utils/utils'
import { front } from '../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../assets'
import { getAbsoluteReparentPropertyChanges } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-property-changes'
import { autoLayoutParentAbsoluteOrStatic } from '../../canvas/canvas-strategies/strategies/reparent-helpers/reparent-strategy-parent-lookup'
import type { ToReparent } from '../../canvas/canvas-strategies/strategies/reparent-utils'
import { getReparentOutcome } from '../../canvas/canvas-strategies/strategies/reparent-utils'
import { foldAndApplyCommandsInner } from '../../canvas/commands/commands'
import { setCssLengthProperty } from '../../canvas/commands/set-css-length-command'
import { setProperty } from '../../canvas/commands/set-property-command'
import { updateFunctionCommand } from '../../canvas/commands/update-function-command'
import { updateSelectedViews } from '../../canvas/commands/update-selected-views-command'
import { cssNumber } from '../../inspector/common/css-utils'
import type { CustomInspectorStrategy } from '../../inspector/inspector-strategies/inspector-strategy'
import type { AllElementProps } from '../store/editor-state'
import type { InsertionPath } from '../store/insertion-path'

export interface InsertAsAbsoluteOptions {
  toPosition: CanvasPoint | null
  indexPosition: IndexPosition
}

const DefaultOptions: InsertAsAbsoluteOptions = {
  toPosition: null,
  indexPosition: front(),
}

export const insertAsAbsoluteStrategy = (
  element: ToReparent,
  parentInsertionPath: InsertionPath,
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  options: Partial<InsertAsAbsoluteOptions> = DefaultOptions,
): CustomInspectorStrategy<{ newPath: ElementPath }> => ({
  name: 'Insert as absolute',
  strategy: (
    metadata: ElementInstanceMetadataMap,
    _: Array<ElementPath>,
    elementPathTree: ElementPathTrees,
    allElementProps: AllElementProps,
  ) => {
    const { indexPosition, toPosition }: InsertAsAbsoluteOptions = { ...DefaultOptions, ...options }
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
      indexPosition,
    )

    if (result == null) {
      return null
    }

    return {
      commands: [
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
              ),
              setProperty('always', result.newPath, PP.create('style', 'position'), 'absolute'),
              ...(toPosition == null
                ? []
                : [
                    setCssLengthProperty(
                      'always',
                      result.newPath,
                      PP.create('style', 'top'),
                      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(toPosition.y, null) },
                      null,
                    ),
                    setCssLengthProperty(
                      'always',
                      result.newPath,
                      PP.create('style', 'left'),
                      { type: 'EXPLICIT_CSS_NUMBER', value: cssNumber(toPosition.x, null) },
                      null,
                    ),
                  ]),
            ],
            commandLifecycle,
          ).statePatches
        }),
      ],
      data: { newPath: result.newPath },
    }
  },
})
