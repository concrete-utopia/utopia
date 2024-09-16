import type { CanvasCommand } from '../../canvas/commands/commands'
import type { EditorDispatch } from '../../editor/action-types'
import { applyCommandsAction, transientActions } from '../../editor/actions/action-creators'
import type { ElementsToRerender } from '../../editor/store/editor-state'

interface CustomInspectorStrategyResultBase {
  commands: Array<CanvasCommand>
}

export type CustomInspectorStrategyResult<T extends undefined | Record<string, unknown>> =
  T extends undefined
    ? CustomInspectorStrategyResultBase
    : CustomInspectorStrategyResultBase & { data: T }

export interface CustomInspectorStrategy<T extends undefined | Record<string, unknown>> {
  name: string
  strategy: () => CustomInspectorStrategyResult<T> | null
}

export interface InspectorStrategy {
  name: string
  strategy: () => Array<CanvasCommand> | null
}

export function resultForFirstApplicableStrategy<T extends undefined | Record<string, unknown>>(
  strategies: Array<CustomInspectorStrategy<T>>,
): CustomInspectorStrategyResult<T> | null {
  for (const strategy of strategies) {
    const result = strategy.strategy()
    if (result != null) {
      return result
    }
  }
  return null
}

export function commandsForFirstApplicableStrategy(
  strategies: Array<InspectorStrategy>,
): Array<CanvasCommand> | null {
  for (const strategy of strategies) {
    const commands = strategy.strategy()
    if (commands != null) {
      return commands
    }
  }
  return null
}

export function executeFirstApplicableStrategy(
  dispatch: EditorDispatch,

  strategies: InspectorStrategy[],
  elementsToRerenderTransient: ElementsToRerender = 'rerender-all-elements',
): void {
  const commands = commandsForFirstApplicableStrategy(strategies)
  if (commands != null) {
    if (elementsToRerenderTransient !== 'rerender-all-elements') {
      dispatch([transientActions([applyCommandsAction(commands)], elementsToRerenderTransient)])
    } else {
      dispatch([applyCommandsAction(commands)])
    }
  }
}
