import * as EP from '../../../core/shared/element-path'
import type {
  EditorState,
  EditorStatePatch,
  ElementsToRerender,
} from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction } from './commands'

export interface SetElementsToRerenderCommand extends BaseCommand {
  type: 'SET_ELEMENTS_TO_RERENDER_COMMAND'
  value: ElementsToRerender
}

export function setElementsToRerenderCommand(
  value: ElementsToRerender,
): SetElementsToRerenderCommand {
  const uniqueValues = value === 'rerender-all-elements' ? value : EP.uniqueElementPaths(value)
  return {
    type: 'SET_ELEMENTS_TO_RERENDER_COMMAND',
    whenToRun: 'always',
    value: uniqueValues,
  }
}

export const runSetElementsToRerender: CommandFunction<SetElementsToRerenderCommand> = (
  e: EditorState,
  command: SetElementsToRerenderCommand,
) => {
  const editorStatePatch: EditorStatePatch = {
    canvas: {
      elementsToRerender: { $set: command.value },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Set Elements To Rerender: [${
      typeof command.value === 'string' ? command.value : command.value.map(EP.toString).join(', ')
    }]`,
  }
}

// NOTE: You probably want to use the command above! Since we don't explicitly clear the current
// value of canvas.elementsToRerender, and the default value is 'render-all-elements', if you use
// this command in the wrong place you can very easily end up re-rendering everything
export interface AppendElementsToRerenderCommand extends BaseCommand {
  type: 'APPEND_ELEMENTS_TO_RERENDER_COMMAND'
  value: ElementsToRerender
}

export function appendElementsToRerenderCommand(
  value: ElementsToRerender,
): AppendElementsToRerenderCommand {
  return {
    type: 'APPEND_ELEMENTS_TO_RERENDER_COMMAND',
    whenToRun: 'mid-interaction',
    value: value,
  }
}

function mergeElementsToRerender(l: ElementsToRerender, r: ElementsToRerender): ElementsToRerender {
  if (l === 'rerender-all-elements' || r === 'rerender-all-elements') {
    return 'rerender-all-elements'
  } else {
    return [...l, ...r]
  }
}

export const runAppendElementsToRerender: CommandFunction<AppendElementsToRerenderCommand> = (
  e: EditorState,
  command: AppendElementsToRerenderCommand,
) => {
  const editorStatePatch: EditorStatePatch = {
    canvas: {
      elementsToRerender: {
        $apply: (current: ElementsToRerender) => mergeElementsToRerender(current, command.value),
      },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Append Elements To Rerender: [${
      typeof command.value === 'string' ? command.value : command.value.map(EP.toString).join(', ')
    }]`,
  }
}
