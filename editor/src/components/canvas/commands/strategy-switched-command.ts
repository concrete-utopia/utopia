import type { BaseCommand, CommandFunctionResult, PathMappings } from './commands'

export interface StrategySwitched extends BaseCommand {
  type: 'STRATEGY_SWITCHED'
  reason: 'automatic' | 'user-input'
  newStrategy: string
  dataReset: boolean
  previousFitness: number
  newFitness: number
}

export function strategySwitched(
  reason: 'automatic' | 'user-input',
  newStrategy: string,
  dataReset: boolean,
  previousFitness: number,
  newFitness: number,
): StrategySwitched {
  return {
    type: 'STRATEGY_SWITCHED',
    transient: 'transient',
    reason,
    newStrategy,
    dataReset,
    previousFitness,
    newFitness,
  }
}

export function runStrategySwitchedCommand(
  pathMappings: PathMappings,
  command: StrategySwitched,
): CommandFunctionResult {
  let commandDescription: string = `Strategy switched to ${command.newStrategy} ${
    command.reason === 'automatic'
      ? `automatically (fitness ${command.previousFitness} -> ${command.newFitness})`
      : 'by user input'
  }. ${command.dataReset ? 'Interaction data reset.' : ''}`

  return {
    editorStatePatch: {},
    pathMappings: pathMappings,
    commandDescription: commandDescription,
  }
}
