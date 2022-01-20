import { sortBy } from '../../../core/shared/array-utils'
import {
  EditorState,
  transientCanvasState,
  TransientCanvasState,
  transientCanvasStateForSession,
} from '../../editor/store/editor-state'
import {
  CanvasStrategy,
  CanvasStrategyUpdateFn,
  emptySelectModeCanvasSessionState,
  SelectModeCanvasSession,
  SelectModeCanvasSessionProps,
  SelectModeCanvasSessionState,
} from './canvas-strategy-types'
import { flexAlignParentStrategy } from './flex-align-parent-strategy'
import { flexGapStrategy } from './flex-gap-strategy'

const RegisteredCanvasStrategies: Array<CanvasStrategy> = [flexGapStrategy, flexAlignParentStrategy]

interface StrategiesWithFitness {
  fitness: number
  strategy: CanvasStrategy
}

function getApplicableStrategies(
  editorState: EditorState,
  sessionProps: SelectModeCanvasSessionProps,
  sessionState: SelectModeCanvasSessionState,
): Array<StrategiesWithFitness> {
  const applicableStrategies = RegisteredCanvasStrategies.map((s) => ({
    fitness: s.fitnessFn(editorState, sessionProps, sessionState),
    strategy: s,
  })).filter((s) => {
    // discard strategies with 0 fitness
    return s.fitness > 0
  })

  return sortBy(applicableStrategies, (l, r) => {
    // sort by fitness, descending
    return r.fitness - l.fitness
  })
}

export function pickDefaultCanvasStrategy(
  applicableStrategies: Array<StrategiesWithFitness>,
): CanvasStrategy | null {
  return applicableStrategies[0]?.strategy ?? null
}

function pickStrategy(
  sessionProps: SelectModeCanvasSessionProps,
  applicableStrategies: Array<StrategiesWithFitness>,
): CanvasStrategy | null {
  if (sessionProps.userPreferredStrategy != null) {
    const foundStrategyByName = applicableStrategies.find(
      (s) => s.strategy.name === sessionProps.userPreferredStrategy,
    )
    if (foundStrategyByName != null) {
      return foundStrategyByName.strategy
    }
  }
  // fall back to default strategy
  return pickDefaultCanvasStrategy(applicableStrategies)
}

export function applyCanvasStrategy(
  lifecycle: 'transient' | 'final',
  editorState: EditorState,
  canvasSession: SelectModeCanvasSession,
  canvasSessionState: SelectModeCanvasSessionState | null,
): TransientCanvasState | null {
  const sessionProps = canvasSession.sessionProps
  const sessionState = canvasSessionState ?? emptySelectModeCanvasSessionState

  const applicableStrategies = getApplicableStrategies(editorState, sessionProps, sessionState)
  const strategy = pickStrategy(sessionProps, applicableStrategies)
  const sessionStateWithStrategy: SelectModeCanvasSessionState = {
    ...sessionState,
    activeStrategy: strategy,
    possibleStrategies: applicableStrategies.map((s) => s.strategy),
  }

  const result =
    strategy?.updateFn?.(lifecycle, editorState, sessionProps, sessionStateWithStrategy) ?? null

  if (result == null) {
    // no strategy was active, return empty result
    return transientCanvasStateForSession(canvasSessionState, null, null)
  } else {
    return transientCanvasStateForSession(
      result.newSessionState,
      result.transientFilesState,
      result.editorStatePatch,
    )
  }
}
