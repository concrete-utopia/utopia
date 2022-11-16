import React from 'react'
import { stripNulls } from '../../../../core/shared/array-utils'
import { RenderControlMemoized } from '../../controls/new-canvas-controls'
import { ShowHideControl } from '../../controls/select-mode/show-hide-control'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { setBorderRadiusStrategy } from './set-border-radius-strategy'
import { setFlexGapStrategy } from './set-flex-gap-strategy'
import { setPaddingStrategy } from './set-padding-strategy'

export const stylePropHandlesMetaStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
) => {
  const setPaddingStrategyResult = setPaddingStrategy(
    canvasState,
    interactionSession,
    customStrategyState,
  )
  const setFlexGapStrategyResult = setFlexGapStrategy(
    canvasState,
    interactionSession,
    customStrategyState,
  )
  const setBorderRadiusStrategyResult = setBorderRadiusStrategy(
    canvasState,
    interactionSession,
    customStrategyState,
  )

  if (
    setPaddingStrategyResult == null &&
    setFlexGapStrategyResult == null &&
    setBorderRadiusStrategyResult == null
  ) {
    return null
  }

  if (interactionSession?.activeControl.type === 'PADDING_RESIZE_HANDLE') {
    return setPaddingStrategyResult
  }

  if (interactionSession?.activeControl.type === 'FLEX_GAP_HANDLE') {
    return setFlexGapStrategyResult
  }

  if (interactionSession?.activeControl.type === 'BORDER_RADIUS_RESIZE_HANDLE') {
    return setBorderRadiusStrategyResult
  }

  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  const applicableControls: React.ReactNode[] = stripNulls([
    setPaddingStrategyResult,
    setFlexGapStrategyResult,
    setBorderRadiusStrategyResult,
  ])
    .flatMap((s) => s.controlsToRender)
    .map((c) => (
      <RenderControlMemoized key={c.key} control={c.control.control} propsForControl={c.props} />
    ))

  return {
    id: 'ADJUST_PROPS',
    name: 'Adjust properties',
    controlsToRender: [
      controlWithProps({
        control: ShowHideControl,
        props: { children: applicableControls, selectedElements: selectedElements },
        key: 'show-hide-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    fitness: 1,
    apply: () => emptyStrategyApplicationResult,
  }
}
