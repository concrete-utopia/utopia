import React from 'react'
import {
  FlexColumn,
  FlexRow,
  Icons,
  ModalityIcons,
  useColorTheme,
  UtopiaStyles,
  UtopiaTheme,
} from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import type { DragToMoveIndicatorFlags } from '../../../editor/store/editor-state'
import { useIsOnlyDoNothingStrategy } from '../../canvas-strategies/canvas-strategies'

export function useGetDragStrategyIndicatorFlags(): {
  indicatorFlags: DragToMoveIndicatorFlags
  dragStarted: boolean
} | null {
  return useEditorState(
    Substores.canvas,
    (store) => {
      if (store.editor.canvas.interactionSession?.interactionData.type === 'DRAG') {
        return {
          indicatorFlags: store.editor.canvas.controls.dragToMoveIndicatorFlags,
          dragStarted: store.editor.canvas.interactionSession?.interactionData.drag != null,
        }
      } else {
        return null
      }
    },
    'useGetStrategyIndicatorFlags',
  )
}

const StrategyIndicatorWidth = 240
export const StrategyIndicator = React.memo(() => {
  const colorTheme = useColorTheme()
  const indicatorFlagsInfo = useGetDragStrategyIndicatorFlags()
  const isOnlyDoNothing = useIsOnlyDoNothingStrategy()

  if (indicatorFlagsInfo == null) {
    // return null
  }

  if (isOnlyDoNothing) {
    return null
  }

  return (
    <FlexRow
      style={{
        marginLeft: 8,
        padding: '0 8px',
        height: 32,
        gap: 10,
        overflow: 'hidden',
        backgroundColor: colorTheme.bg1subdued.value,
        borderRadius: '0px 10px 10px 10px',
        boxShadow: UtopiaStyles.shadowStyles.low.boxShadow,
        pointerEvents: 'initial',
        zIndex: -1,
      }}
      data-testid='drag-strategy-indicator'
    >
      <MoveReorderReparentIndicator />
      <AncestorIndicatorItem enabled={indicatorFlagsInfo?.indicatorFlags.ancestor ?? false} />
    </FlexRow>
  )
})

export const MoveReorderReparentIndicatorID = 'move-reorder-reparent-indicator'

const MoveReorderReparentIndicator = React.memo(() => {
  const indicatorText = useEditorState(
    Substores.restOfStore,
    (store) => store.strategyState.currentStrategyDescriptiveLabel ?? 'Interaction',
    'MoveReorderReparentIndicator currentStrategyState',
  )
  const colorTheme = useColorTheme()
  return (
    <FlexRow
      style={{
        height: 32,
        color: colorTheme.dynamicBlue.value,
        padding: '0 8px',
      }}
      data-testid={MoveReorderReparentIndicatorID}
    >
      {indicatorText}
    </FlexRow>
  )
})

interface IndicatorItemProps {
  enabled: boolean
}
const AncestorIndicatorItem = React.memo<IndicatorItemProps>((props) => {
  return (
    <FlexRow style={{ alignItems: 'center', paddingRight: 8 }}>
      <div
        style={{
          padding: 7,
        }}
      >
        <VisibilityWrapper visible={props.enabled}>
          <ModalityIcons.Magic color={'main'} />
        </VisibilityWrapper>
      </div>
    </FlexRow>
  )
})

interface VisibilityWrapperProps {
  visible: boolean
}
const VisibilityWrapper = React.memo<React.PropsWithChildren<VisibilityWrapperProps>>((props) => {
  return (
    <div style={{ opacity: props.visible ? 1 : 0, height: props.visible ? undefined : 0 }}>
      {props.children}
    </div>
  )
})
