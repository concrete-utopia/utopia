import React from 'react'
import {
  FlexColumn,
  FlexRow,
  ModalityIcons,
  useColorTheme,
  UtopiaStyles,
  UtopiaTheme,
} from '../../../../uuiui'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import type { DragToMoveIndicatorFlags } from '../../../editor/store/editor-state'

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

  if (indicatorFlagsInfo == null) {
    // return null
  }

  return (
    <FlexRow
      style={{
        pointerEvents: 'none',
      }}
      data-testid='drag-strategy-indicator'
    >
      <MoveIndicatorItem dragType={indicatorFlagsInfo?.indicatorFlags.dragType ?? 'none'} />
      <ReparentIndicatorItem status={indicatorFlagsInfo?.indicatorFlags.reparent ?? 'none'} />
      <AncestorIndicatorItem enabled={indicatorFlagsInfo?.indicatorFlags.ancestor ?? false} />
    </FlexRow>
  )
})

interface MoveIndicatorItemProps {
  dragType: 'absolute' | 'static' | 'none'
}

const MoveIndicatorItem = React.memo<MoveIndicatorItemProps>((props) => {
  const colorTheme = useColorTheme()
  return (
    <FlexColumn style={{ alignItems: 'center', flex: 1.5 }}>
      <FlexRow style={{ gap: 4 }}>
        <div
          style={{
            padding: 4,
          }}
        >
          <VisibilityWrapper visible={props.dragType === 'static'}>
            <ModalityIcons.MoveAbsolute color={'on-highlight-secondary'} />
          </VisibilityWrapper>
          <VisibilityWrapper visible={props.dragType === 'absolute'}>
            <ModalityIcons.MoveAbsolute color={'primary'} />
          </VisibilityWrapper>
          <VisibilityWrapper visible={props.dragType === 'none'}>
            <ModalityIcons.MoveAbsolute color={'subdued'} />
          </VisibilityWrapper>
        </div>
        <div
          style={{
            padding: 4,
          }}
        >
          <VisibilityWrapper visible={props.dragType === 'absolute'}>
            <ModalityIcons.Reorder color={'on-highlight-secondary'} />
          </VisibilityWrapper>
          <VisibilityWrapper visible={props.dragType === 'static'}>
            <ModalityIcons.Reorder color={'primary'} />
          </VisibilityWrapper>
          <VisibilityWrapper visible={props.dragType === 'none'}>
            <ModalityIcons.Reorder color={'subdued'} />
          </VisibilityWrapper>
        </div>
      </FlexRow>
      <div
        style={{
          color: props.dragType === 'none' ? colorTheme.fg8.value : colorTheme.fg2.value,
        }}
      >
        {props.dragType === 'static' ? 'Reorder' : 'Move'}
      </div>
    </FlexColumn>
  )
})

interface IndicatorItemProps {
  enabled: boolean
}
const AncestorIndicatorItem = React.memo<IndicatorItemProps>((props) => {
  const colorTheme = useColorTheme()
  return (
    <FlexColumn style={{ alignItems: 'center' }}>
      <div
        style={{
          padding: 4,
        }}
      >
        <VisibilityWrapper visible={props.enabled}>
          <ModalityIcons.Magic color={'primary'} />
        </VisibilityWrapper>
        <VisibilityWrapper visible={!props.enabled}>
          <ModalityIcons.Magic color={'subdued'} />
        </VisibilityWrapper>
      </div>
      <div
        style={{
          color: props.enabled ? colorTheme.fg2.value : colorTheme.fg8.value,
        }}
      >
        Ancestor
      </div>
    </FlexColumn>
  )
})

interface ReparentIndicatorItemProps {
  status: 'same-component' | 'different-component' | 'none'
}
const ReparentIndicatorItem = React.memo<ReparentIndicatorItemProps>(({ status }) => {
  const colorTheme = useColorTheme()
  return (
    <FlexColumn style={{ alignItems: 'center' }}>
      <div style={{ padding: 4 }}>
        <VisibilityWrapper visible={status !== 'none'}>
          <ModalityIcons.Reparent color={'primary'} />
        </VisibilityWrapper>
        <VisibilityWrapper visible={status === 'none'}>
          <ModalityIcons.Reparent color={'subdued'} />
        </VisibilityWrapper>
      </div>
      <div
        style={{
          color: status === 'none' ? colorTheme.fg8.value : colorTheme.fg2.value,
        }}
      >
        Reparent
      </div>
    </FlexColumn>
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
