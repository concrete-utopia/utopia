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
      <MoveReorderReparentIndicator
        dragType={indicatorFlagsInfo?.indicatorFlags.dragType ?? 'none'}
        reparentStatus={indicatorFlagsInfo?.indicatorFlags.reparent ?? 'none'}
      />
      <AncestorIndicatorItem enabled={indicatorFlagsInfo?.indicatorFlags.ancestor ?? false} />
    </FlexRow>
  )
})

interface MoveIndicatorItemProps {
  dragType: 'absolute' | 'static' | 'none'
  reparentStatus: 'same-component' | 'different-component' | 'none'
}

const MoveReorderReparentIndicator = React.memo<MoveIndicatorItemProps>((props) => {
  const colorTheme = useColorTheme()
  return (
    <FlexRow style={{ height: 32 }}>
      <FlexRow style={{}}>
        <div
          style={{
            padding: 7,
          }}
        >
          {(() => {
            if (props.reparentStatus !== 'none') {
              return <ModalityIcons.Reparent color={'primary'} />
            }
            if (props.dragType === 'absolute') {
              return <ModalityIcons.MoveAbsolute color={'primary'} />
            }
            if (props.dragType === 'static') {
              return <ModalityIcons.Reorder color={'primary'} />
            }
            return <ModalityIcons.MoveAbsolute color={'subdued'} />
          })()}
        </div>
      </FlexRow>
      <div
        style={{
          color: props.dragType === 'none' ? colorTheme.fg8.value : colorTheme.primary.value,
          minWidth: 110,
        }}
      >
        {(() => {
          if (props.reparentStatus !== 'none') {
            if (props.dragType === 'absolute') {
              return 'Absolute Reparent'
            } else {
              return 'Reparent'
            }
          }
          if (props.dragType === 'absolute') {
            return 'Absolute Move'
          }
          if (props.dragType === 'static') {
            return 'Reorder'
          }
          return 'Interaction'
        })()}
      </div>
    </FlexRow>
  )
})

interface IndicatorItemProps {
  enabled: boolean
}
const AncestorIndicatorItem = React.memo<IndicatorItemProps>((props) => {
  const colorTheme = useColorTheme()
  return (
    <FlexRow style={{ alignItems: 'center' }}>
      <div
        style={{
          padding: 7,
        }}
      >
        <VisibilityWrapper visible={props.enabled}>
          <ModalityIcons.Magic color={'main'} />
        </VisibilityWrapper>
        <VisibilityWrapper visible={!props.enabled}>
          <ModalityIcons.Magic color={'subdued'} />
        </VisibilityWrapper>
      </div>
      <div
        style={{
          color: props.enabled ? colorTheme.fg2.value : colorTheme.fg8.value,
          width: 145,
        }}
      >
        {props.enabled ? 'Affects Ancestor' : 'Does not affect Ancestor'}
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
