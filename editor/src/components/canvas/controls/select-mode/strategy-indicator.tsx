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
    return null
  }

  return (
    <FlexRow
      style={{
        ...UtopiaStyles.popup,
        pointerEvents: 'none',
        padding: '10px',
        alignItems: 'center',
        gap: 8,
        backgroundColor: colorTheme.bg0.value,
        borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
        boxShadow: `3px 4px 10px 0px ${UtopiaTheme.panelStyles.panelShadowColor}`,
        opacity:
          indicatorFlagsInfo.dragStarted && indicatorFlagsInfo.indicatorFlags.showIndicator ? 1 : 0,
      }}
      data-testid='drag-strategy-indicator'
    >
      <MoveIndicatorItem dragType={indicatorFlagsInfo.indicatorFlags.dragType} />
      <ReparentIndicatorItem status={indicatorFlagsInfo.indicatorFlags.reparent} />
      <AncestorIndicatorItem enabled={indicatorFlagsInfo.indicatorFlags.ancestor} />
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
            backgroundColor:
              props.dragType === 'absolute' ? colorTheme.primary.value : colorTheme.bg0.value,
            padding: 4,
            borderRadius: 10,
          }}
        >
          <VisibilityWrapper visible={props.dragType === 'absolute'}>
            <ModalityIcons.MoveAbsolute color={'on-highlight-main'} />
          </VisibilityWrapper>
          <VisibilityWrapper visible={props.dragType === 'static'}>
            <ModalityIcons.MoveAbsolute color={'main'} />
          </VisibilityWrapper>
          <VisibilityWrapper visible={props.dragType === 'none'}>
            <ModalityIcons.MoveAbsolute color={'subdued'} />
          </VisibilityWrapper>
        </div>
        <div
          style={{
            backgroundColor:
              props.dragType === 'static' ? colorTheme.primary.value : colorTheme.bg0.value,
            padding: 4,
            borderRadius: 10,
          }}
        >
          <VisibilityWrapper visible={props.dragType === 'static'}>
            <ModalityIcons.Reorder color={'on-highlight-main'} />
          </VisibilityWrapper>
          <VisibilityWrapper visible={props.dragType === 'absolute'}>
            <ModalityIcons.Reorder color={'main'} />
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
          borderRadius: 10,
          backgroundColor: props.enabled ? colorTheme.primary.value : 'transparent',
        }}
      >
        <VisibilityWrapper visible={props.enabled}>
          <ModalityIcons.Magic color={'on-highlight-main'} />
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
  const iconBackgroundColorFromStatus: string = React.useMemo(() => {
    switch (status) {
      case 'same-component':
        return colorTheme.primary.value
      case 'different-component':
        return colorTheme.component.value
      case 'none':
      default:
        return 'transparent'
    }
  }, [status, colorTheme])
  return (
    <FlexColumn style={{ alignItems: 'center' }}>
      <div style={{ padding: 4, borderRadius: 10, backgroundColor: iconBackgroundColorFromStatus }}>
        <VisibilityWrapper visible={status !== 'none'}>
          <ModalityIcons.Reparent color={'on-highlight-main'} />
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
