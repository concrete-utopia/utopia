import React from 'react'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../../../core/shared/atom-with-pub-sub'
import { FlexColumn, FlexRow, ModalityIcons, useColorTheme, UtopiaStyles } from '../../../../uuiui'
import { NavigatorWidthAtom } from '../../../editor/store/editor-state'
import { useEditorState } from '../../../editor/store/store-hook'

const StrategyIndicatorWidth = 240
export const StrategyIndicator = React.memo(() => {
  const colorTheme = useColorTheme()
  const indicatorFlagsInfo = useEditorState((store) => {
    if (store.editor.canvas.interactionSession?.interactionData.type === 'DRAG') {
      return {
        indicatorFlags: store.editor.canvas.controls.dragToMoveIndicatorFlags,
        dragStarted: store.editor.canvas.interactionSession?.interactionData.drag != null,
      }
    } else {
      return null
    }
  }, 'StrategyIndicator')

  const isNavigatorOpen = useEditorState(
    (store) => !store.editor.navigator.minimised,
    'StrategyIndicator navigator status',
  )
  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom, AlwaysTrue)

  if (indicatorFlagsInfo == null) {
    return null
  }

  return (
    <FlexRow
      style={{
        pointerEvents: 'none',
        position: 'absolute',
        top: 4,
        left: `calc(50% - ${StrategyIndicatorWidth / 2}px + ${
          isNavigatorOpen ? navigatorWidth / 2 : 0
        }px)`,
        width: StrategyIndicatorWidth,
        height: 57,
        borderRadius: 24,
        padding: '0 16px',
        alignItems: 'flex-end',
        gap: 8,
        backgroundColor: colorTheme.bg0.value,
        boxShadow: UtopiaStyles.shadowStyles.medium.boxShadow,
        opacity:
          indicatorFlagsInfo.dragStarted && indicatorFlagsInfo.indicatorFlags.showIndicator ? 1 : 0,
      }}
      data-testid='drag-strategy-indicator'
    >
      <MoveIndicatorItem dragType={indicatorFlagsInfo.indicatorFlags.dragType} />
      <Divider />
      <ReparentIndicatorItem status={indicatorFlagsInfo.indicatorFlags.reparent} />
      <Divider />
      <AncestorIndicatorItem enabled={indicatorFlagsInfo.indicatorFlags.ancestor} />
    </FlexRow>
  )
})

interface MoveIndicatorItemProps {
  dragType: 'absolute' | 'static'
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
          <ModalityIcons.MoveAbsolute
            color={props.dragType === 'absolute' ? 'on-highlight-main' : 'main'}
          />
        </div>
        <div
          style={{
            backgroundColor:
              props.dragType === 'static' ? colorTheme.primary.value : colorTheme.bg0.value,
            padding: 4,
            borderRadius: 10,
          }}
        >
          <ModalityIcons.Reorder
            color={props.dragType === 'static' ? 'on-highlight-main' : 'main'}
          />
        </div>
      </FlexRow>
      <div style={{}}>{props.dragType === 'absolute' ? 'Move' : 'Reorder'}</div>
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
        <ModalityIcons.Magic color={props.enabled ? 'on-highlight-main' : 'subdued'} />
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
        <ModalityIcons.Reparent color={status !== 'none' ? 'on-highlight-main' : 'subdued'} />
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

const Divider = React.memo(() => {
  const colorTheme = useColorTheme()
  return <div style={{ height: '100%', width: 1, backgroundColor: colorTheme.fg8.value }} />
})
