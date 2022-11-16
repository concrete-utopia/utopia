import React from 'react'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../../../core/shared/atom-with-pub-sub'
import {
  FlexColumn,
  FlexRow,
  IcnColor,
  ModalityIcons,
  useColorTheme,
  UtopiaStyles,
} from '../../../../uuiui'
import {
  DragToMoveIndicatorFlags,
  EditorStorePatched,
  NavigatorWidthAtom,
} from '../../../editor/store/editor-state'
import { useEditorState } from '../../../editor/store/store-hook'
import { useDelayedEditorState } from '../../canvas-strategies/canvas-strategies'

const useDelayedDragToMoveIndicatorFlags = () => {
  const selector = (store: EditorStorePatched) =>
    !store.editor.canvas.controls.dragToMoveIndicatorFlags.showIndicator
      ? null
      : store.editor.canvas.controls.dragToMoveIndicatorFlags
  return useDelayedEditorState<DragToMoveIndicatorFlags | null>(selector)
}

const StrategyIndicatorWidth = 240
export const StrategyIndicator = React.memo(() => {
  const colorTheme = useColorTheme()
  const indicatorFlags = useDelayedDragToMoveIndicatorFlags()

  const isNavigatorOpen = useEditorState(
    (store) => !store.editor.navigator.minimised,
    'StrategyIndicator navigator status',
  )
  const navigatorWidth = usePubSubAtomReadOnly(NavigatorWidthAtom, AlwaysTrue)

  if (indicatorFlags == null || !indicatorFlags.showIndicator) {
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
      }}
      data-testid='drag-strategy-indicator'
    >
      <MoveIndicatorItem dragType={indicatorFlags.dragType} />
      <Divider />
      <ReparentIndicatorItem status={indicatorFlags.reparent} />
      <Divider />
      <AncestorIndicatorItem enabled={indicatorFlags.ancestor} />
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
      <div style={{ padding: 2 }}>
        <ModalityIcons.Magic color={props.enabled ? 'primary' : 'subdued'} />
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
  const iconColorFromStatus: IcnColor = React.useMemo(() => {
    switch (status) {
      case 'same-component':
        return 'primary'
      case 'different-component':
        return 'component'
      case 'none':
      default:
        return 'subdued'
    }
  }, [status])
  return (
    <FlexColumn style={{ alignItems: 'center' }}>
      <div style={{ padding: 2 }}>
        <ModalityIcons.Reparent color={iconColorFromStatus} />
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
