import React from 'react'
import { assertNever } from '../../core/shared/utils'
import { ControlStatus, getControlStyles } from '../../uuiui-deps'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { OptionChainControl, OptionChainOption } from './controls/option-chain-control'
import { detectPackedSpacedSetting, PackedSpaced } from './inspector-common'
import {
  setSpacingModePackedStrategies,
  setSpacingModeSpaceBetweenStrategies,
} from './inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import { UIGridRow } from './widgets/ui-grid-row'

const OverflowControlOptions: Array<OptionChainOption<PackedSpaced>> = [
  {
    tooltip: 'Packed',
    label: 'Packed',
    value: 'packed',
  },
  {
    tooltip: 'Spaced',
    label: 'Spaced',
    value: 'spaced',
  },
]

export const SpacedPackedControl = React.memo(() => {
  const metadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'SpacedPackedControl metadata',
  )
  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'SpacedPackedControl selectedViews',
  )
  const dispatch = useDispatch()

  const setting = detectPackedSpacedSetting(metadata, selectedViews)

  const onUpdate = React.useCallback(
    (value: PackedSpaced) => {
      switch (value) {
        case 'packed':
          return executeFirstApplicableStrategy(
            dispatch,
            metadata,
            selectedViews,
            setSpacingModePackedStrategies,
          )
        case 'spaced':
          return executeFirstApplicableStrategy(
            dispatch,
            metadata,
            selectedViews,
            setSpacingModeSpaceBetweenStrategies,
          )
        default:
          assertNever(value)
      }
    },
    [dispatch, metadata, selectedViews],
  )

  const controlStatus: ControlStatus = 'simple'
  return (
    <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
      Spacing
      <OptionChainControl
        id={'spaced-packed-control'}
        key={'spaced-packed-control'}
        testId={'spaced-packed-control'}
        onSubmitValue={onUpdate}
        value={setting}
        options={OverflowControlOptions}
        controlStatus={controlStatus}
        controlStyles={getControlStyles(controlStatus)}
      />
    </UIGridRow>
  )
})
