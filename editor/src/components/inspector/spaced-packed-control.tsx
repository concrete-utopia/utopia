import React from 'react'
import { createSelector } from 'reselect'
import { assertNever } from '../../core/shared/utils'
import { ControlStatus, getControlStyles } from '../../uuiui-deps'
import { useHighlighPaddingHandlers } from '../canvas/controls/select-mode/select-mode-hooks'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { OptionChainControl, OptionChainOption } from './controls/option-chain-control'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import { detectPackedSpacedSetting, PackedSpaced } from './inspector-common'
import {
  setSpacingModePackedStrategies,
  setSpacingModeSpaceBetweenStrategies,
} from './inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import { UIGridRow } from './widgets/ui-grid-row'

export const PackedLabelCopy = 'Packed' as const
export const SpacedLabelCopy = 'Spaced' as const

const OverflowControlOptions: Array<OptionChainOption<PackedSpaced>> = [
  {
    tooltip: PackedLabelCopy,
    label: PackedLabelCopy,
    value: 'packed',
  },
  {
    tooltip: SpacedLabelCopy,
    label: SpacedLabelCopy,
    value: 'spaced',
  },
]

const packedSpacedSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  detectPackedSpacedSetting,
)

export const SpacedPackedControl = React.memo(() => {
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const dispatch = useDispatch()

  const spacedPackedSetting = useEditorState(Substores.metadata, packedSpacedSelector, '')

  const onUpdate = React.useCallback(
    (value: PackedSpaced) => {
      switch (value) {
        case 'packed':
          return executeFirstApplicableStrategy(
            dispatch,
            metadataRef.current,
            selectedViewsRef.current,
            setSpacingModePackedStrategies,
          )
        case 'spaced':
          return executeFirstApplicableStrategy(
            dispatch,
            metadataRef.current,
            selectedViewsRef.current,
            setSpacingModeSpaceBetweenStrategies,
          )
        default:
          assertNever(value)
      }
    },
    [dispatch, metadataRef, selectedViewsRef],
  )

  const { onMouseEnter, onMouseLeave } = useHighlighPaddingHandlers()

  const controlStatus: ControlStatus = 'simple'
  return (
    <UIGridRow
      onMouseEnter={onMouseEnter}
      onMouseLeave={onMouseLeave}
      padded={true}
      variant='<---1fr--->|------172px-------|'
    >
      Spacing
      <OptionChainControl
        id={'spaced-packed-control'}
        key={'spaced-packed-control'}
        testId={'spaced-packed-control'}
        onSubmitValue={onUpdate}
        value={spacedPackedSetting}
        options={OverflowControlOptions}
        controlStatus={controlStatus}
        controlStyles={getControlStyles(controlStatus)}
      />
    </UIGridRow>
  )
})
