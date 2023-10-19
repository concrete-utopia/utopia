import React from 'react'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { strictEvery } from '../../../../../core/shared/array-utils'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { applyCommandsAction } from '../../../../editor/actions/action-creators'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../../editor/store/store-hook'
import type { ControlStatus } from '../../../common/control-status'
import { getControlStyles } from '../../../common/control-styles'
import { cssNumber } from '../../../common/css-utils'
import type { OptionChainOption } from '../../../controls/option-chain-control'
import { OptionChainControl } from '../../../controls/option-chain-control'
import {
  metadataSelector,
  selectedViewsSelector,
  useNonRoundedComputedSizeRef,
} from '../../../inpector-selectors'
import type { FixedHugFill } from '../../../inspector-common'
import { detectFillHugFixedStateMultiselect, isFixedHugFillEqual } from '../../../inspector-common'
import {
  setPropFixedSizeStrategies,
  setPropHugStrategies,
} from '../../../inspector-strategies/inspector-strategies'
import { commandsForFirstApplicableStrategy } from '../../../inspector-strategies/inspector-strategy'
import { fixedSizeDimensionHandlingText } from '../../../../text-editor/text-handling'

export const TextAutoSizingTestId = 'textAutoSizing'

function useAutoSizingTypeAndStatus(): {
  status: ControlStatus
  type: 'fixed' | 'hug' | 'hug-group' | 'computed' | 'detected' | 'scaled' | null
} {
  const isEditableText = useEditorState(
    Substores.metadata,
    (store) => {
      return strictEvery(store.editor.selectedViews, (path) =>
        MetadataUtils.targetTextEditableAndHasText(
          store.editor.jsxMetadata,
          store.editor.elementPathTree,
          path,
        ),
      )
    },
    'TextAutoSizingControl isEditableText',
  )

  const widthFillHugFixedState = useEditorState(
    Substores.metadata,
    (store) => {
      return detectFillHugFixedStateMultiselect(
        'horizontal',
        store.editor.jsxMetadata,
        store.editor.selectedViews,
      )
    },
    'TextAutoSizingControl fixedHugFillState width',
    isFixedHugFillEqual,
  )

  const heightFillHugFixedState = useEditorState(
    Substores.metadata,
    (store) => {
      return detectFillHugFixedStateMultiselect(
        'vertical',
        store.editor.jsxMetadata,
        store.editor.selectedViews,
      )
    },
    'TextAutoSizingControl fixedHugFillState height',
    isFixedHugFillEqual,
  )

  let fixedHugState: FixedHugFill | null = null
  if (
    widthFillHugFixedState.fixedHugFill != null &&
    widthFillHugFixedState.fixedHugFill.type !== 'fill' &&
    heightFillHugFixedState.fixedHugFill != null &&
    heightFillHugFixedState.fixedHugFill.type !== 'fill'
  ) {
    if (widthFillHugFixedState.fixedHugFill.type === heightFillHugFixedState.fixedHugFill.type) {
      fixedHugState = widthFillHugFixedState.fixedHugFill
    }
  }

  const controlStatus = React.useMemo(() => {
    if (!isEditableText) {
      return 'disabled'
    } else {
      return widthFillHugFixedState.controlStatus
    }
  }, [widthFillHugFixedState, isEditableText])

  const type =
    controlStatus === 'disabled' || controlStatus === 'unset' ? null : fixedHugState?.type ?? null

  return { status: controlStatus, type: type }
}

export const TextAutoSizingControl = React.memo(() => {
  const dispatch = useDispatch()
  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const controlStatusAndValueType = useAutoSizingTypeAndStatus()
  const controlStyles = React.useMemo(
    () => getControlStyles(controlStatusAndValueType.status),
    [controlStatusAndValueType],
  )

  const widthComputedValue = useNonRoundedComputedSizeRef('width')
  const heightComputedValue = useNonRoundedComputedSizeRef('height')

  const onSubmitValue = React.useCallback(
    (newValue: any) => {
      if (selectedViewsRef.current.length === 0) {
        return
      }
      if (newValue === 'fixed') {
        const width = fixedSizeDimensionHandlingText(
          metadataRef.current,
          elementPathTreeRef.current,
          selectedViewsRef.current[0],
          widthComputedValue.current ?? 0,
        )
        const widthCommands =
          commandsForFirstApplicableStrategy(
            metadataRef.current,
            selectedViewsRef.current,
            elementPathTreeRef.current,
            allElementPropsRef.current,
            setPropFixedSizeStrategies('always', 'horizontal', cssNumber(width, null)),
          ) ?? []
        const heightCommands =
          commandsForFirstApplicableStrategy(
            metadataRef.current,
            selectedViewsRef.current,
            elementPathTreeRef.current,
            allElementPropsRef.current,
            setPropFixedSizeStrategies(
              'always',
              'vertical',
              cssNumber(heightComputedValue.current ?? 0, null),
            ),
          ) ?? []
        dispatch([applyCommandsAction([...widthCommands, ...heightCommands])])
      } else {
        const widthCommands =
          commandsForFirstApplicableStrategy(
            metadataRef.current,
            selectedViewsRef.current,
            elementPathTreeRef.current,
            allElementPropsRef.current,
            setPropHugStrategies('horizontal'),
          ) ?? []
        const heightCommands =
          commandsForFirstApplicableStrategy(
            metadataRef.current,
            selectedViewsRef.current,
            elementPathTreeRef.current,
            allElementPropsRef.current,
            setPropHugStrategies('vertical'),
          ) ?? []
        dispatch([applyCommandsAction([...widthCommands, ...heightCommands])])
      }
    },
    [
      metadataRef,
      selectedViewsRef,
      elementPathTreeRef,
      allElementPropsRef,
      widthComputedValue,
      heightComputedValue,
      dispatch,
    ],
  )

  return (
    <InspectorContextMenuWrapper
      id='textSizing-context-menu'
      items={[]}
      data={null}
      style={{ gridColumn: '1 / span 2' }}
    >
      <OptionChainControl
        id='textAutoSizing'
        key='textAutoSizing'
        testId={TextAutoSizingTestId}
        controlStatus={controlStatusAndValueType.status}
        controlStyles={controlStyles}
        onSubmitValue={onSubmitValue}
        value={controlStatusAndValueType.type}
        options={
          [
            {
              value: 'hug',
              tooltip: 'Auto',
              icon: {
                category: 'typography',
                type: 'auto',
                color: 'secondary',
                width: 16,
                height: 16,
              },
            },
            {
              value: 'fixed',
              tooltip: 'Fixed',
              icon: {
                category: 'typography',
                type: 'fixed',
                color: 'secondary',
                width: 16,
                height: 16,
              },
            },
          ] as Array<OptionChainOption<string | number>>
        }
      />
    </InspectorContextMenuWrapper>
  )
})
