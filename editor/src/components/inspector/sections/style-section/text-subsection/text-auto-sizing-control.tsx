import React from 'react'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { strictEvery } from '../../../../../core/shared/array-utils'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { applyCommandsAction } from '../../../../editor/actions/action-creators'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../../editor/store/store-hook'
import { getControlStyles } from '../../../common/control-status'
import { cssNumber } from '../../../common/css-utils'
import { OptionChainControl, OptionChainOption } from '../../../controls/option-chain-control'
import {
  metadataSelector,
  selectedViewsSelector,
  useComputedSizeRef,
} from '../../../inpector-selectors'
import { detectFillHugFixedState } from '../../../inspector-common'
import {
  setPropFixedStrategies,
  setPropHugStrategies,
} from '../../../inspector-strategies/inspector-strategies'
import { commandsForFirstApplicableStrategy } from '../../../inspector-strategies/inspector-strategy'

export const TextAutoSizingTestId = 'textAutoSizing'

export const TextAutoSizingControl = React.memo(() => {
  const dispatch = useDispatch()
  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const isEditableText = useEditorState(
    Substores.metadata,
    (store) => {
      return strictEvery(store.editor.selectedViews, (path) =>
        MetadataUtils.targetTextEditableAndHasText(store.editor.jsxMetadata, path),
      )
    },
    'TextAutoSizingControl isEditableText',
  )

  const fixedHugFillState = useEditorState(
    Substores.metadata,
    (store) => {
      const target = store.editor.selectedViews[0]
      const width = detectFillHugFixedState('horizontal', store.editor.jsxMetadata, target)
      const height = detectFillHugFixedState('vertical', store.editor.jsxMetadata, target)

      if (width?.type === height?.type) {
        return width?.type
      }
      return null
    },
    'TextAutoSizingControl fixedHugFillState',
  )

  const controlStatus = React.useMemo(() => {
    if (!isEditableText) {
      return 'disabled'
    } else {
      return fixedHugFillState != null && fixedHugFillState !== 'fill' ? 'simple' : 'unset'
    }
  }, [fixedHugFillState, isEditableText])
  const controlStyles = React.useMemo(() => getControlStyles(controlStatus), [controlStatus])

  const widthComputedValue = useComputedSizeRef('width')
  const heightComputedValue = useComputedSizeRef('height')

  const onSubmitValue = React.useCallback(
    (newValue: any) => {
      if (newValue === 'fixed') {
        const widthCommands =
          commandsForFirstApplicableStrategy(
            metadataRef.current,
            selectedViewsRef.current,
            setPropFixedStrategies(
              'always',
              'horizontal',
              cssNumber(widthComputedValue.current ?? 0, null),
            ),
          ) ?? []
        const heightCommands =
          commandsForFirstApplicableStrategy(
            metadataRef.current,
            selectedViewsRef.current,
            setPropFixedStrategies(
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
            setPropHugStrategies('horizontal'),
          ) ?? []
        const heightCommands =
          commandsForFirstApplicableStrategy(
            metadataRef.current,
            selectedViewsRef.current,
            setPropHugStrategies('vertical'),
          ) ?? []
        dispatch([applyCommandsAction([...widthCommands, ...heightCommands])])
      }
    },
    [dispatch, metadataRef, selectedViewsRef, widthComputedValue, heightComputedValue],
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
        controlStatus={controlStatus}
        controlStyles={controlStyles}
        onSubmitValue={onSubmitValue}
        value={fixedHugFillState}
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
