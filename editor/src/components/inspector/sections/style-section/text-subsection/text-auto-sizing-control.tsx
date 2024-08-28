import React from 'react'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { applyCommandsAction } from '../../../../editor/actions/action-creators'
import { useDispatch } from '../../../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../../../editor/store/store-hook'
import type { OptionChainOption } from '../../../controls/option-chain-control'
import { OptionChainControl } from '../../../controls/option-chain-control'
import type { FixedHugFill } from '../../../inspector-common'
import {
  detectFillHugFixedState,
  isHuggingFixedHugFill,
  setAutoHeightCommands,
  setAutoWidthCommands,
  setFixedSizeCommands,
} from '../../../inspector-common'
import type { ElementInstanceMetadataMap } from '../../../../../core/shared/element-template'
import type { ElementPath } from '../../../../../core/shared/project-file-types'
import { getControlStyles } from '../../../common/control-styles'
import { useNonRoundedComputedSizeRef } from '../../../inpector-selectors'
import type { ElementPathTrees } from '../../../../../core/shared/element-path-tree'

export const TextAutoSizingTestId = 'textAutoSizing'

type TextSizingState = 'auto-width' | 'auto-height' | 'fixed-size' | 'mixed' | 'disabled'

const isConsideredFixed = (type: FixedHugFill['type'] | null | undefined): boolean =>
  type === 'fixed' || type === 'detected'

function detectTextSizingState(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): TextSizingState {
  if (!MetadataUtils.targetTextEditableAndHasText(metadata, pathTrees, elementPath)) {
    return 'disabled'
  }

  const horizontal = detectFillHugFixedState('horizontal', metadata, elementPath)
  const vertical = detectFillHugFixedState('vertical', metadata, elementPath)

  if (
    isHuggingFixedHugFill(horizontal.fixedHugFill?.type) &&
    isHuggingFixedHugFill(vertical.fixedHugFill?.type)
  ) {
    return 'auto-width'
  }

  if (
    isHuggingFixedHugFill(horizontal.fixedHugFill?.type) &&
    isConsideredFixed(vertical.fixedHugFill?.type)
  ) {
    return 'auto-width'
  }

  if (
    isHuggingFixedHugFill(vertical.fixedHugFill?.type) &&
    isConsideredFixed(horizontal.fixedHugFill?.type)
  ) {
    return 'auto-height'
  }

  if (
    isConsideredFixed(horizontal.fixedHugFill?.type) &&
    isConsideredFixed(vertical.fixedHugFill?.type)
  ) {
    return 'fixed-size'
  }

  return 'disabled'
}

export function detectTextSizingStateMultiSelect(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPaths: ElementPath[],
): TextSizingState {
  if (elementPaths.length === 0) {
    return 'disabled'
  }

  const result = detectTextSizingState(metadata, pathTrees, elementPaths[0])
  for (const path of elementPaths.slice(1)) {
    const state = detectTextSizingState(metadata, pathTrees, path)
    if (state !== result) {
      return 'mixed'
    }
  }
  return result
}

export const TextAutoSizingControl = React.memo(() => {
  const state = useEditorState(
    Substores.metadata,
    (store) =>
      detectTextSizingStateMultiSelect(
        store.editor.jsxMetadata,
        store.editor.elementPathTree,
        store.editor.selectedViews,
      ),
    'TextSizingControl state',
  )

  const controlStyles = React.useMemo(() => {
    if (state === 'disabled') {
      return getControlStyles('unset')
    }
    if (state === 'mixed') {
      return getControlStyles('multiselect-mixed-simple-or-unset')
    }
    return getControlStyles('simple')
  }, [state])

  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const pathTreesRef = useRefEditorState((store) => store.editor.elementPathTree)

  const dispatch = useDispatch()

  const nonRoundedComputedWidthRef = useNonRoundedComputedSizeRef('width')
  const nonRoundedComputedHeightRef = useNonRoundedComputedSizeRef('height')

  const setAutoWidth = React.useCallback(() => {
    const commands = selectedViewsRef.current.flatMap((elementPath) => {
      const parentFlexDirection =
        MetadataUtils.findElementByElementPath(metadataRef.current, elementPath)
          ?.specialSizeMeasurements.parentFlexDirection ?? null
      return setAutoWidthCommands(
        elementPath,
        parentFlexDirection,
        nonRoundedComputedHeightRef.current ?? 0,
      )
    })
    dispatch([applyCommandsAction(commands)])
  }, [dispatch, metadataRef, nonRoundedComputedHeightRef, selectedViewsRef])

  const setAutoHeight = React.useCallback(() => {
    const commands = selectedViewsRef.current.flatMap((elementPath) => {
      const parentFlexDirection =
        MetadataUtils.findElementByElementPath(metadataRef.current, elementPath)
          ?.specialSizeMeasurements.parentFlexDirection ?? null
      return setAutoHeightCommands(
        elementPath,
        parentFlexDirection,
        nonRoundedComputedWidthRef.current ?? 0,
      )
    })
    dispatch([applyCommandsAction(commands)])
  }, [dispatch, metadataRef, nonRoundedComputedWidthRef, selectedViewsRef])

  const setFixedSize = React.useCallback(() => {
    const commands = selectedViewsRef.current.flatMap((elementPath) => {
      return setFixedSizeCommands(metadataRef.current, pathTreesRef.current, elementPath, {
        width: nonRoundedComputedWidthRef.current ?? 0,
        height: nonRoundedComputedHeightRef.current ?? 0,
      })
    })
    dispatch([applyCommandsAction(commands)])
  }, [
    dispatch,
    metadataRef,
    nonRoundedComputedHeightRef,
    nonRoundedComputedWidthRef,
    pathTreesRef,
    selectedViewsRef,
  ])

  const onSubmitValue = React.useCallback(
    (option: any) => {
      switch (option) {
        case 'auto-width':
          setAutoWidth()
          break
        case 'auto-height':
          setAutoHeight()
          break
        case 'fixed-size':
          setFixedSize()
          break
        default:
          return
      }
    },
    [setAutoHeight, setAutoWidth, setFixedSize],
  )

  const value = state === 'disabled' || state === 'mixed' ? null : state

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
        controlStatus={'simple'}
        controlStyles={controlStyles}
        onSubmitValue={onSubmitValue}
        value={value}
        options={
          [
            {
              value: 'auto-width',
              tooltip: 'Auto Width',
              icon: {
                category: 'typography',
                type: 'auto-width',
                width: 18,
                height: 18,
              },
            },
            {
              value: 'auto-height',
              tooltip: 'Auto Height',
              icon: {
                category: 'typography',
                type: 'auto-height',
                width: 18,
                height: 18,
              },
            },
            {
              value: 'fixed-size',
              tooltip: 'Fixed',
              icon: {
                category: 'typography',
                type: 'fixed-size',
                width: 18,
                height: 18,
              },
            },
          ] as Array<OptionChainOption<string | number>>
        }
      />
    </InspectorContextMenuWrapper>
  )
})
