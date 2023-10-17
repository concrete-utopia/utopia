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
  setAutoHeightCommands,
  setAutoWidthCommands,
  sizeToVisualDimensions,
} from '../../../inspector-common'
import type { ElementInstanceMetadataMap } from '../../../../../core/shared/element-template'
import type { ElementPath } from '../../../../../core/shared/project-file-types'
import { getControlStyles } from '../../../common/control-styles'

export const TextAutoSizingTestId = 'textAutoSizing'

type TextSizingState = 'auto-width' | 'auto-height' | 'fixed-size'

const isConsideredFixed = (type: FixedHugFill['type'] | null | undefined): boolean =>
  type === 'fixed' || type === 'detected'

function detectTextSizingState(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): TextSizingState | null {
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null) {
    return null
  }
  if (!MetadataUtils.isSpan(instance)) {
    return null
  }

  const horizontal = detectFillHugFixedState('horizontal', metadata, elementPath)
  const vertical = detectFillHugFixedState('vertical', metadata, elementPath)

  if (horizontal.fixedHugFill?.type === 'hug' && isConsideredFixed(vertical.fixedHugFill?.type)) {
    return 'auto-width'
  }

  if (vertical.fixedHugFill?.type === 'hug' && isConsideredFixed(horizontal.fixedHugFill?.type)) {
    return 'auto-height'
  }

  if (
    isConsideredFixed(horizontal.fixedHugFill?.type) &&
    isConsideredFixed(vertical.fixedHugFill?.type)
  ) {
    return 'fixed-size'
  }

  return null
}

function detectTextSizingStateMultiSelect(
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
) {
  if (elementPaths.length === 0) {
    return null
  }

  const result = detectTextSizingState(metadata, elementPaths[0])
  for (const path of elementPaths.slice(1)) {
    const state = detectTextSizingState(metadata, path)
    if (state !== result) {
      return null
    }
  }
  return result
}

const controlStyles = getControlStyles('simple')

export const TextAutoSizingControl = React.memo(() => {
  const state = useEditorState(
    Substores.metadata,
    (store) =>
      detectTextSizingStateMultiSelect(store.editor.jsxMetadata, store.editor.selectedViews),
    'TextSizingControl state',
  )

  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)
  const pathTreesRef = useRefEditorState((store) => store.editor.elementPathTree)

  const dispatch = useDispatch()

  const setAutoWidth = React.useCallback(() => {
    const commands = selectedViewsRef.current.flatMap((elementPath) => {
      const parentFlexDirection =
        MetadataUtils.findElementByElementPath(metadataRef.current, elementPath)
          ?.specialSizeMeasurements.parentFlexDirection ?? null
      return setAutoWidthCommands(metadataRef.current, elementPath, parentFlexDirection)
    })
    dispatch([applyCommandsAction(commands)])
  }, [dispatch, metadataRef, selectedViewsRef])

  const setAutoHeight = React.useCallback(() => {
    const commands = selectedViewsRef.current.flatMap((elementPath) => {
      const parentFlexDirection =
        MetadataUtils.findElementByElementPath(metadataRef.current, elementPath)
          ?.specialSizeMeasurements.parentFlexDirection ?? null
      return setAutoHeightCommands(metadataRef.current, elementPath, parentFlexDirection)
    })
    dispatch([applyCommandsAction(commands)])
  }, [dispatch, metadataRef, selectedViewsRef])

  const setFixedSize = React.useCallback(() => {
    const commands = selectedViewsRef.current.flatMap((elementPath) => {
      return sizeToVisualDimensions(metadataRef.current, pathTreesRef.current, elementPath)
    })
    dispatch([applyCommandsAction(commands)])
  }, [dispatch, metadataRef, pathTreesRef, selectedViewsRef])

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
        value={state}
        options={
          [
            {
              value: 'auto-width',
              tooltip: 'Auto Width',
              icon: {
                category: 'typography',
                type: 'auto-width',
                color: 'secondary',
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
                color: 'secondary',
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
                color: 'secondary',
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
