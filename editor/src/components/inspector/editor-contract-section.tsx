/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { createSelector } from 'reselect'
import { assertNever } from '../../core/shared/utils'
import { FlexColumn, InspectorSectionHeader, PopupList, FlexRow, colorTheme } from '../../uuiui'
import type { ControlStyles } from '../../uuiui-deps'
import { getControlStyles } from '../../uuiui-deps'
import { getElementFragmentLikeType } from '../canvas/canvas-strategies/strategies/fragment-like-helpers'
import { addToast, applyCommandsAction } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { useRefEditorState, useEditorState, Substores } from '../editor/store/store-hook'
import type { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import type { SelectOption } from './controls/select-control'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  convertFragmentToFrame,
  convertFragmentToGroup,
  convertFrameToFragmentCommands,
  convertFrameToGroup,
  convertGroupToFrameCommands,
  getInstanceForFragmentToFrameConversion,
  getInstanceForFragmentToGroupConversion,
  getInstanceForFrameToFragmentConversion,
  getInstanceForFrameToGroupConversion,
  getInstanceForGroupToFrameConversion,
} from '../canvas/canvas-strategies/strategies/group-conversion-helpers'
import type { CanvasCommand } from '../canvas/commands/commands'
import type { EditorContract } from '../canvas/canvas-strategies/strategies/contracts/contract-helpers'
import { getEditorContractForElement } from '../canvas/canvas-strategies/strategies/contracts/contract-helpers'
import { allElemsEqual } from '../../core/shared/array-utils'
import * as EP from '../../core/shared/element-path'
import { notice } from '../common/notice'

const simpleControlStyles = getControlStyles('simple')
const disabledControlStyles: ControlStyles = {
  ...getControlStyles('simple'),
  mainColor: colorTheme.fg5.value,
}

const selectedElementGrouplikeTypeSelector = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  selectedViewsSelector,
  (metadata, allElementProps, pathTrees, selectedViews) => {
    if (selectedViews.length !== 1) {
      return null
    }
    return getElementFragmentLikeType(metadata, allElementProps, pathTrees, selectedViews[0])
  },
)

const selectedElementContractSelector = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  selectedViewsSelector,
  (metadata, allElementProps, pathTrees, selectedViews): EditorContract | null => {
    if (selectedViews.length !== 1) {
      return null // TODO make it work for mixed selection
    }
    return getEditorContractForElement(metadata, allElementProps, pathTrees, selectedViews[0])
  },
)

export const allSelectedElementsContractSelector = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  selectedViewsSelector,
  (
    metadata,
    allElementProps,
    pathTrees,
    selectedViews,
  ): EditorContract | 'mixed-multiselection' => {
    const contracts = selectedViews.map((selectedView) =>
      getEditorContractForElement(metadata, allElementProps, pathTrees, selectedView),
    )
    if (allElemsEqual(contracts)) {
      return contracts[0]
    } else {
      return 'mixed-multiselection'
    }
  },
)

export function groupSectionOption(wrapperType: 'frame' | 'fragment' | 'group'): SelectOption {
  switch (wrapperType) {
    case 'frame':
      return { value: 'frame', label: 'Frame' }
    case 'fragment':
      return { value: 'fragment', label: 'Fragment' }
    case 'group':
      return { value: 'group', label: 'Group' }
    default:
      assertNever(wrapperType)
  }
}

const FragmentOption = groupSectionOption('fragment')
const FrameOption = groupSectionOption('frame')
const GroupOption = groupSectionOption('group')

export const EditorContractDropdown = React.memo(() => {
  const dispatch = useDispatch()

  const metadataRef = useRefEditorState(metadataSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)

  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const selectedElementContract = useEditorState(
    Substores.metadata,
    selectedElementContractSelector,
    'EditorContractDropdown selectedElementContract',
  )

  const selectedViews = useEditorState(
    Substores.selectedViews,
    selectedViewsSelector,
    'EditorContractDropdown selectedViews',
  )

  const onChange = React.useCallback(
    ({ value }: SelectOption) => {
      const currentType: EditorContract | null = selectedElementContract

      if (currentType == null) {
        // for now, in case of multiselect etc, do nothing
        return
      }

      // If any of the selected views are root elements of components,
      // bounce the entire operation as changing those currently is a problem.
      if (selectedViews.some((selectedView) => EP.isRootElementOfInstance(selectedView))) {
        dispatch(
          [
            addToast(
              notice(
                `Cannot change root elements of components.`,
                'WARNING',
                false,
                'change-root-element-of-component',
              ),
            ),
          ],
          'everyone',
        )
        return
      }

      const desiredType = value as EditorContract

      if (desiredType === 'not-quite-frame') {
        throw new Error(
          'Invariant violation: not-quite-frame should never be a selectable option in the dropdown',
        )
      }

      const commands = selectedViews.flatMap((elementPath): CanvasCommand[] => {
        if (currentType === 'fragment') {
          if (desiredType === 'fragment') {
            // NOOP
            return []
          }

          if (desiredType === 'frame') {
            return convertFragmentToFrame(
              metadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              elementPath,
              'do-not-convert-if-it-has-static-children',
            )
          }

          if (desiredType === 'group') {
            return convertFragmentToGroup(
              metadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              elementPath,
            )
          }
          assertNever(desiredType)
        }

        if (currentType === 'frame' || currentType === 'not-quite-frame') {
          if (desiredType === 'frame') {
            // NOOP
            return []
          }

          if (desiredType === 'fragment') {
            return convertFrameToFragmentCommands(
              metadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              elementPath,
            )
          }

          if (desiredType === 'group') {
            return convertFrameToGroup(
              metadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              elementPath,
            )
          }
          assertNever(desiredType)
        }

        if (currentType === 'group') {
          if (desiredType === 'group') {
            // NOOP
            return []
          }

          if (desiredType === 'fragment') {
            return convertFrameToFragmentCommands(
              metadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              elementPath,
            )
          }

          if (desiredType === 'frame') {
            return convertGroupToFrameCommands(
              metadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              elementPath,
            )
          }
          assertNever(desiredType)
        }

        assertNever(currentType)
      })

      if (commands.length > 0) {
        dispatch([applyCommandsAction(commands)])
      }
    },
    [
      allElementPropsRef,
      dispatch,
      metadataRef,
      elementPathTreeRef,
      selectedElementContract,
      selectedViews,
    ],
  )

  const currentValue = React.useMemo(() => {
    if (selectedElementContract === 'fragment') {
      return FragmentOption
    } else if (selectedElementContract === 'group') {
      return GroupOption
    }
    return FrameOption
  }, [selectedElementContract])

  const options = React.useMemo((): Array<SelectOption> => {
    function maybeDisable(option: SelectOption, disabled: boolean) {
      return { ...option, disabled: disabled }
    }
    let disabledGroup = false
    let disabledFrame = false
    let disabledFragment = false

    if (selectedViews.length > 1) {
      disabledGroup = true
      disabledFrame = true
      disabledFragment = true
    } else if (selectedViews.length === 1) {
      const view = selectedViews[0]
      switch (currentValue.value) {
        case 'frame':
          disabledGroup =
            getInstanceForFrameToGroupConversion(
              metadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              view,
            ) == null
          disabledFragment =
            getInstanceForFrameToFragmentConversion(
              metadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              view,
            ) == null
          break
        case 'fragment':
          disabledFrame =
            getInstanceForFragmentToFrameConversion(
              metadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              view,
              'do-not-convert-if-it-has-static-children',
            ) == null
          disabledGroup =
            getInstanceForFragmentToGroupConversion(
              metadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              view,
            ) == null
          break
        case 'group':
          disabledFrame =
            getInstanceForGroupToFrameConversion(
              metadataRef.current,
              elementPathTreeRef.current,
              allElementPropsRef.current,
              view,
            ) == null
          break
      }
    }

    return [
      maybeDisable(FrameOption, disabledFrame),
      maybeDisable(GroupOption, disabledGroup),
      maybeDisable(FragmentOption, disabledFragment),
    ]
  }, [selectedViews, metadataRef, elementPathTreeRef, allElementPropsRef, currentValue])

  return (
    <PopupList
      id={'editor-contract-popup-list'}
      value={currentValue}
      options={options}
      onSubmitValue={onChange}
      controlStyles={
        selectedElementContract === 'not-quite-frame' ? disabledControlStyles : simpleControlStyles
      }
      containerMode={'noBorder'}
      style={{ position: 'relative', left: -8 }}
    />
  )
})
EditorContractDropdown.displayName = 'EditorContractDropdown'

export const GroupSection = React.memo(() => {
  return (
    <FlexColumn>
      <InspectorSectionHeader
        css={{
          transition: 'color .1s ease-in-out',
          color: colorTheme.fg1.value,
          '--buttonContentOpacity': 0.3,
          '&:hover': {
            color: colorTheme.fg1.value,
            '--buttonContentOpacity': 1,
          },
        }}
      >
        <FlexRow
          style={{
            flexGrow: 1,
            alignSelf: 'stretch',
            gap: 8,
          }}
        >
          <span style={{ textTransform: 'capitalize', fontSize: '11px' }}>Group</span>
        </FlexRow>
      </InspectorSectionHeader>
      <FlexRow style={{ padding: 4 }}>
        <EditorContractDropdown />
      </FlexRow>
    </FlexColumn>
  )
})
