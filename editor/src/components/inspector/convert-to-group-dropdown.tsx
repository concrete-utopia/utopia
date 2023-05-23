/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { createSelector } from 'reselect'
import { assertNever } from '../../core/shared/utils'
import { useColorTheme, FlexColumn, InspectorSectionHeader, PopupList, FlexRow } from '../../uuiui'
import { getControlStyles } from '../../uuiui-deps'
import {
  ContentAffectingType,
  getElementContentAffectingType,
} from '../canvas/canvas-strategies/strategies/group-like-helpers'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { useRefEditorState, useEditorState, Substores } from '../editor/store/store-hook'
import { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import { SelectOption } from './controls/select-control'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  convertFragmentToFrame,
  convertFragmentToGroup,
  convertFrameToFragmentCommands,
  convertFrameToGroupCommands,
  convertGroupToFragment,
  convertGroupToFrameCommands,
  isAbsolutePositionedFrame,
} from '../canvas/canvas-strategies/strategies/group-conversion-helpers'
import { CanvasCommand } from '../canvas/commands/commands'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import {
  EditorContract,
  getEditorContractForContentAffectingType,
} from '../canvas/canvas-strategies/strategies/contracts/contract-helpers'

const simpleControlStyles = getControlStyles('simple')
const disabledControlStyles = getControlStyles('disabled')

const selectedElementGrouplikeTypeSelector = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.allElementProps,
  selectedViewsSelector,
  (metadata, allElementProps, selectedViews) => {
    if (selectedViews.length !== 1) {
      return null
    }
    return getElementContentAffectingType(metadata, allElementProps, selectedViews[0])
  },
)

const selectedElementContractSelector = createSelector(
  metadataSelector,
  (store: MetadataSubstate) => store.editor.allElementProps,
  selectedViewsSelector,
  (metadata, allElementProps, selectedViews): EditorContract | null => {
    if (selectedViews.length !== 1) {
      return null // TODO make it work for mixed selection
    }
    return getEditorContractForContentAffectingType(
      getElementContentAffectingType(metadata, allElementProps, selectedViews[0]),
    )
  },
)

export function groupSectionOption(wrapperType: EditorContract): SelectOption {
  switch (wrapperType) {
    case 'frame':
      return { value: 'frame', label: 'Frame' }
    case 'fragment':
      return { value: 'fragment', label: 'Fragment' }
    default:
      assertNever(wrapperType)
  }
}

const FragmentOption = groupSectionOption('fragment')
const DivOption = groupSectionOption('frame')

const Options: Array<SelectOption> = [FragmentOption, DivOption]

export const GroupDropdown = React.memo(() => {
  const dispatch = useDispatch()

  const metadataRef = useRefEditorState(metadataSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const selectedElementGrouplikeType = useEditorState(
    Substores.metadata,
    selectedElementGrouplikeTypeSelector,
    'GroupSection allSelectedElementGrouplike',
  )

  const selectedElementContract = useEditorState(
    Substores.metadata,
    selectedElementContractSelector,
    'GroupSection selectedElementContract',
  )

  const onChange = React.useCallback(
    ({ value }: SelectOption) => {
      const currentType: EditorContract | null = selectedElementContract

      if (currentType == null) {
        // for now, in case of multiselect etc, do nothing
        return
      }

      const desiredType = value as EditorContract
      const commands = selectedViewsRef.current.flatMap((elementPath): CanvasCommand[] => {
        if (currentType === 'fragment') {
          if (desiredType === 'fragment') {
            // NOOP
            return []
          }

          if (desiredType === 'frame') {
            return (
              convertFragmentToFrame(
                metadataRef.current,
                elementPathTreeRef.current,
                allElementPropsRef.current,
                elementPath,
              ) ?? []
            )
          }
          assertNever(desiredType)
        }

        if (currentType === 'frame') {
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
          assertNever(desiredType)
        }

        // placeholder for currentType === 'group

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
      selectedViewsRef,
    ],
  )

  const currentValue = React.useMemo(() => {
    if (selectedElementGrouplikeType === 'fragment') {
      return FragmentOption
    }
    return DivOption
  }, [selectedElementGrouplikeType])

  return (
    <PopupList
      value={currentValue}
      options={Options}
      onSubmitValue={onChange}
      controlStyles={simpleControlStyles}
      containerMode={'noBorder'}
    />
  )
})

export const GroupSection = React.memo(() => {
  const colorTheme = useColorTheme()

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
        <GroupDropdown />
      </FlexRow>
    </FlexColumn>
  )
})
