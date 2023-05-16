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

export type WrapperType = 'fragment' | 'frame' | 'group'

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

export function groupSectionOption(wrapperType: WrapperType): SelectOption {
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
const GroupOption = groupSectionOption('group')
const DivOption = groupSectionOption('frame')

const Options: Array<SelectOption> = [FragmentOption, GroupOption, DivOption]

function wrapperTypeFromContentAffectingType(type: ContentAffectingType | null): WrapperType {
  if (type === 'fragment') {
    return 'fragment'
  }
  if (type === 'sizeless-div') {
    return 'group'
  }
  return 'frame'
}

export const GroupDropdown = React.memo(() => {
  const dispatch = useDispatch()

  const metadataRef = useRefEditorState(metadataSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const selectedFrames = useEditorState(
    Substores.metadata,
    (store) =>
      selectedViewsSelector(store).filter((elementPath) => {
        const contentAffectingType = getElementContentAffectingType(
          store.editor.jsxMetadata,
          store.editor.allElementProps,
          elementPath,
        )

        const isFrameHeuristic = isAbsolutePositionedFrame(
          store.editor.jsxMetadata,
          store.editor.allElementProps,
          elementPath,
        )

        const isFrameOrGroup =
          isFrameHeuristic ||
          contentAffectingType === 'fragment' ||
          contentAffectingType === 'sizeless-div'

        return (
          isFrameOrGroup &&
          !MetadataUtils.isParentFlexLayoutedContainerForElement(
            MetadataUtils.findElementByElementPath(store.editor.jsxMetadata, elementPath),
          )
        )
      }),
    'GroupSection selectedViews',
  )

  const isDropDownEnabled = selectedFrames.length === 1

  const selectedElementGrouplikeType = useEditorState(
    Substores.metadata,
    selectedElementGrouplikeTypeSelector,
    'GroupSection allSelectedElementGrouplike',
  )

  const onChange = React.useCallback(
    ({ value }: SelectOption) => {
      if (!isDropDownEnabled) {
        return
      }

      const currentType: WrapperType = wrapperTypeFromContentAffectingType(
        selectedElementGrouplikeType,
      )
      const desiredType = value as WrapperType
      const commands = selectedViewsRef.current.flatMap((elementPath): CanvasCommand[] => {
        if (currentType === 'fragment') {
          if (desiredType === 'fragment') {
            // NOOP
            return []
          }

          if (desiredType === 'group') {
            return convertFragmentToGroup(
              metadataRef.current,
              elementPathTreeRef.current,
              elementPath,
            )
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

        if (currentType === 'group') {
          if (desiredType === 'group') {
            // NOOP
            return []
          }

          if (desiredType === 'frame') {
            return (
              convertGroupToFrameCommands(
                metadataRef.current,
                allElementPropsRef.current,
                elementPath,
              ) ?? []
            )
          }

          if (desiredType === 'fragment') {
            return convertGroupToFragment(
              metadataRef.current,
              elementPathTreeRef.current,
              elementPath,
            )
          }

          assertNever(desiredType)
        }

        if (currentType === 'frame') {
          if (desiredType === 'frame') {
            // NOOP
            return []
          }

          if (desiredType === 'group') {
            return convertFrameToGroupCommands(
              metadataRef.current,
              allElementPropsRef.current,
              elementPath,
            )
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

        assertNever(currentType)
      })

      if (commands.length > 0) {
        dispatch([applyCommandsAction(commands)])
      }
    },
    [
      allElementPropsRef,
      dispatch,
      isDropDownEnabled,
      metadataRef,
      elementPathTreeRef,
      selectedElementGrouplikeType,
      selectedViewsRef,
    ],
  )

  const currentValue = React.useMemo(() => {
    if (selectedElementGrouplikeType === 'fragment') {
      return FragmentOption
    }
    if (selectedElementGrouplikeType === 'sizeless-div') {
      return GroupOption
    }
    return DivOption
  }, [selectedElementGrouplikeType])

  const controlStyles = isDropDownEnabled ? simpleControlStyles : disabledControlStyles
  return (
    <PopupList
      value={currentValue}
      options={Options}
      onSubmitValue={onChange}
      controlStyles={controlStyles}
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
