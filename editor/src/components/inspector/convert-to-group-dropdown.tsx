/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { createSelector } from 'reselect'
import { optionalMap } from '../../core/shared/optional-utils'
import { assertNever } from '../../core/shared/utils'
import { useColorTheme, FlexColumn, InspectorSectionHeader, PopupList, FlexRow } from '../../uuiui'
import { getControlStyles } from '../../uuiui-deps'
import { getElementContentAffectingType } from '../canvas/canvas-strategies/strategies/group-like-helpers'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { useRefEditorState, useEditorState, Substores } from '../editor/store/store-hook'
import { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import { SelectOption } from './controls/select-control'
import { WrapperType } from './convert-to-group-dropdown-helpers'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'

const simpleControlStyles = getControlStyles('simple')

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

export const GroupSection = React.memo(() => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()

  const metadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)

  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const selectedViews = useEditorState(
    Substores.selectedViews,
    selectedViewsSelector,
    'GroupSection selectedViews',
  )

  const selectedElementGrouplikeType = useEditorState(
    Substores.metadata,
    selectedElementGrouplikeTypeSelector,
    'GroupSection allSelectedElementGrouplike',
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

  const onChange = React.useCallback(
    ({ value }: SelectOption) => {
      const mode = value as WrapperType
      const commands = selectedViewsRef.current.flatMap((elementPath) => {
        switch (mode) {
          case 'fragment':
            return wrapInFragment(metadataRef.current, allElementPropsRef.current, elementPath)
          case 'sizeless-div':
            return wrapInSizelessDiv(metadataRef.current, allElementPropsRef.current, elementPath)
          case 'div':
            return (
              optionalMap(
                (props) =>
                  wrapInDiv(metadataRef.current, allElementPropsRef.current, elementPath, props),
                getWrapperPositioningProps(metadataRef.current, elementPath),
              ) ?? []
            )
          default:
            assertNever(mode)
        }
      })
      dispatch([applyCommandsAction(commands)])
    },
    [allElementPropsRef, dispatch, metadataRef, selectedViewsRef],
  )

  if (selectedViews.length !== 1) {
    return null
  }

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
        <PopupList
          value={currentValue}
          options={Options}
          onSubmitValue={onChange}
          controlStyles={simpleControlStyles}
        />
      </FlexRow>
    </FlexColumn>
  )
})
