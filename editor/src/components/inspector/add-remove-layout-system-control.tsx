/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import { Icons, FlexRow, SquareButton, InspectorSectionHeader, useColorTheme } from '../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  addFlexLayoutStrategies,
  addGridLayoutStrategies,
  removeFlexLayoutStrategies,
  removeGridLayoutStrategies,
} from './inspector-strategies/inspector-strategies'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import { useDispatch } from '../editor/store/dispatch-context'
import { NO_OP, assertNever } from '../../core/shared/utils'
import type { DropdownMenuItem } from '../../uuiui/radix-components'
import { DropdownMenu, regularDropdownMenuItem } from '../../uuiui/radix-components'
import { stripNulls } from '../../core/shared/array-utils'
import { layoutSystemSelector } from './flex-section'
import { optionalMap } from '../../core/shared/optional-utils'
import { useGridPropertiesSet } from './common/inspector-atoms'
import { when } from '../../utils/react-conditionals'

export const AddRemoveLayoutSystemControlTestId = (): string => 'AddRemoveLayoutSystemControlTestId'
export const AddFlexLayoutOptionId = 'add-flex-layout'
export const AddGridLayoutOptionId = 'add-grid-layout'
export const RemoveLayoutSystemOptionId = 'remove-layout-system'

interface AddRemoveLayoutSystemControlProps {}

export const AddRemoveLayoutSystemControl = React.memo<AddRemoveLayoutSystemControlProps>(() => {
  const layoutSystem = useEditorState(
    Substores.metadata,
    layoutSystemSelector,
    'AddRemoveLayoutSystemControl isFlexLayoutedContainer',
  )

  const dispatch = useDispatch()
  const elementMetadataRef = useRefEditorState(metadataSelector)
  const selectedViewsRef = useRefEditorState(selectedViewsSelector)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)

  const addFlexLayoutSystem = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        addFlexLayoutStrategies(
          elementMetadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
        ),
      ),
    [allElementPropsRef, dispatch, elementMetadataRef, elementPathTreeRef, selectedViewsRef],
  )

  const addGridLayoutSystem = React.useCallback(
    () =>
      executeFirstApplicableStrategy(
        dispatch,
        addGridLayoutStrategies(
          elementMetadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
          allElementPropsRef.current,
        ),
      ),
    [allElementPropsRef, dispatch, elementMetadataRef, elementPathTreeRef, selectedViewsRef],
  )

  const removeLayoutSystem = React.useCallback(
    () =>
      executeFirstApplicableStrategy(dispatch, [
        ...removeFlexLayoutStrategies(
          elementMetadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
        ),
        ...removeGridLayoutStrategies(
          elementMetadataRef.current,
          selectedViewsRef.current,
          elementPathTreeRef.current,
        ),
      ]),
    [dispatch, elementMetadataRef, elementPathTreeRef, selectedViewsRef],
  )

  const colorTheme = useColorTheme()

  const addLayoutSystemOpenerButton = React.useCallback(
    () => (
      <SquareButton highlight onClick={NO_OP}>
        {layoutSystem == null ? <Icons.SmallPlus /> : <Icons.Threedots />}
      </SquareButton>
    ),
    [layoutSystem],
  )

  const addLayoutSystemMenuDropdownItems = React.useMemo(
    (): DropdownMenuItem[] =>
      stripNulls([
        regularDropdownMenuItem({
          id: AddFlexLayoutOptionId,
          label: 'Convert to Flex',
          onSelect: addFlexLayoutSystem,
        }),
        regularDropdownMenuItem({
          id: AddGridLayoutOptionId,
          label: 'Convert to Grid',
          onSelect: addGridLayoutSystem,
        }),
        optionalMap(
          () =>
            regularDropdownMenuItem({
              id: RemoveLayoutSystemOptionId,
              label: 'Remove layout system',
              onSelect: removeLayoutSystem,
              danger: true,
            }),
          layoutSystem,
        ),
      ]),
    [addFlexLayoutSystem, addGridLayoutSystem, layoutSystem, removeLayoutSystem],
  )

  const label = () => {
    switch (layoutSystem) {
      case null:
        return 'Layout System'
      case 'flex':
        return 'Flex'
      case 'grid':
        return 'Grid'
      default:
        assertNever(layoutSystem)
    }
  }

  const numberOfGridPropsSet = useGridPropertiesSet().length

  return (
    <InspectorSectionHeader
      css={{
        transition: 'color .1s ease-in-out',
        color: colorTheme.fg1.value,
        '--buttonContentOpacity': 0.3,
        '&:hover': {
          color: colorTheme.fg1.value,
          '--buttonContentOpacity': 1,
        },
        alignItems: 'center',
      }}
    >
      <FlexRow
        style={{
          flexGrow: 1,
          alignSelf: 'stretch',
          gap: 8,
        }}
      >
        <span style={{ textTransform: 'capitalize', fontSize: '11px' }}>{label()}</span>
      </FlexRow>
      <FlexRow>
        {when(
          numberOfGridPropsSet > 0,
          <span
            style={{
              padding: 2,
              backgroundColor: colorTheme.emphasizedBackground.value,
              color: colorTheme.dynamicBlue.value,
            }}
          >
            {`+${numberOfGridPropsSet}`}
          </span>,
        )}
        <div data-testid={AddRemoveLayoutSystemControlTestId()}>
          <DropdownMenu
            align='end'
            items={addLayoutSystemMenuDropdownItems}
            opener={addLayoutSystemOpenerButton}
          />
        </div>
      </FlexRow>
    </InspectorSectionHeader>
  )
})
