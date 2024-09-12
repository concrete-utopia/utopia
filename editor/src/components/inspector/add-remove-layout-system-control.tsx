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
import {
  DropdownMenu,
  regularDropdownMenuItem,
  separatorDropdownMenuItem,
} from '../../uuiui/radix-components'
import { stripNulls } from '../../core/shared/array-utils'
import { layoutSystemSelector } from './flex-section'
import { AdvancedGridModal } from './controls/advanced-grid-modal'
import { when } from '../../utils/react-conditionals'

export const AddRemoveLayoutSystemControlTestId = (): string => 'AddRemoveLayoutSystemControlTestId'
export const AddFlexLayoutOptionId = 'add-flex-layout'
export const AddGridLayoutOptionId = 'add-grid-layout'
export const RemoveLayoutSystemOptionId = 'remove-layout-system'

interface AddRemoveLayoutSystemControlProps {}

export const AddRemoveLayoutSystemControl = React.memo<AddRemoveLayoutSystemControlProps>(() => {
  const [popupOpen, setPopupOpen] = React.useState(false)
  const closePopup = React.useCallback(() => {
    setPopupOpen(false)
  }, [])
  const openPopup = React.useCallback(() => {
    setPopupOpen(true)
  }, [])

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

  const addLayoutSystemOpenerButton = React.useCallback(() => {
    return (
      <SquareButton highlight onClick={NO_OP} primary={popupOpen}>
        {layoutSystem == null ? (
          <Icons.SmallPlus />
        ) : (
          <Icons.Threedots color={popupOpen ? 'white' : 'main'} />
        )}
      </SquareButton>
    )
  }, [layoutSystem, popupOpen])

  const addLayoutSystemMenuDropdownItems = React.useMemo((): DropdownMenuItem[] => {
    const gridItems: DropdownMenuItem[] = [
      regularDropdownMenuItem({
        id: 'more-settings',
        label: 'Advanced',
        onSelect: openPopup,
      }),
      regularDropdownMenuItem({
        id: AddFlexLayoutOptionId,
        label: 'Convert to Flex',
        onSelect: addFlexLayoutSystem,
      }),
      separatorDropdownMenuItem('dropdown-separator'),
      regularDropdownMenuItem({
        id: RemoveLayoutSystemOptionId,
        label: 'Remove Grid',
        onSelect: removeLayoutSystem,
        danger: true,
      }),
    ]

    const flexItems: DropdownMenuItem[] = [
      regularDropdownMenuItem({
        id: AddGridLayoutOptionId,
        label: 'Convert to Grid',
        onSelect: addGridLayoutSystem,
      }),
      separatorDropdownMenuItem('dropdown-separator'),
      regularDropdownMenuItem({
        id: RemoveLayoutSystemOptionId,
        label: 'Remove Flex',
        onSelect: removeLayoutSystem,
        danger: true,
      }),
    ]

    const noLayoutItems: DropdownMenuItem[] = [
      regularDropdownMenuItem({
        id: AddFlexLayoutOptionId,
        label: 'Flex',
        onSelect: addFlexLayoutSystem,
      }),
      regularDropdownMenuItem({
        id: AddGridLayoutOptionId,
        label: 'Grid',
        onSelect: addGridLayoutSystem,
      }),
    ]

    return stripNulls(
      layoutSystem === 'grid' ? gridItems : layoutSystem === 'flex' ? flexItems : noLayoutItems,
    )
  }, [addFlexLayoutSystem, addGridLayoutSystem, layoutSystem, removeLayoutSystem, openPopup])

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
        {when(
          popupOpen,
          <AdvancedGridModal
            id='grid-advanced1'
            testId='grid-advanced1'
            key='grid-advanced1'
            closePopup={closePopup}
            popupOpen={popupOpen}
          />,
        )}
      </FlexRow>
      <div data-testid={AddRemoveLayoutSystemControlTestId()}>
        <DropdownMenu
          align='end'
          items={addLayoutSystemMenuDropdownItems}
          opener={addLayoutSystemOpenerButton}
        />
      </div>
    </InspectorSectionHeader>
  )
})
