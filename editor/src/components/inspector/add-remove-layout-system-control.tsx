/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import {
  Icons,
  FlexRow,
  SquareButton,
  FunctionIcons,
  InspectorSectionHeader,
  useColorTheme,
  Icn,
} from '../../uuiui'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import {
  addFlexLayoutStrategies,
  addGridLayoutStrategies,
  removeFlexLayoutStrategies,
  removeGridLayoutStrategies,
} from './inspector-strategies/inspector-strategies'
import {
  detectAreElementsFlexContainers,
  detectAreElementsGridContainers,
} from './inspector-common'
import { executeFirstApplicableStrategy } from './inspector-strategies/inspector-strategy'
import { useDispatch } from '../editor/store/dispatch-context'
import { NO_OP } from '../../core/shared/utils'
import type { DropdownMenuItem } from '../../uuiui/radix-components'
import { DropdownMenu } from '../../uuiui/radix-components'

export const AddRemoveLayoutSystemControlTestId = (): string => 'AddRemoveLayoutSystemControlTestId'
export const AddFlexLayoutOptionId = 'add-flex-layout-option'
export const AddGridLayoutOptionId = 'add-grid-layout-option'

interface AddRemoveLayoutSystemControlProps {}

export const AddRemoveLayoutSystemControl = React.memo<AddRemoveLayoutSystemControlProps>(() => {
  const isLayoutedContainer = useEditorState(
    Substores.metadata,
    (store) =>
      detectAreElementsFlexContainers(metadataSelector(store), selectedViewsSelector(store)) ||
      detectAreElementsGridContainers(metadataSelector(store), selectedViewsSelector(store)),
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
      <SquareButton
        data-testid={AddRemoveLayoutSystemControlTestId()}
        highlight
        onClick={NO_OP}
        style={{ width: 12, height: 22 }}
      >
        <Icn category='semantic' type='plus' width={12} height={12} />
      </SquareButton>
    ),
    [],
  )

  const addLayoutSystemMenuDropdownItems = React.useMemo(
    (): DropdownMenuItem[] => [
      { id: AddFlexLayoutOptionId, label: 'Flex', onSelect: addFlexLayoutSystem },
      { id: AddGridLayoutOptionId, label: 'Grid', onSelect: addGridLayoutSystem },
    ],
    [addFlexLayoutSystem, addGridLayoutSystem],
  )

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
        <span style={{ textTransform: 'capitalize', fontSize: '11px' }}>Layout System</span>
      </FlexRow>
      {isLayoutedContainer ? (
        <SquareButton
          data-testid={AddRemoveLayoutSystemControlTestId()}
          highlight
          onClick={removeLayoutSystem}
        >
          <Icn category='semantic' type='cross' width={12} height={12} />
        </SquareButton>
      ) : (
        <div data-testid={AddRemoveLayoutSystemControlTestId()}>
          <DropdownMenu
            align='end'
            items={addLayoutSystemMenuDropdownItems}
            opener={addLayoutSystemOpenerButton}
          />
        </div>
      )}
    </InspectorSectionHeader>
  )
})
