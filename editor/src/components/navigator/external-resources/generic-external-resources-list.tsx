import React from 'react'
import { setFocus } from '../../../components/common/actions'
import { isRight } from '../../../core/shared/either'
import type { ExternalResources } from '../../../printer-parsers/html/external-resources-parser'
import {
  genericExternalResource,
  useExternalResources,
} from '../../../printer-parsers/html/external-resources-parser'
import {
  SectionTitleRow,
  FlexRow,
  Title,
  SquareButton,
  FunctionIcons,
  SectionBodyArea,
} from '../../../uuiui'
import { clearSelection, togglePanel } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import type { GridRowProps } from '../../inspector/widgets/ui-grid-row'
import { GenericExternalResourcesInput } from './generic-external-resources-input'
import { GenericExternalResourcesListItem } from './generic-external-resources-list-item'

export const ResourcesListGridRowConfig: GridRowProps = {
  padded: false,
  variant: '<-------auto------->|---60px---|',
}

function updatePushNewGenericResource(
  { hrefValue, relValue }: { hrefValue: string; relValue: string },
  oldValue: ExternalResources,
): ExternalResources {
  const working = { ...oldValue }
  working.genericExternalResources = [
    ...oldValue.genericExternalResources,
    genericExternalResource(hrefValue, relValue),
  ]
  return working
}

function getIndexedUpdateGenericResource(index: number) {
  return function (
    { hrefValue, relValue }: { hrefValue: string; relValue: string },
    oldValue: ExternalResources,
  ): ExternalResources {
    const working = { ...oldValue }
    working.genericExternalResources[index] = genericExternalResource(hrefValue, relValue)
    return working
  }
}

export const GenericExternalResourcesList = React.memo(() => {
  const { values, useSubmitValueFactory } = useExternalResources()

  const [editingIndexOrInserting, setEditingIndexOrInserting] = React.useState<
    null | number | 'insert-new'
  >(null)

  const closeInsertAndEditingFields = React.useCallback(() => {
    setEditingIndexOrInserting(null)
  }, [])

  const dispatch = useDispatch()

  const { minimised, focusedPanel } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        minimised: store.editor.genericExternalResources.minimised,
        focusedPanel: store.editor.focusedPanel,
      }
    },
    'GenericExternalResourcesList',
  )

  const toggleMinimised = React.useCallback(() => {
    dispatch([togglePanel('genericExternalResources')], 'leftpane')
  }, [dispatch])

  const onFocus = React.useCallback(
    (e: React.FocusEvent<HTMLElement>) => {
      dispatch([clearSelection()], 'everyone')
      if (focusedPanel !== 'dependencylist') {
        dispatch([setFocus('dependencylist')], 'everyone')
      }
    },
    [focusedPanel, dispatch],
  )

  const toggleOpenAddInsertField = React.useCallback((e: React.MouseEvent) => {
    e.stopPropagation()
    setEditingIndexOrInserting((value) => (value === 'insert-new' ? null : 'insert-new'))
  }, [])

  const [pushNewGenericResource] = useSubmitValueFactory(updatePushNewGenericResource)
  const [indexedUpdateGenericResource] = useSubmitValueFactory(
    getIndexedUpdateGenericResource(
      typeof editingIndexOrInserting === 'number' ? editingIndexOrInserting : 0,
    ),
  )

  return (
    <div onFocus={onFocus} tabIndex={-1}>
      <SectionTitleRow minimised={minimised} toggleMinimised={toggleMinimised}>
        <FlexRow flexGrow={1}>
          <Title>External Resources</Title>
        </FlexRow>
        {minimised ? null : (
          <SquareButton highlightOnHover onClick={toggleOpenAddInsertField}>
            <FunctionIcons.Add
              style={{
                flexGrow: 0,
                flexShrink: 0,
              }}
            />
          </SquareButton>
        )}
      </SectionTitleRow>
      {minimised ? null : (
        <SectionBodyArea minimised={false}>
          {isRight(values) ? (
            values.value.genericExternalResources.map((value, i) =>
              editingIndexOrInserting === i ? (
                <GenericExternalResourcesInput
                  key={'ListItem-' + i}
                  hrefValueToEdit={value.href}
                  relValueToEdit={value.rel}
                  closeField={closeInsertAndEditingFields}
                  onSubmitValues={indexedUpdateGenericResource}
                />
              ) : (
                <GenericExternalResourcesListItem
                  key={'ListItem-' + i}
                  value={value}
                  index={i}
                  setEditingIndex={setEditingIndexOrInserting}
                />
              ),
            )
          ) : (
            <div>{values.value.description}</div>
          )}
          {editingIndexOrInserting === 'insert-new' ? (
            <GenericExternalResourcesInput
              closeField={closeInsertAndEditingFields}
              onSubmitValues={pushNewGenericResource}
            />
          ) : null}
        </SectionBodyArea>
      )}
    </div>
  )
})
