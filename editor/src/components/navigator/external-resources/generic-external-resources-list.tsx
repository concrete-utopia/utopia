import * as React from 'react'
import {
  FlexRow,
  FunctionIcons,
  SectionBodyArea,
  SectionTitleRow,
  SquareButton,
  Title,
} from 'uuiui'
import { setFocus } from '../../../components/common/actions'
import { isRight } from '../../../core/shared/either'
import {
  ExternalResources,
  genericExternalResource,
  useExternalResources,
} from '../../../printer-parsers/html/external-resources-parser'
import { betterReactMemo } from '../../../uuiui-deps'
import { clearSelection, togglePanel } from '../../editor/actions/actions'
import { useEditorState } from '../../editor/store/store-hook'
import { GridRowProps } from '../../inspector/widgets/grid-row'
import { GenericExternalResourcesInput } from './generic-external-resources-input'
import { GenericExternalResourcesListItem } from './generic-external-resources-list-item'

export const ResourcesListGridRowConfig: GridRowProps = {
  padded: false,
  type: '<-------auto------->|---60px---|',
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

export const GenericExternalResourcesList = betterReactMemo('GenericExternalResourcesList', () => {
  const { values, useSubmitValueFactory } = useExternalResources()

  const [editingIndexOrInserting, setEditingIndexOrInserting] = React.useState<
    null | number | 'insert-new'
  >(null)

  const closeInsertAndEditingFields = React.useCallback(() => {
    setEditingIndexOrInserting(null)
  }, [])

  const { dispatch, minimised, focusedPanel } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      minimised: store.editor.genericExternalResources.minimised,
      focusedPanel: store.editor.focusedPanel,
    }
  }, 'GenericExternalResourcesList')

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
          <SquareButton highlight onClick={toggleOpenAddInsertField}>
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
                  hrefValueToEdit={value.href}
                  relValueToEdit={value.rel}
                  closeField={closeInsertAndEditingFields}
                  onSubmitValues={indexedUpdateGenericResource}
                />
              ) : (
                <GenericExternalResourcesListItem
                  key={value.href}
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
