import React from 'react'
import { setFocus } from '../../../components/common/actions'
import { isRight } from '../../../core/shared/either'
import { useExternalResources } from '../../../printer-parsers/html/external-resources-parser'
import { SectionTitleRow, FlexRow, Title, SectionBodyArea } from '../../../uuiui'
import { clearSelection, togglePanel } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { GoogleFontsResourcesListSearch } from './google-fonts-resources-list-search'

export const GoogleFontsResourcesList = React.memo(() => {
  const { values, useSubmitValueFactory } = useExternalResources()
  const dispatch = useDispatch()
  const { minimised, focusedPanel } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        minimised: store.editor.googleFontsResources.minimised,
        focusedPanel: store.editor.focusedPanel,
      }
    },
    'GoogleFontsResourcesList',
  )

  const toggleMinimised = React.useCallback(() => {
    dispatch([togglePanel('googleFontsResources')], 'leftpane')
  }, [dispatch])

  const onFocus = React.useCallback(
    (e: React.FocusEvent<HTMLElement>) => {
      dispatch([clearSelection()], 'everyone')
      if (focusedPanel !== 'googleFontsResources') {
        dispatch([setFocus('googleFontsResources')], 'leftpane')
      }
    },
    [focusedPanel, dispatch],
  )

  return (
    <div onFocus={onFocus} tabIndex={-1}>
      <SectionTitleRow minimised={minimised} toggleMinimised={toggleMinimised}>
        <FlexRow flexGrow={1}>
          <Title>Google Fonts</Title>
        </FlexRow>
      </SectionTitleRow>
      {minimised ? null : (
        <SectionBodyArea
          minimised={false}
          style={{
            paddingLeft: 12,
            paddingRight: 8,
          }}
        >
          <GoogleFontsResourcesListSearch
            linkedResources={isRight(values) ? values.value.googleFontsResources : []}
            useSubmitValueFactory={useSubmitValueFactory}
          />
        </SectionBodyArea>
      )}
    </div>
  )
})
