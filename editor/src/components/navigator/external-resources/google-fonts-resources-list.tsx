import * as React from 'react'
import { FlexRow, SectionBodyArea, SectionTitleRow, Title } from 'uuiui'
import { setFocus } from '../../../components/common/actions'
import {
  ExternalResources,
  useExternalResources,
} from '../../../printer-parsers/html/external-resources-parser'
import { isDescriptionParseError } from '../../../utils/value-parser-utils'
import { betterReactMemo } from '../../../uuiui-deps'
import { clearSelection, togglePanel } from '../../editor/actions/actions'
import { useEditorState } from '../../editor/store/store-hook'
import { GoogleFontsResourcesListItem } from './google-fonts-resources-list-item'
import { GoogleFontsResourcesListSearch } from './google-fonts-resources-list-search'

export const GoogleFontsResourcesList = betterReactMemo('GoogleFontsResourcesList', () => {
  const { values, useSubmitValueFactory } = useExternalResources()
  const { dispatch, minimised, focusedPanel } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      minimised: store.editor.googleFontsResources.minimised,
      focusedPanel: store.editor.focusedPanel,
    }
  })

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
          <GoogleFontsResourcesListSearch useSubmitValueFactory={useSubmitValueFactory} />
          {isDescriptionParseError(values) ? (
            <div>{values.description}</div>
          ) : (
            (values as ExternalResources).googleFontsResources.map((value, i) => (
              <GoogleFontsResourcesListItem key={value.fontFamily} value={value} />
            ))
          )}
        </SectionBodyArea>
      )}
    </div>
  )
})
