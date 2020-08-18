/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { ListChildComponentProps } from 'react-window'
import { isRight } from '../../../../../core/shared/either'
import { useExternalResources } from '../../../../../printer-parsers/html/external-resources-parser'
import { FlexColumn, UtopiaTheme } from '../../../../../uuiui'
import { notice } from '../../../../common/notices'
import { pushToast } from '../../../../editor/actions/actions'
import { useEditorState } from '../../../../editor/store/store-hook'
import { updatePushNewFontFamilyVariant } from '../../../../navigator/external-resources/google-fonts-resources-list-search'
import {
  cssFontStyleToWebFontStyle,
  cssFontWeightToWebFontWeight,
  GoogleFontsTypeface,
  prettyNameForFontVariant,
  SystemDefaultTypeface,
  webFontFamilyVariant,
  webFontVariant,
} from '../../../../navigator/external-resources/google-fonts-utils'
import { CSSFontStyle, CSSFontWeight } from '../../../common/css-utils'
import { OnSubmitValue } from '../../../controls/control'
import { ItemData } from './font-family-select-popup'

interface FontsListChildComponentProps extends ListChildComponentProps {
  data: {
    fontWeight: CSSFontWeight
    fontStyle: CSSFontStyle
    onSubmitFontFamily: OnSubmitValue<SystemDefaultTypeface | GoogleFontsTypeface>
    itemsArray: Array<ItemData>
    closePopup: () => void
  }
}

export const FontFamilySelectPopupItem: React.FunctionComponent<FontsListChildComponentProps> = ({
  data: { itemsArray, onSubmitFontFamily, fontWeight, fontStyle, closePopup },
  style,
  index,
}) => {
  const { dispatch } = useEditorState((state) => ({ dispatch: state.dispatch }))
  const metadata = itemsArray[index].metadata
  const { useSubmitValueFactory } = useExternalResources()
  const [pushNewFontFamilyVariant] = useSubmitValueFactory(updatePushNewFontFamilyVariant)

  const onClick = React.useCallback(() => {
    onSubmitFontFamily(metadata)
    closePopup()
    if (metadata.type === 'google-fonts-typeface') {
      const webFontWeight = cssFontWeightToWebFontWeight(fontWeight)
      const webFontStyle = cssFontStyleToWebFontStyle(fontStyle)
      if (isRight(webFontWeight) && isRight(webFontStyle)) {
        const newWebFontFamilyVariant = webFontFamilyVariant(
          metadata.name,
          webFontVariant(webFontWeight.value, webFontStyle.value),
        )
        pushNewFontFamilyVariant(newWebFontFamilyVariant)

        dispatch([
          pushToast(
            notice(
              `${metadata.name} - ${prettyNameForFontVariant(
                newWebFontFamilyVariant.fontVariant,
              )} has been added to the project`,
            ),
          ),
        ])
      }
    }
  }, [
    onSubmitFontFamily,
    fontStyle,
    fontWeight,
    metadata,
    pushNewFontFamilyVariant,
    closePopup,
    dispatch,
  ])
  return (
    <FlexColumn
      style={{ ...style, paddingLeft: 12, paddingRight: 12, paddingTop: 6, paddingBottom: 6 }}
      className='font-family-popup-item'
      css={{
        ':hover': {
          backgroundColor: UtopiaTheme.color.inspectorFocusedColor.value,
          color: 'white',
        },
      }}
      onClick={onClick}
    >
      {metadata.type === 'system-default-typeface' ? (
        <React.Fragment>
          <div style={{ fontSize: 12 }}>System Default</div>
          <div
            css={{
              fontSize: 11,
              color: '#888',
              whiteSpace: 'normal',
              '.font-family-popup-item:hover &': {
                backgroundColor: UtopiaTheme.color.inspectorFocusedColor.value,
                color: 'white',
              },
            }}
          >
            Use the default typeface of the operating system: SF Pro on macOS and iOS, Roboto on
            Android and Segoe UI on Windows
          </div>
        </React.Fragment>
      ) : (
        <div style={{ fontSize: 12 }}>{metadata.name}</div>
      )}
    </FlexColumn>
  )
}
