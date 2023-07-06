import React from 'react'
import { FlexRow, Icons } from '../../../uuiui'
import type { FontVariantData } from './google-fonts-utils'
import { prettyNameForFontVariant } from './google-fonts-utils'
import type {
  PushNewFontFamilyVariant,
  RemoveFontFamilyVariant,
} from './google-fonts-resources-list-search'

interface FontVariantItemProps {
  style: React.CSSProperties
  data: FontVariantData & {
    pushNewFontFamilyVariant: PushNewFontFamilyVariant
    removeFontFamilyVariant: RemoveFontFamilyVariant
  }
}

export const FontVariantItem: React.FunctionComponent<
  React.PropsWithChildren<FontVariantItemProps>
> = ({ style, data }) => {
  const [hovered, setHovered] = React.useState(false)
  const onMouseEnter = React.useCallback(() => setHovered(true), [])
  const onMouseLeave = React.useCallback(() => setHovered(false), [])

  const { pushNewFontFamilyVariant, removeFontFamilyVariant, variant } = data

  const onDownloadedClick = React.useCallback(
    () => removeFontFamilyVariant(variant),
    [removeFontFamilyVariant, variant],
  )
  const onDownloadClick = React.useCallback(
    () => pushNewFontFamilyVariant(variant),
    [pushNewFontFamilyVariant, variant],
  )
  return (
    <FlexRow style={style} onMouseEnter={onMouseEnter} onMouseLeave={onMouseLeave}>
      <div style={{ paddingLeft: 24 + 12, flexGrow: 1 }}>
        {prettyNameForFontVariant(data.variant.fontVariant)}
      </div>
      {data.isDownloaded ? (
        hovered ? (
          <Icons.CrossInTranslucentCircle
            onClick={onDownloadedClick}
            style={{ cursor: 'pointer' }}
          />
        ) : (
          <Icons.Downloaded onClick={onDownloadedClick} style={{ cursor: 'pointer' }} />
        )
      ) : (
        <Icons.Download onClick={onDownloadClick} style={{ cursor: 'pointer' }} />
      )}
    </FlexRow>
  )
}
