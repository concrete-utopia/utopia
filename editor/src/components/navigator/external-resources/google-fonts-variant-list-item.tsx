import React from 'react'
import type { NodeComponentProps } from 'react-vtree/dist/es/Tree'
import { FontFamilyItem } from './font-family-item'
import { FontVariantItem } from './font-variant-item'
import { GoogleFontsResourcesListItemHeight } from './google-fonts-resources-list-search'
import type { FontNode } from './google-fonts-utils'

interface GoogleFontsListItemProps extends NodeComponentProps<FontNode> {}

export const GoogleFontsListItem: React.FunctionComponent<
  React.PropsWithChildren<GoogleFontsListItemProps>
> = (props) => {
  if (props.data.type === 'font-family') {
    return (
      <FontFamilyItem
        style={{ ...props.style, height: GoogleFontsResourcesListItemHeight }}
        data={props.data}
        toggle={props.toggle}
        isOpen={props.isOpen}
      />
    )
  } else if (props.data.type === 'font-variant') {
    return (
      <FontVariantItem
        style={{ ...props.style, height: GoogleFontsResourcesListItemHeight }}
        data={props.data}
      />
    )
  } else {
    return null
  }
}
