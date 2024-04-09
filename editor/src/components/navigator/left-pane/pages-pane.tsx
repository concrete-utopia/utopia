import React from 'react'
import { FlexColumn, FlexRow, UtopiaTheme, colorTheme } from '../../../uuiui'

export const PagesPane = React.memo((props) => {
  return (
    <FlexColumn style={{ height: '100%', overflowY: 'scroll' }}>
      {['1', '2', '3'].map((pageRoute: string) => {
        return <PageRouteEntry key={pageRoute} filepath={pageRoute} active={pageRoute === '1'} />
      })}
    </FlexColumn>
  )
})

interface PageRouteEntryProps {
  filepath: string
  active: boolean
}
const PageRouteEntry = React.memo<PageRouteEntryProps>((props) => {
  return (
    <FlexRow
      style={{
        color: colorTheme.neutralForeground.value,
        backgroundColor: props.active ? colorTheme.subtleBackground.value : 'transparent',
        marginLeft: 8,
        marginRight: 8,
        paddingLeft: 3,
        paddingTop: 3,
        paddingBottom: 3,
        height: UtopiaTheme.layout.rowHeight.smaller,
        alignItems: 'center',
        justifyContent: 'space-between',
        borderRadius: 2,
        position: 'relative',
      }}
    >
      {/* TODO if we want renaming, cannibalize it from FileBrowserItem */}
      <span
        style={{
          flex: 1,
          marginLeft: 6,
          display: 'inline-block',
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
        }}
      >
        {props.filepath}
      </span>
    </FlexRow>
  )
})
