/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'

import * as React from 'react'
import { colorTheme, SimpleFlexColumn, SimpleFlexRow, UtopiaTheme } from '../../uuiui'
import { FileTabs } from '../filebrowser/file-tabs'
import { H1, H2, PrettyKeys, EM, CalloutPrimary, A } from './documentation-components'

import { GettingStarted } from './getting-started'

export function ReleaseNotesContent() {
  return (
    <SimpleFlexColumn>
      <SimpleFlexRow
        className='tabRail'
        style={{
          minHeight: 30,
          height: 30,
          borderBottom: `1px solid ${UtopiaTheme.color.subduedBorder.value}`,
          alignItems: 'stretch',
          justifyContent: 'stretch',
          backgroundColor: 'transparent',
          overflowX: 'hidden',
        }}
      >
        <FileTabs />
      </SimpleFlexRow>
      <div
        css={{
          label: 'ReleaseNotesContainer',
          backgroundColor: colorTheme.emphasizedBackground.value,
          fontSize: '16px',
          lineHeight: '26px',
          paddingBottom: 18,
          width: '100%',
          cursor: 'text',
          userSelect: 'text',
          WebkitUserSelect: 'text',
          overflow: 'scroll',
          whiteSpace: 'pre-wrap',
          '&  *': {
            userSelect: 'text',
            WebkitUserSelect: 'text',
          },
        }}
      >
        <GettingStarted />
      </div>
    </SimpleFlexColumn>
  )
}
