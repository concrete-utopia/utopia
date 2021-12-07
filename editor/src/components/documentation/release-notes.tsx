/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'

import React from 'react'
import { useColorTheme, SimpleFlexColumn, SimpleFlexRow, UtopiaTheme } from '../../uuiui'
import { H1, H2, PrettyKeys, EM, CalloutPrimary, A } from './documentation-components'

import { GettingStarted } from './getting-started'

export function ReleaseNotesContent() {
  const colorTheme = useColorTheme()

  return (
    <SimpleFlexColumn>
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
