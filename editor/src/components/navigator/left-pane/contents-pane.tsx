/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { FlexColumn } from '../../../uuiui'
import { FileBrowser } from '../../filebrowser/filebrowser'
import { DependencyList } from '../dependency-list'
import { GenericExternalResourcesList } from '../external-resources/generic-external-resources-list'
import { GoogleFontsResourcesList } from '../external-resources/google-fonts-resources-list'

export const ContentsPane = React.memo(() => {
  return (
    <div
      style={{
        overflowY: 'scroll',
        height: '100%',
      }}
    >
      <FlexColumn
        id='leftPaneContents'
        key='leftPaneContents'
        style={{
          display: 'relative',
          alignItems: 'stretch',
          paddingBottom: 50,
        }}
      >
        <FileBrowser />
        <DependencyList />
        <GenericExternalResourcesList />
        <GoogleFontsResourcesList />
      </FlexColumn>
    </div>
  )
})
