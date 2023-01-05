import React from 'react'
import { FlexColumn } from '../../../uuiui'
import { FileBrowser } from '../../filebrowser/filebrowser'
import { DependencyList } from '../dependency-list'
import { GenericExternalResourcesList } from '../external-resources/generic-external-resources-list'
import { GoogleFontsResourcesList } from '../external-resources/google-fonts-resources-list'

export const ContentsPane = React.memo(() => {
  return (
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
  )
})
