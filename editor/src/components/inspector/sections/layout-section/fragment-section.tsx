import React from 'react'
import { InspectorSubsectionHeader } from '../../../../uuiui'
import { EditorContractDropdown } from '../../editor-contract-section'

export const FragmentSection = React.memo(() => {
  return (
    <InspectorSubsectionHeader>
      <EditorContractDropdown />
    </InspectorSubsectionHeader>
  )
})
FragmentSection.displayName = 'FragmentSection'
