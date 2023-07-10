import React from 'react'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import * as EP from '../../../core/shared/element-path'
import { UtopiaStyles } from '../../../uuiui'

export const RootElementIndicator = () => {
  const rootElementIsSelected = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews.some(EP.isRootElementOfInstance),
    'RootElementIndicator aRootElementIsSelected',
  )

  if (rootElementIsSelected) {
    return (
      <div
        key={'root-element-indicator'}
        style={{
          ...UtopiaStyles.noticeStyles.error,
          borderRadius: 6,
          padding: 5,
          margin: 7,
        }}
      >
        <span>A Root Element Is Selected!</span>
      </div>
    )
  } else {
    return null
  }
}
