import * as React from 'react'
import * as EditorActions from '../../editor/actions/action-creators'
import { betterReactMemo } from '../../../uuiui-deps'
import { SimpleFlexRow } from '../../../uuiui'
import { useEditorState } from '../../editor/store/store-hook'

export const FormulaBar = betterReactMemo('FormulaBar', () => {
  const dispatch = useEditorState((store) => store.dispatch, 'TopMenu dispatch')

  return <SimpleFlexRow></SimpleFlexRow>
})
