import * as React from 'react'
import { betterReactMemo } from '../../../uuiui-deps'
import { useEditorState } from '../../editor/store/store-hook'

interface FloatingInsertMenuProps {}

export const FloatingInsertMenu = betterReactMemo(
  'FloatingInsertMenu',
  (props: FloatingInsertMenuProps) => {
    const isVisible = useEditorState(
      (store) => store.editor.floatingInsertMenu.insertMenuOpen,
      'FloatingInsertMenu insertMenuOpen',
    )
    return isVisible ? (
      <div
        style={{
          pointerEvents: 'initial',
          position: 'absolute',
          left: '50%',
          top: '50%',
        }}
      >
        hi!
      </div>
    ) : null
  },
)
