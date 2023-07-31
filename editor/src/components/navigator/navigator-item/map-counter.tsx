import React from 'react'
import type { NavigatorEntry } from '../../../components/editor/store/editor-state'
import { isRegularNavigatorEntry } from '../../../components/editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { colorTheme } from '../../../uuiui'

interface MapCounterProps {
  testId: string
  navigatorEntry: NavigatorEntry
}

export const MapCounter = React.memo((props: MapCounterProps) => {
  const { testId, navigatorEntry } = props

  const counterValue = useEditorState(
    Substores.metadata,
    (store) => {
      if (!isRegularNavigatorEntry(navigatorEntry)) {
        return false
      }
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        navigatorEntry.elementPath,
      )
      if (MetadataUtils.isJSXMapExpressionFromMetadata(elementMetadata)) {
        return MetadataUtils.getChildrenOrdered(
          store.editor.jsxMetadata,
          store.editor.elementPathTree,
          navigatorEntry.elementPath,
        ).length
      }
      return null
    },
    'MapCounter counterValue',
  )

  if (counterValue == null) {
    return null
  }

  return (
    <div
      data-testid={testId}
      style={{
        display: 'flex',
        width: 22,
        height: 22,
        flexDirection: 'column',
        justifyContent: 'center',
        alignItems: 'center',
        borderRadius: 44,
        backgroundColor: colorTheme.dynamicBlue10.value,
      }}
    >
      <span>{counterValue}</span>
    </div>
  )
})
