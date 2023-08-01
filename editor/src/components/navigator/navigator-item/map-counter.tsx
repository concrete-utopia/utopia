import React from 'react'
import type { NavigatorEntry } from '../../../components/editor/store/editor-state'
import { isRegularNavigatorEntry } from '../../../components/editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { colorTheme } from '../../../uuiui'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'

export const MapCounterTestIdPrefix = 'map-counter-'

export function getMapCounterTestId(path: ElementPath): string {
  return `${MapCounterTestIdPrefix}${EP.toString(path)}`
}

interface MapCounterProps {
  navigatorEntry: NavigatorEntry
}

export const MapCounter = React.memo((props: MapCounterProps) => {
  const { navigatorEntry } = props
  const { elementPath } = navigatorEntry

  const counterValue = useEditorState(
    Substores.metadata,
    (store) => {
      if (!isRegularNavigatorEntry(navigatorEntry)) {
        return false
      }
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        elementPath,
      )
      if (MetadataUtils.isJSXMapExpressionFromMetadata(elementMetadata)) {
        return MetadataUtils.getChildrenOrdered(
          store.editor.jsxMetadata,
          store.editor.elementPathTree,
          elementPath,
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
      data-testid={getMapCounterTestId(elementPath)}
      style={{
        display: 'flex',
        justifyContent: 'center',
        height: 22,
        width: 22,
        alignItems: 'center',
        borderRadius: '50%',
        backgroundColor: colorTheme.dynamicBlue10.value,
      }}
    >
      {counterValue}
    </div>
  )
})
