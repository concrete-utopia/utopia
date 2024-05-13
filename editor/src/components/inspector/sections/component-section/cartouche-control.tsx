import React from 'react'
import { when } from '../../../../utils/react-conditionals'
import { FlexRow, Icn, Tooltip, colorTheme } from '../../../../uuiui'
import { stopPropagation } from '../../common/inspector-utils'
import { DataCartoucheInner } from './data-reference-cartouche'
import { NO_OP } from '../../../../core/shared/utils'
import type { ElementPath, PropertyPath } from '../../../../core/shared/project-file-types'
import * as EPP from '../../../template-property-path'
import { traceDataFromProp } from '../../../../core/data-tracing/data-tracing'
import { Substores, useEditorState } from '../../../editor/store/store-hook'

interface IdentifierExpressionCartoucheControlProps {
  contents: string
  icon: React.ReactChild
  matchType: 'full' | 'partial'
  onOpenDataPicker: () => void
  onDeleteCartouche: () => void
  safeToDelete: boolean
  testId: string
  propertyPath: PropertyPath
  elementPath: ElementPath
}
export const IdentifierExpressionCartoucheControl = React.memo(
  (props: IdentifierExpressionCartoucheControlProps) => {
    const { onOpenDataPicker, onDeleteCartouche, testId, safeToDelete } = props

    const isDataComingFromHookResult = useEditorState(
      Substores.projectContentsAndMetadata,
      (store) =>
        traceDataFromProp(
          EPP.create(props.elementPath, props.propertyPath),
          store.editor.jsxMetadata,
          store.editor.projectContents,
          [],
        ).type === 'hook-result',
      'IdentifierExpressionCartoucheControl trace',
    )

    return (
      <DataCartoucheInner
        contentsToDisplay={{ label: props.contents, type: 'reference' }}
        onClick={onOpenDataPicker}
        selected={false}
        onDoubleClick={NO_OP}
        safeToDelete={safeToDelete}
        onDelete={onDeleteCartouche}
        testId={testId}
        inverted={false}
        contentIsComingFromServer={isDataComingFromHookResult}
      />
    )
  },
)
