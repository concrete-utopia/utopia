import React from 'react'
import { DataCartoucheInner } from './data-reference-cartouche'
import { NO_OP } from '../../../../core/shared/utils'
import type { ElementPath, PropertyPath } from '../../../../core/shared/project-file-types'
import * as EPP from '../../../template-property-path'
import { dataPathSuccess, traceDataFromProp } from '../../../../core/data-tracing/data-tracing'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import type { CartoucheDataType } from './cartouche-ui'

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
  datatype: CartoucheDataType
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
          dataPathSuccess([]),
        ).type === 'hook-result',
      'IdentifierExpressionCartoucheControl trace',
    )

    return (
      <DataCartoucheInner
        contentsToDisplay={{ label: props.contents, type: 'reference' }}
        onClick={NO_OP}
        selected={false}
        onDoubleClick={onOpenDataPicker}
        safeToDelete={safeToDelete}
        onDelete={onDeleteCartouche}
        testId={testId}
        contentIsComingFromServer={isDataComingFromHookResult}
        datatype={props.datatype}
      />
    )
  },
)
