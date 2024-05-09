import React from 'react'
import { when } from '../../../../utils/react-conditionals'
import { FlexRow, Icn, Tooltip, colorTheme } from '../../../../uuiui'
import { stopPropagation } from '../../common/inspector-utils'
import { DataCartoucheInner } from './data-reference-cartouche'
import { NO_OP } from '../../../../core/shared/utils'

interface IdentifierExpressionCartoucheControlProps {
  contents: string
  icon: React.ReactChild
  matchType: 'full' | 'partial'
  onOpenDataPicker: () => void
  onDeleteCartouche: () => void
  safeToDelete: boolean
  testId: string
}
export const IdentifierExpressionCartoucheControl = React.memo(
  (props: IdentifierExpressionCartoucheControlProps) => {
    const { onOpenDataPicker, onDeleteCartouche, testId, safeToDelete } = props

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
      />
    )
  },
)
