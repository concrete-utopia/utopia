import React from 'react'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { OptionChainControl } from '../../../controls/option-chain-control'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import utils from '../../../../../utils/utils'
import { useInspectorStyleInfo } from '../../../common/property-path-hooks'

export const TextAlignControl = () => {
  const textAlignMetadata = useInspectorStyleInfo('textAlign')

  const textAlignContextMenuItems = utils.stripNulls([
    textAlignMetadata.controlStyles.unsettable
      ? addOnUnsetValues(['textAlign'], textAlignMetadata.onUnsetValues)
      : null,
  ])

  return (
    <InspectorContextMenuWrapper
      id='textAlign-context-menu'
      items={textAlignContextMenuItems}
      data={null}
    >
      <OptionChainControl
        id='textAlign'
        key='textAlign'
        testId='textAlign'
        value={textAlignMetadata.value}
        onSubmitValue={textAlignMetadata.onSubmitValue}
        controlStatus={textAlignMetadata.controlStatus}
        controlStyles={textAlignMetadata.controlStyles}
        options={[
          {
            value: 'left',
            icon: {
              category: 'typography',
              type: 'leftAlign',
              color: 'secondary',
              width: 16,
              height: 16,
            },
          },
          {
            value: 'center',
            icon: {
              category: 'typography',
              type: 'centerAlign',
              color: 'secondary',
              width: 16,
              height: 16,
            },
          },
          {
            value: 'right',
            icon: {
              category: 'typography',
              type: 'rightAlign',
              color: 'secondary',
              width: 16,
              height: 16,
            },
          },
          {
            value: 'justify',
            icon: {
              category: 'typography',
              type: 'justify',
              color: 'secondary',
              width: 16,
              height: 16,
            },
          },
        ]}
      />
    </InspectorContextMenuWrapper>
  )
}
