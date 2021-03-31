import * as React from 'react'

import { ControlStyleDefaults } from '../../../common/control-status'
import { UIGridRow } from '../../../widgets/uigrid-row'
import { PositionControl, MarginControl, AlignSelfControl } from './flex-element-controls'
import { PropertyLabel } from '../../../widgets/property-label'
import { createLayoutPropertyPath } from '../../../../../core/layout/layout-helpers-new'
import { betterReactMemo } from '../../../../../uuiui-deps'

const marginProps = [
  createLayoutPropertyPath('marginLeft'),
  createLayoutPropertyPath('marginTop'),
  createLayoutPropertyPath('marginRight'),
  createLayoutPropertyPath('marginBottom'),
]

const alignSelfProp = [createLayoutPropertyPath('alignSelf')]

export const FlexElementSubsection = betterReactMemo('FlexElementSubsection', () => {
  return (
    <>
      <UIGridRow tall padded={true} type='<---1fr--->|------172px-------|'>
        <PropertyLabel
          target={marginProps}
          style={{ paddingBottom: 12 }}
          propNamesToUnset={['all margins']}
        >
          Margin
        </PropertyLabel>
        <MarginControl />
      </UIGridRow>
      <UIGridRow padded={true} type='<---1fr--->|------172px-------|'>
        <PropertyLabel target={alignSelfProp}>Align Self</PropertyLabel>
        <AlignSelfControl />
      </UIGridRow>
    </>
  )
})
