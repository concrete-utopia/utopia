import * as React from 'react'

import { ControlStyleDefaults } from '../../../common/control-status'
import { betterReactMemo } from 'uuiui-deps'
import { GridRow } from '../../../widgets/grid-row'
import { PositionControl, MarginControl, AlignSelfControl } from './flex-element-controls'
import { PropertyLabel } from '../../../widgets/property-label'
import { createLayoutPropertyPath } from '../../../../../core/layout/layout-helpers-new'

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
      <GridRow tall padded={true} type='<---1fr--->|------172px-------|'>
        <PropertyLabel
          target={marginProps}
          style={{ paddingBottom: 12 }}
          propNamesToUnset={['all margins']}
        >
          Margin
        </PropertyLabel>
        <MarginControl />
      </GridRow>
      <GridRow padded={true} type='<---1fr--->|------172px-------|'>
        <PropertyLabel target={alignSelfProp}>Align Self</PropertyLabel>
        <AlignSelfControl />
      </GridRow>
    </>
  )
})
