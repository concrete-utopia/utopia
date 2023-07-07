import * as React from 'react'
import { Tooltip } from '../uuiui'
import { getNamedPath } from './react-helpers'

describe('getNamedPath', () => {
  it('finds name of react class component', () => {
    const TippyTooltip = (
      <Tooltip title='Wrap selection in div' placement='bottom' data-testid='test-tooltip' />
    )

    expect(getNamedPath(TippyTooltip)).toMatchInlineSnapshot(
      `"/Tooltip:data-testid='test-tooltip'"`,
    )
  })
})
