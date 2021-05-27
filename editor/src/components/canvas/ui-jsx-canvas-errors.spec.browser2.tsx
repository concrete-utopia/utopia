import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { act, render } from '@testing-library/react'
import * as React from 'react'

describe('UiJsxCanvas errors', () => {
  it('handles a component that is not imported by throwing a ReferenceError', () => {
    const renderResult = render(<div style={{ width: 50, height: 12 }}>{BakedInStoryboardUID}</div>)
    expect(renderResult.baseElement.nodeName).toEqual('BODY')
  })
})
