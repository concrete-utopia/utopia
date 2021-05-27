import { BakedInStoryboardUID, BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { act, render } from '@testing-library/react'
import * as chai from 'chai'
import * as React from 'react'
var expect = chai.expect

describe('UiJsxCanvas errors', () => {
  it('handles a component that is not imported by throwing a ReferenceError', () => {
    const renderResult = render(<div style={{ width: 50, height: 12 }}>{BakedInStoryboardUID}</div>)
    expect(renderResult.baseElement.nodeName).equal('BODY')
  })
})
