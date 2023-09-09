/* eslint-disable jest/valid-expect */
/* eslint-disable no-unused-expressions */
import * as React from 'react'
import { sendMouse } from '@web/test-runner-commands'
import '../../../../templates/editor-entry-point'

import { expect } from 'chai'
import { canvasPoint, canvasRectangle, rectContainsPoint } from '../../../../core/shared/math-utils'
import { render } from '@testing-library/react'
import { wait } from '../../../../utils/utils.test-utils'

describe('hello?', () => {
  it('hello!', async () => {
    let clicked = false
    const onClick = () => {
      clicked = true
    }
    const testRender = render(<div onClick={onClick}>Hello World</div>)

    await wait(10000000)
    await sendMouse({ type: 'click', position: [20, 20] })
    expect(testRender.getByText('Hello World')).to.exist
    expect(clicked).to.be.true
  })

  it('hello2!', async () => {
    const testRender = render(<div>Hello World</div>)
    // await wait(10000000)
    expect(testRender.getByText('Hello World')).to.exist
  })
  it('hello3!', async () => {
    const testRender = render(<div>Hello World</div>)
    // await wait(10000000)
    expect(testRender.getByText('Hello World')).to.exist
  })
  it('hello4!', async () => {
    const testRender = render(<div>Hello World</div>)
    // await wait(10000000)
    expect(testRender.getByText('Hello World')).to.exist
  })
  it('hello5!', async () => {
    const testRender = render(<div>Hello World</div>)
    // await wait(10000000)
    expect(testRender.getByText('Hello World')).to.exist
  })
  it('hello6!', async () => {
    const testRender = render(<div>Hello World</div>)
    // await wait(10000000)
    expect(testRender.getByText('Hello World')).to.exist
  })
  it('hello7!', async () => {
    const testRender = render(<div>Hello World</div>)
    // await wait(10000000)
    expect(testRender.getByText('Hello World')).to.exist
  })
  it('hello8!', async () => {
    const testRender = render(<div>Hello World</div>)
    // await wait(10000000)
    expect(testRender.getByText('Hello World')).to.exist
  })
  it('hello9!', async () => {
    const testRender = render(<div>Hello World</div>)
    // await wait(10000000)
    expect(testRender.getByText('Hello World')).to.exist
  })
})
