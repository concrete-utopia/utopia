import { parseThenPrint } from './parser-printer.test-utils'
import { applyPrettier } from './prettier-utils'

describe('Parsing and then printing code', () => {
  ;['var', 'let', 'const'].forEach((varLetOrConst) => {
    it(`retains the variable declaration keyword ${varLetOrConst}`, () => {
      const code = applyPrettier(
        `export ${varLetOrConst} whatever = (props) => {
          return <div data-uid='aaa' />
        }`,
        false,
      ).formatted

      const parsedThenPrinted = parseThenPrint(code)
      expect(parsedThenPrinted).toEqual(code)
    })
  })

  it('does not replace a function with a const', () => {
    const code = applyPrettier(
      `export default function whatever(props) {
        return <div data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('retains a parenthesized expression body on an arrow function component', () => {
    const code = applyPrettier(
      `export const whatever = (props) => (
        <div data-uid='aaa' />
      )`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('retains a non-parenthesized expression body on an arrow function component', () => {
    const code = applyPrettier(`export const whatever = (props) => <div data-uid='aaa' />`, false)
      .formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('retains a block expression body on an arrow function component', () => {
    const code = applyPrettier(
      `export const whatever = (props) => {
        return <div data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('does not surround a jsx attribute value in braces when it was not previously surrounded in braces', () => {
    const code = applyPrettier(
      `export const whatever = (props) => {
        return <div data-something='something' data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  xit('does not remove the braces surrounding a jsx attribute value', () => {
    const code = applyPrettier(
      `export const whatever = (props) => {
        return <div data-something={'something'} data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('does not remove a trailing export default statement for a component', () => {
    const code = applyPrettier(
      `const whatever = (props) => {
        return <div data-uid='aaa' />
      }
      export default whatever`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('does not remove a trailing export default statement for anything else', () => {
    const code = applyPrettier(
      `const Thing = 1
      export default whatever`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('preserves import statement order', () => {
    const code = applyPrettier(
      `
      import * as THREE from 'three'
      import React, { useEffect, useRef } from 'react'
      import { extend, useThree, useFrame } from 'react-three-fiber'
      import { EffectComposer } from 'three/examples/jsm/postprocessing/EffectComposer'
      import { ShaderPass } from 'three/examples/jsm/postprocessing/ShaderPass'
      import { RenderPass } from 'three/examples/jsm/postprocessing/RenderPass'
      import { GammaCorrectionShader } from 'three/examples/jsm/shaders/GammaCorrectionShader'
      import { WaterPass } from './shaders/WaterPass'
      import state from '../state'
      export const whatever = (props) => {
        return <div data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })
})
