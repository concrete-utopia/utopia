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

  it('does not include data-uid attributes in top level arbitrary blocks', () => {
    const code = applyPrettier(
      `
import { GithubPicker } from "react-color";
function Picker() {
  const [color, setColor] = useThemeContext();
  const [visible, setVisible] = usePickerVisibilityContext();
  return visible ? (
    <GithubPicker
      style={{ position: "absolute" }}
      triangle="hide"
      color={color}
      onChange={(c) => {
        setColor(c.hex);
        setVisible(false);
      }}
    />
  ) : null;
}
`,
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
      export default Thing`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint(code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('preserves export statements including those with module specifiers', () => {
    const code = applyPrettier(
      `
      const Thing = 1
      const OtherThing = 2
      export { default as Calendar } from './Calendar'
      export { DateLocalizer } from './localizer'
      export { Thing, OtherThing as SomethingElse }
      export { default as momentLocalizer } from './localizers/moment'
      export { default as globalizeLocalizer } from './localizers/globalize'
      export { default as dateFnsLocalizer } from './localizers/date-fns'
      export { default as move } from './utils/move'
      export { views as Views, navigate as Navigate } from './utils/constants'
      `,
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
