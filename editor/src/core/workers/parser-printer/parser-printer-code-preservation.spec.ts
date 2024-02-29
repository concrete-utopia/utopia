import { parseThenPrint } from './parser-printer.test-utils'
import { applyPrettier } from 'utopia-vscode-common'

describe('Parsing and then printing code', () => {
  ;['var', 'let', 'const'].forEach((varLetOrConst) => {
    it(`retains the variable declaration keyword ${varLetOrConst}`, () => {
      const code = applyPrettier(
        `export ${varLetOrConst} whatever = (props) => {
          return <div data-uid='aaa' />
        }`,
        false,
      ).formatted

      const parsedThenPrinted = parseThenPrint('/index.js', code)
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

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('retains a parenthesized expression body on an arrow function component', () => {
    const code = applyPrettier(
      `export const whatever = (props) => (
        <div data-uid='aaa' />
      )`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('retains a non-parenthesized expression body on an arrow function component', () => {
    const code = applyPrettier(
      `export const whatever = (props) => <div data-uid='aaa' />`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('retains a block expression body on an arrow function component', () => {
    const code = applyPrettier(
      `export const whatever = (props) => {
        return <div data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('does not surround a jsx attribute value in braces when it was not previously surrounded in braces', () => {
    const code = applyPrettier(
      `export const whatever = (props) => {
        return <div data-something='something' data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('does not include data-uid attributes in top level arbitrary blocks', () => {
    const code = applyPrettier(
      `
import { GithubPicker } from "react-color";

function Picker() {
  const [color, setColor] = useThemeContext();
  const [visible, setVisible] = usePickerVisibilityContext();

  if (visible) {
    return <GithubPicker
      style={{ position: "absolute" }}
      triangle="hide"
      color={color}
      onChange={(c) => {
        setColor(c.hex);
        setVisible(false);
      }}
    />
  } else {
    return null
  }
}
`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  xit('does not remove the braces surrounding a jsx attribute value', () => {
    const code = applyPrettier(
      `export const whatever = (props) => {
        return <div data-something={'something'} data-uid='aaa' />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
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

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('does not remove a trailing export default statement for anything else', () => {
    const code = applyPrettier(
      `const Thing = 1

      export default Thing`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
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

    const parsedThenPrinted = parseThenPrint('/index.js', code)
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

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it(`preserves arbitrary code around import and export statements`, () => {
    const code = applyPrettier(
      `
      const a = 1
      /** @jsx jsx */
      import React, { useEffect, useRef } from 'react'
      const b = 2
      import { extend, useThree, useFrame } from 'react-three-fiber'
      const c = 3

      export const whatever = (props) => {
        return <div data-uid='aaa' />
      }
      
      const d = 4
      export { d }
      const e = 5
      export { e as SomethingElse }
      const f = 6`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('for jsx boolean attributes use shorthand style for true values, and explicit style for false values', () => {
    const code = applyPrettier(
      `
      export const whatever = (props) => {
        return <div data-uid='aaa' data-trueprop data-falseprop={false} />
      }
      `,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('retains empty lines in the code', () => {
    const code = applyPrettier(
      `
      import * as React from 'react'
      import { 
        Loads,
        AndLoads,
        AndMore,
        Just,
        So,
        Many,
        Different,
        Things,
        Enough,
        To,
        Force,
        Onto,
        Multiple,
        Lines
      } from 'some-library'
      import { Scene, Storyboard, View, Group } from 'utopia-api'
      
      const { m } = require('./thing')
      const { n } = require('./thing')
      const { o } = require('./thing')
      
      export function doThing() {
        return 1
      }
      
      export default function App(props) {
        return <div data-uid='aaa' />
      }
      
      export const whatever = (props) => {
        return <div data-uid='aab' />
      }

      export var storyboard = (
        <Storyboard data-uid='bbb'>
          <Scene
            style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
            data-uid='ccc'
          >
            <App data-uid='app' />
          </Scene>
        </Storyboard>
      )
      
      const a = 1
      const b = 10
      const c = 100
      
      const thing1 = {
        a: 10,
      }
      
      const thing2 = {
        b: 100,
      }
      
      export { thing1, thing2 }
      export { default as p } from './thing'
      `,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('Simple JSIdentifier', () => {
    const code = applyPrettier(
      `const element = 'cica'
      
      export function MyComponent(props) {
        return <div data-uid='aaa' myProp={element} />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('Simple JSPropertyAccess', () => {
    const code = applyPrettier(
      `const element = {cica: 'cica'}
      
      export function MyComponent(props) {
        return <div data-uid='aaa' myProp={element.cica} />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('Optional chaining property access operator', () => {
    const code = applyPrettier(
      `const element = {cica: 'cica'}
      
      export function MyComponent(props) {
        return <div data-uid='aaa' myProp={element?.cica} />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('Simple JSElementAccess', () => {
    const code = applyPrettier(
      `const element = {cica: 'cica'}
      
      export function MyComponent(props) {
        return <div data-uid='aaa' myProp={element['cica']}  myPro2={element[0]} />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('Optional chaining element access operator', () => {
    const code = applyPrettier(
      `const element = {cica: 'cica'}
      
      export function MyComponent(props) {
        return <div data-uid='aaa' myProp={element?.['cica']}  myPro2={element?.[0]} />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })

  it('Long chained property access and element access', () => {
    const code = applyPrettier(
      `const element = {cica: 'cica'}
      
      export function MyComponent(props) {
        return <div data-uid='aaa' myProp={element.cica[0]['left'].right}  myPropOptional={element.cica[0]?.['left']?.right} />
      }`,
      false,
    ).formatted

    const parsedThenPrinted = parseThenPrint('/index.js', code)
    expect(parsedThenPrinted).toEqual(code)
  })
})
