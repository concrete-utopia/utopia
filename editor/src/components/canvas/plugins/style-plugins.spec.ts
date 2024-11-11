import type { JSExpression } from 'utopia-shared/src/types'
import type { PropsOrJSXAttributes } from '../../../core/model/element-metadata-utils'
import { right } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import { assertNever } from '../../../core/shared/utils'
import { getTailwindConfigCached } from '../../../core/tailwind/tailwind-compilation'
import { setFeatureForUnitTestsUseInDescribeBlockOnly } from '../../../utils/utils.test-utils'
import { getJSXElementFromProjectContents } from '../../editor/store/editor-state'
import { renderTestEditorWithCode } from '../ui-jsx.test-utils'
import { InlineStylePlugin } from './inline-style-plugin'
import { TailwindPlugin } from './tailwind-style-plugin'

describe('style plugins', () => {
  describe('inline style plugin', () => {
    it('can read width from element props', async () => {
      const editor = await renderTestEditorWithCode(Project, 'await-first-dom-report')

      const element = getJSXElementFromProjectContents(
        EP.fromString('sb/scene'),
        editor.getEditorState().editor.projectContents,
      )!
      const width = InlineStylePlugin.readStyleFromElementProps(right(element.props), 'width')
      expect(width).toEqual({
        tag: null,
        type: 'property',
        value: {
          unit: null,
          value: 700,
        },
      })
    })
    it('can read width based on element path', async () => {
      const editor = await renderTestEditorWithCode(Project, 'await-first-dom-report')
      const width = InlineStylePlugin.styleInfoFactory({
        projectContents: editor.getEditorState().editor.projectContents,
        metadata: editor.getEditorState().editor.jsxMetadata,
        elementPathTree: editor.getEditorState().editor.elementPathTree,
      })(EP.fromString('sb/scene'))!.width

      expect(width).toEqual({
        tag: null,
        type: 'property',
        value: { unit: null, value: 700 },
      })
    })

    it('can write width to element props', async () => {
      const editor = await renderTestEditorWithCode(Project, 'await-first-dom-report')
      const element = getJSXElementFromProjectContents(
        EP.fromString('sb/scene'),
        editor.getEditorState().editor.projectContents,
      )!
      const updatedProps = InlineStylePlugin.updateCSSPropertyInProps(element.props, [
        { type: 'set', property: 'width', value: 123 },
      ])
      expect(getSimplifiedProps(updatedProps)).toEqual([
        ['id', 'scene'],
        ['commentId', 'scene'],
        ['data-uid', 'scene'],
        [
          'style',
          [
            ['width', 123], // <- the updated prop value
            ['height', 759],
            ['position', 'absolute'],
            ['left', 212],
            ['top', 128],
          ],
        ],
      ])
    })
  })

  describe('tailwind style plugin', () => {
    setFeatureForUnitTestsUseInDescribeBlockOnly('Tailwind', true)

    it('can read width from element props', async () => {
      const editor = await renderTestEditorWithCode(Project, 'await-first-dom-report')

      const element = getJSXElementFromProjectContents(
        EP.fromString('sb/scene/mydiv/child-1'),
        editor.getEditorState().editor.projectContents,
      )!

      const width = TailwindPlugin(
        getTailwindConfigCached(editor.getEditorState().editor),
      ).readStyleFromElementProps(right(element.props), 'width')

      expect(width).toEqual({
        tag: null,
        type: 'property',
        value: { unit: 'rem', value: 2.5 },
      })
    })

    it('can read width based on element path', async () => {
      const editor = await renderTestEditorWithCode(Project, 'await-first-dom-report')
      const width = TailwindPlugin(
        getTailwindConfigCached(editor.getEditorState().editor),
      ).styleInfoFactory({
        projectContents: editor.getEditorState().editor.projectContents,
        metadata: editor.getEditorState().editor.jsxMetadata,
        elementPathTree: editor.getEditorState().editor.elementPathTree,
      })(EP.fromString('sb/scene/mydiv/child-1'))!.width

      expect(width).toEqual({
        tag: null,
        type: 'property',
        value: { unit: 'rem', value: 2.5 },
      })
    })

    it('can write width to element props', async () => {
      const editor = await renderTestEditorWithCode(Project, 'await-first-dom-report')
      const element = getJSXElementFromProjectContents(
        EP.fromString('sb/scene/mydiv/child-1'),
        editor.getEditorState().editor.projectContents,
      )!
      const updatedProps = TailwindPlugin(
        getTailwindConfigCached(editor.getEditorState().editor),
      ).updateCSSPropertyInProps(element.props, [{ type: 'set', property: 'width', value: 123 }])
      expect(getSimplifiedProps(updatedProps)).toEqual([
        ['className', 'bg-red-500 w-[123px] h-10'], // w-[123px] is the new value
        ['data-uid', 'child-1'],
      ])
    })
  })
})

const Project = `
    import React from 'react'
    import { Scene, Storyboard } from 'utopia-api'
    export var storyboard = (
      <Storyboard data-uid='sb'>
        <Scene
          id='scene'
          commentId='scene'
          data-uid='scene'
          style={{
            width: 700,
            height: 759,
            position: 'absolute',
            left: 212,
            top: 128,
          }}
        >
          <div
            data-uid='mydiv'
            data-testid='mydiv'
            className='top-10 left-10 absolute flex flex-row'
          >
            <div className='bg-red-500 w-10 h-10' data-uid='child-1' />
            <div className='bg-red-500 w-10 h-10' data-uid='child-2' />
          </div>  
        </Scene>
      </Storyboard>
    )
    `

function getSimplifiedJSExpression(expr: JSExpression) {
  return expr.type === 'ATTRIBUTE_VALUE'
    ? expr.value
    : expr.type === 'ATTRIBUTE_NESTED_OBJECT'
    ? expr.content.map((prop): unknown =>
        prop.type === 'SPREAD_ASSIGNMENT'
          ? '<spread assigmnent>'
          : [prop.key, getSimplifiedJSExpression(prop.value)],
      )
    : '<attribute value>'
}

function getSimplifiedProps(props: PropsOrJSXAttributes) {
  if (props.type === 'LEFT') {
    return props.value
  }
  if (props.type === 'RIGHT') {
    return props.value.map((p) => {
      switch (p.type) {
        case 'JSX_ATTRIBUTES_SPREAD':
          return `<spread value>`
        case 'JSX_ATTRIBUTES_ENTRY':
          return [p.key, getSimplifiedJSExpression(p.value)]
        default:
          assertNever(p)
      }
    })
  }
}
