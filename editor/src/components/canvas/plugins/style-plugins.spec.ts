import { right } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
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
