import type { JSXAttributes } from 'utopia-shared/src/types'
import * as EP from '../../../core/shared/element-path'
import {
  getJSXElementFromProjectContents,
  StoryboardFilePath,
} from '../../editor/store/editor-state'
import { renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { TailwindPlugin } from './tailwind-style-plugin'
import { createModifiedProject } from '../../../sample-projects/sample-project-utils.test-utils'
import { TailwindConfigPath } from '../../../core/tailwind/tailwind-config'
import { getTailwindConfigCached } from '../../../core/tailwind/tailwind-compilation'

const Project = createModifiedProject({
  [StoryboardFilePath]: `
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
        data-uid='div'
        className='flex flex-row gap-[12px]'
      />
    </Scene>
  </Storyboard>
)

`,
  [TailwindConfigPath]: `
    const TailwindConfig = {
        content: [],
        theme: { extend: { gap: { enormous: '222px' } } }
    }
    export default TailwindConfig
`,
})

describe('tailwind style plugin', () => {
  it('can set Tailwind class', async () => {
    const editor = await renderTestEditorWithModel(Project, 'await-first-dom-report')
    const target = EP.fromString('sb/scene/div')
    const updatedEditor = TailwindPlugin(null).updateStyles(
      editor.getEditorState().editor,
      target,
      [
        { type: 'set', property: 'gap', value: '222px' },
        { type: 'set', property: 'display', value: 'flex' },
        { type: 'set', property: 'flexDirection', value: 'column' },
      ],
    )
    const normalizedElement = getJSXElementFromProjectContents(
      target,
      updatedEditor.editorStateWithChanges.projectContents,
    )!
    expect(formatJSXAttributes(normalizedElement.props)).toEqual({
      className: 'flex flex-col gap-[222px]',
      'data-uid': 'div',
    })
  })

  it('can set Tailwind class, with custom values in the Tailwind config', async () => {
    const editor = await renderTestEditorWithModel(Project, 'await-first-dom-report')
    const target = EP.fromString('sb/scene/div')
    const updatedEditor = TailwindPlugin(
      getTailwindConfigCached(editor.getEditorState().editor),
    ).updateStyles(editor.getEditorState().editor, target, [
      { type: 'set', property: 'gap', value: '222px' },
      { type: 'set', property: 'display', value: 'flex' },
      { type: 'set', property: 'flexDirection', value: 'column' },
    ])
    const normalizedElement = getJSXElementFromProjectContents(
      target,
      updatedEditor.editorStateWithChanges.projectContents,
    )!
    expect(formatJSXAttributes(normalizedElement.props)).toEqual({
      className: 'flex flex-col gap-enormous',
      'data-uid': 'div',
    })
  })
})

function formatJSXAttributes(attributes: JSXAttributes) {
  return attributes.reduce((acc: Record<string, unknown>, attribute) => {
    if (attribute.type === 'JSX_ATTRIBUTES_SPREAD' || attribute.value.type !== 'ATTRIBUTE_VALUE') {
      return acc
    }
    return { ...acc, [attribute.key]: attribute.value.value }
  }, {})
}
