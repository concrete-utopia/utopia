import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import * as EP from '../../../core/shared/element-path'
import type {
  JSXConditionalExpression,
  JSXElement,
  JSXFragment,
  JSXTextBlock,
} from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { complexDefaultProjectPreParsed } from '../../../sample-projects/sample-project-utils.test-utils'
import type { EditorStorePatched } from '../../editor/store/editor-state'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { DefaultStartingFeatureSwitches, renderTestEditorWithModel } from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import { runWrapInContainerCommand, wrapInContainerCommand } from './wrap-in-container-command'

describe('wrapInContainerCommand', () => {
  function getElement(path: ElementPath, store: EditorStorePatched) {
    return withUnderlyingTargetFromEditorState(path, store.editor, null, (_, element) => element)
  }

  function getPath(uid: string) {
    return EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', uid],
    ])
  }

  it('wraps with a fragment', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      DefaultStartingFeatureSwitches,
      createBuiltInDependenciesList(null),
    )

    const { editor } = renderResult.getEditorState()

    const targetPath = getPath('card-instance')

    const originalElement = getElement(targetPath, renderResult.getEditorState())
    expect(originalElement).not.toBeNull()

    const cmd = wrapInContainerCommand('always', targetPath, 'the-wrapper', 'fragment')
    const result = runWrapInContainerCommand(editor, cmd)
    const patchedEditor = updateEditorStateWithPatches(editor, result.editorStatePatches)

    const wrapperPath = EP.appendToPath(EP.parentPath(targetPath), 'the-wrapper')

    const oldElement = getElement(targetPath, {
      ...renderResult.getEditorState(),
      editor: patchedEditor,
    })
    expect(oldElement).toBeNull()

    const wrapper = getElement(wrapperPath, {
      ...renderResult.getEditorState(),
      editor: patchedEditor,
    })
    expect(wrapper).not.toBeNull()
    expect(wrapper?.type).toEqual('JSX_FRAGMENT')

    const fragment = wrapper as JSXFragment
    expect(fragment.children).toHaveLength(1)

    const child = fragment.children[0]
    expect(child).toEqual(originalElement)
  })

  it('wraps with a conditional', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      DefaultStartingFeatureSwitches,
      createBuiltInDependenciesList(null),
    )

    const { editor } = renderResult.getEditorState()

    const targetPath = getPath('card-instance')

    const originalElement = getElement(targetPath, renderResult.getEditorState())
    expect(originalElement).not.toBeNull()

    const cmd = wrapInContainerCommand('always', targetPath, 'the-wrapper', 'conditional')
    const result = runWrapInContainerCommand(editor, cmd)
    const patchedEditor = updateEditorStateWithPatches(editor, result.editorStatePatches)

    const wrapperPath = EP.appendToPath(EP.parentPath(targetPath), 'the-wrapper')

    const oldElement = getElement(targetPath, {
      ...renderResult.getEditorState(),
      editor: patchedEditor,
    })
    expect(oldElement).toBeNull()

    const wrapper = getElement(wrapperPath, {
      ...renderResult.getEditorState(),
      editor: patchedEditor,
    })
    expect(wrapper).not.toBeNull()
    expect(wrapper?.type).toEqual('JSX_CONDITIONAL_EXPRESSION')

    const conditional = wrapper as JSXConditionalExpression
    // the true branch is the wrapped element
    expect(conditional.whenTrue).toEqual(originalElement)
    // the false branch is the default dummy text
    expect(conditional.whenFalse.type).toEqual('JSX_ELEMENT')
    const whenFalse = conditional.whenFalse as JSXElement
    expect(whenFalse.children).toHaveLength(1)
    expect(whenFalse.children[0].type).toEqual('JSX_TEXT_BLOCK')
    expect((whenFalse.children[0] as JSXTextBlock).text).toEqual('False branch')
  })

  it('does nothing with a wrong path', async () => {
    const renderResult = await renderTestEditorWithModel(
      complexDefaultProjectPreParsed(),
      'dont-await-first-dom-report',
      DefaultStartingFeatureSwitches,
      createBuiltInDependenciesList(null),
    )

    const { editor } = renderResult.getEditorState()

    const targetPath = getPath('THIS-DOES-NOT-EXIST')

    const originalElement = getElement(targetPath, renderResult.getEditorState())
    expect(originalElement).toBeNull()

    const cmd = wrapInContainerCommand('always', targetPath, 'the-wrapper', 'fragment')
    const result = runWrapInContainerCommand(editor, cmd)
    const patchedEditor = updateEditorStateWithPatches(editor, result.editorStatePatches)

    const wrapperPath = EP.appendToPath(EP.parentPath(targetPath), 'the-wrapper')

    const oldElement = getElement(targetPath, {
      ...renderResult.getEditorState(),
      editor: patchedEditor,
    })
    expect(oldElement).toBeNull()

    const wrapper = getElement(wrapperPath, {
      ...renderResult.getEditorState(),
      editor: patchedEditor,
    })
    expect(wrapper).toBeNull()
  })
})
