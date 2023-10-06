import { absolute } from '../../../utils/utils'
import * as EP from '../../../core/shared/element-path'
import type { EditorState } from '../../editor/store/editor-state'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import { getEditorState, makeTestProjectCodeWithSnippet } from '../ui-jsx.test-utils'
import { updateEditorStateWithPatches } from './commands'
import { reorderElement, runReorderElement } from './reorder-element-command'
import { isJSXElementLike } from '../../../core/shared/element-template'

describe('runReorderElement', () => {
  it.each([
    ['child-0', 1, ['child-1', 'child-0', 'child-2', 'child-3', 'child-4']],
    ['child-0', 2, ['child-1', 'child-2', 'child-0', 'child-3', 'child-4']],
    ['child-0', 4, ['child-1', 'child-2', 'child-3', 'child-4', 'child-0']],
    ['child-2', 1, ['child-0', 'child-2', 'child-1', 'child-3', 'child-4']],
    ['child-2', 3, ['child-0', 'child-1', 'child-3', 'child-2', 'child-4']],
    ['child-4', 0, ['child-4', 'child-0', 'child-1', 'child-2', 'child-3']],
    ['child-4', 2, ['child-0', 'child-1', 'child-4', 'child-2', 'child-3']],
  ])(
    'reorder works for target %s to new index %d',
    async (targetId: string, newIdx: number, expectedChildIds: Array<string>) => {
      const originalEditorState: EditorState = getEditorState(
        makeTestProjectCodeWithSnippet(
          `
      <div
        data-uid="app-outer-div"
        style={{
          display: "flex",
          gap: 10,
        }}
      >
        <div
          data-uid="child-0"
          style={{
            width: 200,
            height: 300,
          }}
        />
        <div
          data-uid="child-1"
          style={{
            width: 200,
            height: 300,
          }}
        />
        <div
          data-uid="child-2"
          style={{
            width: 200,
            height: 300,
          }}
        />
        <div
          data-uid="child-3"
          style={{
            width: 200,
            height: 300,
          }}
        />
        <div
          data-uid="child-4"
          style={{
            width: 200,
            height: 300,
          }}
        />
      </div>
    `,
        ),
      )

      const target = EP.elementPath([
        ['scene-aaa', 'app-entity'],
        ['app-outer-div', targetId],
      ])

      const parent = EP.parentPath(target)

      const reorderCommand = reorderElement('always', target, absolute(newIdx))

      const result = runReorderElement(originalEditorState, reorderCommand)

      const patchedEditor = updateEditorStateWithPatches(
        originalEditorState,
        result.editorStatePatches,
      )

      const children = withUnderlyingTargetFromEditorState(
        parent,
        patchedEditor,
        null,
        (_, element) => {
          if (isJSXElementLike(element)) {
            return element.children
          } else {
            return []
          }
        },
      )

      expect(children).not.toBeNull()

      const childrenUids = children!.map((child) => {
        return child.type === 'JSX_ELEMENT' ? child.uid : null
      })

      expect(childrenUids).toEqual(expectedChildIds)
    },
  )
})
