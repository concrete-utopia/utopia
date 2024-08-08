import { elementPath } from '../../../../core/shared/element-path'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { KeyCharacter } from '../../../../utils/keyboard'
import type { Modifiers } from '../../../../utils/modifiers'
import { cmdModifier, shiftCmdModifier } from '../../../../utils/modifiers'
import type { EditorState } from '../../../editor/store/editor-state'
import {
  getEditorState,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../../ui-jsx.test-utils'
import { keyboardAbsoluteResizeStrategy } from './keyboard-absolute-resize-strategy'
import { pressKeys } from './keyboard-interaction.test-utils'

function prepareEditorState(codeSnippet: string, selectedViews: Array<ElementPath>): EditorState {
  return {
    ...getEditorState(makeTestProjectCodeWithSnippet(codeSnippet)),
    selectedViews: selectedViews,
  }
}

describe('Keyboard Absolute Resize Strategy', () => {
  it.each([
    [['left'] as Array<KeyCharacter>, cmdModifier, -1, 0],
    [['right'] as Array<KeyCharacter>, cmdModifier, 1, 0],
    [['up'] as Array<KeyCharacter>, cmdModifier, 0, -1],
    [['down'] as Array<KeyCharacter>, cmdModifier, 0, 1],
    [['left', 'up'] as Array<KeyCharacter>, cmdModifier, -1, -1],
    [['left', 'right'] as Array<KeyCharacter>, cmdModifier, 0, 0],
    [['left'] as Array<KeyCharacter>, shiftCmdModifier, -10, 0],
    [['right'] as Array<KeyCharacter>, shiftCmdModifier, 10, 0],
    [['up'] as Array<KeyCharacter>, shiftCmdModifier, 0, -10],
    [['down'] as Array<KeyCharacter>, shiftCmdModifier, 0, 10],
    [['left', 'up'] as Array<KeyCharacter>, shiftCmdModifier, -10, -10],
    [['left', 'right'] as Array<KeyCharacter>, shiftCmdModifier, 0, 0],
  ])(
    'Key %s with modifiers %o works with a TL pinned absolute element',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, deltaW: number, deltaH: number) => {
      const targetElement = elementPath([
        ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keyboardAbsoluteResizeStrategy, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: ${
            250 + deltaW
          }, height: ${300 + deltaH} }}
          data-uid='bbb'
        />
      </View>`,
        ),
      )
    },
  )

  it.each([
    [['left'] as Array<KeyCharacter>, cmdModifier, -1, 0],
    [['right'] as Array<KeyCharacter>, cmdModifier, 1, 0],
    [['up'] as Array<KeyCharacter>, cmdModifier, 0, -1],
    [['down'] as Array<KeyCharacter>, cmdModifier, 0, 1],
    [['left', 'up'] as Array<KeyCharacter>, cmdModifier, -1, -1],
    [['left', 'right'] as Array<KeyCharacter>, cmdModifier, 0, 0],
    [['left'] as Array<KeyCharacter>, shiftCmdModifier, -10, 0],
    [['right'] as Array<KeyCharacter>, shiftCmdModifier, 10, 0],
    [['up'] as Array<KeyCharacter>, shiftCmdModifier, 0, -10],
    [['down'] as Array<KeyCharacter>, shiftCmdModifier, 0, 10],
    [['left', 'up'] as Array<KeyCharacter>, shiftCmdModifier, -10, -10],
    [['left', 'right'] as Array<KeyCharacter>, shiftCmdModifier, 0, 0],
  ])(
    'Key %s with modifiers %o works with a TL pinned absolute element with px values',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, deltaW: number, deltaH: number) => {
      const targetElement = elementPath([
        ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50px', top: '50px', width: '250px', height: '300px' }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keyboardAbsoluteResizeStrategy, keys, modifiers)

      const horizontalKeysPressed = keys.some((k) => ['left', 'right'].includes(k))
      const verticalKeysPressed = keys.some((k) => ['up', 'down'].includes(k))

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50px', top: '50px', width: ${
            !horizontalKeysPressed ? "'250px'" : 250 + deltaW
          }, height: ${!verticalKeysPressed ? "'300px'" : 300 + deltaH} }}
          data-uid='bbb'
        />
      </View>`,
        ),
      )
    },
  )

  it.each([
    [['left'] as Array<KeyCharacter>, cmdModifier, -1, 0],
    [['right'] as Array<KeyCharacter>, cmdModifier, 1, 0],
    [['up'] as Array<KeyCharacter>, cmdModifier, 0, -1],
    [['down'] as Array<KeyCharacter>, cmdModifier, 0, 1],
    [['left', 'up'] as Array<KeyCharacter>, cmdModifier, -1, -1],
    [['left', 'right'] as Array<KeyCharacter>, cmdModifier, 0, 0],
    [['left'] as Array<KeyCharacter>, shiftCmdModifier, -10, 0],
    [['right'] as Array<KeyCharacter>, shiftCmdModifier, 10, 0],
    [['up'] as Array<KeyCharacter>, shiftCmdModifier, 0, -10],
    [['down'] as Array<KeyCharacter>, shiftCmdModifier, 0, 10],
    [['left', 'up'] as Array<KeyCharacter>, shiftCmdModifier, -10, -10],
    [['left', 'right'] as Array<KeyCharacter>, shiftCmdModifier, 0, 0],
  ])(
    'Key %s with modifiers %o works with a RB pinned absolute element',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, deltaW: number, deltaH: number) => {
      const targetElement = elementPath([
        ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', right: 100, bottom: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keyboardAbsoluteResizeStrategy, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', right: ${100 - deltaW}
          , bottom: ${50 - deltaH}, width: ${250 + deltaW}, height: ${300 + deltaH} }}
          data-uid='bbb'
        />
      </View>`,
        ),
      )
    },
  )

  it.each([
    [['left'] as Array<KeyCharacter>, cmdModifier, -1, 0],
    [['right'] as Array<KeyCharacter>, cmdModifier, 1, 0],
    [['up'] as Array<KeyCharacter>, cmdModifier, 0, -1],
    [['down'] as Array<KeyCharacter>, cmdModifier, 0, 1],
    [['left', 'up'] as Array<KeyCharacter>, cmdModifier, -1, -1],
    [['left', 'right'] as Array<KeyCharacter>, cmdModifier, 0, 0],
    [['left'] as Array<KeyCharacter>, shiftCmdModifier, -10, 0],
    [['right'] as Array<KeyCharacter>, shiftCmdModifier, 10, 0],
    [['up'] as Array<KeyCharacter>, shiftCmdModifier, 0, -10],
    [['down'] as Array<KeyCharacter>, shiftCmdModifier, 0, 10],
    [['left', 'up'] as Array<KeyCharacter>, shiftCmdModifier, -10, -10],
    [['left', 'right'] as Array<KeyCharacter>, shiftCmdModifier, 0, 0],
  ])(
    'Key %s with modifiers %o works with a TLRB pinned absolute element',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, deltaW: number, deltaH: number) => {
      const targetElement = elementPath([
        ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, right: 100, bottom: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keyboardAbsoluteResizeStrategy, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, right: ${
            100 - deltaW
          }, bottom: ${50 - deltaH}, width: ${250 + deltaW}, height: ${300 + deltaH} }}
          data-uid='bbb'
        />
      </View>`,
        ),
      )
    },
  )

  // TODO needs design review
  it.each([
    [['left'] as Array<KeyCharacter>, cmdModifier, -1, 0],
    [['right'] as Array<KeyCharacter>, cmdModifier, 1, 0],
    [['up'] as Array<KeyCharacter>, cmdModifier, 0, -1],
    [['down'] as Array<KeyCharacter>, cmdModifier, 0, 1],
    [['left', 'up'] as Array<KeyCharacter>, cmdModifier, -1, -1],
    [['left', 'right'] as Array<KeyCharacter>, cmdModifier, 0, 0],
    [['left'] as Array<KeyCharacter>, shiftCmdModifier, -10, 0],
    [['right'] as Array<KeyCharacter>, shiftCmdModifier, 10, 0],
    [['up'] as Array<KeyCharacter>, shiftCmdModifier, 0, -10],
    [['down'] as Array<KeyCharacter>, shiftCmdModifier, 0, 10],
    [['left', 'up'] as Array<KeyCharacter>, shiftCmdModifier, -10, -10],
    [['left', 'right'] as Array<KeyCharacter>, shiftCmdModifier, 0, 0],
  ])(
    'Key %s with modifiers %o replaces expressions and fires a toast',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, deltaW: number, deltaH: number) => {
      const targetElement = elementPath([
        ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250 + props.width, height: 300 + props.height }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keyboardAbsoluteResizeStrategy, keys, modifiers)

      const horizontalKeysPressed = keys.some((k) => ['left', 'right'].includes(k))
      const verticalKeysPressed = keys.some((k) => ['up', 'down'].includes(k))

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: ${
              !horizontalKeysPressed ? '250 + props.width' : 250 + deltaW
            }, height: ${!verticalKeysPressed ? '300 + props.height' : 300 + deltaH} }}
            data-uid='bbb'
          />
        </View>`,
        ),
      )
    },
  )
})
