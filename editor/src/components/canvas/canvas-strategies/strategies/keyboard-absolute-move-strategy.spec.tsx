import { elementPath } from '../../../../core/shared/element-path'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { KeyCharacter } from '../../../../utils/keyboard'
import type { Modifiers } from '../../../../utils/modifiers'
import { emptyModifiers, shiftModifier } from '../../../../utils/modifiers'
import type { EditorState } from '../../../editor/store/editor-state'
import {
  getEditorState,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../../ui-jsx.test-utils'
import { keyboardAbsoluteMoveStrategy } from './keyboard-absolute-move-strategy'
import { pressKeys } from './keyboard-interaction.test-utils'

function prepareEditorState(codeSnippet: string, selectedViews: Array<ElementPath>): EditorState {
  return {
    ...getEditorState(makeTestProjectCodeWithSnippet(codeSnippet)),
    selectedViews: selectedViews,
  }
}

describe('Keyboard Absolute Move Strategy', () => {
  it.each([
    [['left'] as Array<KeyCharacter>, emptyModifiers, -1, 0],
    [['right'] as Array<KeyCharacter>, emptyModifiers, 1, 0],
    [['up'] as Array<KeyCharacter>, emptyModifiers, 0, -1],
    [['down'] as Array<KeyCharacter>, emptyModifiers, 0, 1],
    [['left', 'up'] as Array<KeyCharacter>, emptyModifiers, -1, -1],
    [['left', 'right'] as Array<KeyCharacter>, emptyModifiers, 0, 0],
    [['left'] as Array<KeyCharacter>, shiftModifier, -10, 0],
    [['right'] as Array<KeyCharacter>, shiftModifier, 10, 0],
    [['up'] as Array<KeyCharacter>, shiftModifier, 0, -10],
    [['down'] as Array<KeyCharacter>, shiftModifier, 0, 10],
    [['left', 'up'] as Array<KeyCharacter>, shiftModifier, -10, -10],
    [['left', 'right'] as Array<KeyCharacter>, shiftModifier, 0, 0],
  ])(
    'Key %s with modifiers %o works with a TL pinned absolute element',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, moveX: number, moveY: number) => {
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
      const finalEditor = pressKeys(initialEditor, keyboardAbsoluteMoveStrategy, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${50 + moveX}, top: ${
            50 + moveY
          }, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
        ),
      )
    },
  )

  it.each([
    [['left'] as Array<KeyCharacter>, emptyModifiers, -1, 0],
    [['right'] as Array<KeyCharacter>, emptyModifiers, 1, 0],
    [['up'] as Array<KeyCharacter>, emptyModifiers, 0, -1],
    [['down'] as Array<KeyCharacter>, emptyModifiers, 0, 1],
    [['left', 'up'] as Array<KeyCharacter>, emptyModifiers, -1, -1],
    [['left', 'right'] as Array<KeyCharacter>, emptyModifiers, 0, 0],
    [['left'] as Array<KeyCharacter>, shiftModifier, -10, 0],
    [['right'] as Array<KeyCharacter>, shiftModifier, 10, 0],
    [['up'] as Array<KeyCharacter>, shiftModifier, 0, -10],
    [['down'] as Array<KeyCharacter>, shiftModifier, 0, 10],
    [['left', 'up'] as Array<KeyCharacter>, shiftModifier, -10, -10],
    [['left', 'right'] as Array<KeyCharacter>, shiftModifier, 0, 0],
  ])(
    'Key %s with modifiers %o works with a TL pinned absolute element with px values',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, moveX: number, moveY: number) => {
      const targetElement = elementPath([
        ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50px', top: '50px', width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keyboardAbsoluteMoveStrategy, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${
            moveX == 0 ? "'50px'" : 50 + moveX
          }, top: ${moveY == 0 ? "'50px'" : 50 + moveY}, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
        ),
      )
    },
  )

  it.each([
    [['left'] as Array<KeyCharacter>, emptyModifiers, -1, 0],
    [['right'] as Array<KeyCharacter>, emptyModifiers, 1, 0],
    [['up'] as Array<KeyCharacter>, emptyModifiers, 0, -1],
    [['down'] as Array<KeyCharacter>, emptyModifiers, 0, 1],
    [['left', 'up'] as Array<KeyCharacter>, emptyModifiers, -1, -1],
    [['left', 'right'] as Array<KeyCharacter>, emptyModifiers, 0, 0],
    [['left'] as Array<KeyCharacter>, shiftModifier, -10, 0],
    [['right'] as Array<KeyCharacter>, shiftModifier, 10, 0],
    [['up'] as Array<KeyCharacter>, shiftModifier, 0, -10],
    [['down'] as Array<KeyCharacter>, shiftModifier, 0, 10],
    [['left', 'up'] as Array<KeyCharacter>, shiftModifier, -10, -10],
    [['left', 'right'] as Array<KeyCharacter>, shiftModifier, 0, 0],
  ])(
    'Key %s with modifiers %o works with a RB pinned absolute element',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, moveX: number, moveY: number) => {
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

      const finalEditor = pressKeys(initialEditor, keyboardAbsoluteMoveStrategy, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', right: ${100 - moveX}
          , bottom: ${50 - moveY}, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
        ),
      )
    },
  )

  it.each([
    [['left'] as Array<KeyCharacter>, emptyModifiers, -1, 0],
    [['right'] as Array<KeyCharacter>, emptyModifiers, 1, 0],
    [['up'] as Array<KeyCharacter>, emptyModifiers, 0, -1],
    [['down'] as Array<KeyCharacter>, emptyModifiers, 0, 1],
    [['left', 'up'] as Array<KeyCharacter>, emptyModifiers, -1, -1],
    [['left', 'right'] as Array<KeyCharacter>, emptyModifiers, 0, 0],
    [['left'] as Array<KeyCharacter>, shiftModifier, -10, 0],
    [['right'] as Array<KeyCharacter>, shiftModifier, 10, 0],
    [['up'] as Array<KeyCharacter>, shiftModifier, 0, -10],
    [['down'] as Array<KeyCharacter>, shiftModifier, 0, 10],
    [['left', 'up'] as Array<KeyCharacter>, shiftModifier, -10, -10],
    [['left', 'right'] as Array<KeyCharacter>, shiftModifier, 0, 0],
  ])(
    'Key %s with modifiers %o works with a TLRB pinned absolute element',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, moveX: number, moveY: number) => {
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

      const finalEditor = pressKeys(initialEditor, keyboardAbsoluteMoveStrategy, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${50 + moveX}, top: ${
            50 + moveY
          } , right: ${100 - moveX}, bottom: ${50 - moveY}, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
        ),
      )
    },
  )

  it.each([
    [['left'] as Array<KeyCharacter>, emptyModifiers, -1, 0],
    [['right'] as Array<KeyCharacter>, emptyModifiers, 1, 0],
    [['up'] as Array<KeyCharacter>, emptyModifiers, 0, -1],
    [['down'] as Array<KeyCharacter>, emptyModifiers, 0, 1],
    [['left', 'up'] as Array<KeyCharacter>, emptyModifiers, -1, -1],
    [['left', 'right'] as Array<KeyCharacter>, emptyModifiers, 0, 0],
    [['left'] as Array<KeyCharacter>, shiftModifier, -10, 0],
    [['right'] as Array<KeyCharacter>, shiftModifier, 10, 0],
    [['up'] as Array<KeyCharacter>, shiftModifier, 0, -10],
    [['down'] as Array<KeyCharacter>, shiftModifier, 0, 10],
    [['left', 'up'] as Array<KeyCharacter>, shiftModifier, -10, -10],
    [['left', 'right'] as Array<KeyCharacter>, shiftModifier, 0, 0],
  ])(
    'Key %s with modifiers %o replace expressions and fires toast',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, moveX: number, moveY: number) => {
      const targetElement = elementPath([
        ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50 + 5, top: 50 + props.top, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keyboardAbsoluteMoveStrategy, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${
              moveX == 0 ? '50 + 5' : 50 + moveX
            }, top: ${moveY == 0 ? '50 + props.top' : 50 + moveY}, width: 250, height: 300 }}
            data-uid='bbb'
          />
        </View>`,
        ),
      )
    },
  )

  it.each([
    [['left'] as Array<KeyCharacter>, emptyModifiers, -1, 0],
    [['right'] as Array<KeyCharacter>, emptyModifiers, 1, 0],
    [['up'] as Array<KeyCharacter>, emptyModifiers, 0, -1],
    [['down'] as Array<KeyCharacter>, emptyModifiers, 0, 1],
    [['left', 'up'] as Array<KeyCharacter>, emptyModifiers, -1, -1],
    [['left', 'right'] as Array<KeyCharacter>, emptyModifiers, 0, 0],
    [['left'] as Array<KeyCharacter>, shiftModifier, -10, 0],
    [['right'] as Array<KeyCharacter>, shiftModifier, 10, 0],
    [['up'] as Array<KeyCharacter>, shiftModifier, 0, -10],
    [['down'] as Array<KeyCharacter>, shiftModifier, 0, 10],
    [['left', 'up'] as Array<KeyCharacter>, shiftModifier, -10, -10],
    [['left', 'right'] as Array<KeyCharacter>, shiftModifier, 0, 0],
  ])(
    'Key %s with modifiers %o works with percentages',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, moveX: number, moveY: number) => {
      const targetElement = elementPath([
        ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '12.5%', top: '12.5%', width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keyboardAbsoluteMoveStrategy, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).not.toEqual(
        expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
          makeTestProjectCodeWithSnippet(
            `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '${
            12.5 + moveX * 0.25
          }%', top: '${12.5 + moveY * 0.25}%', width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
          ),
        ),
      )
    },
  )
})
