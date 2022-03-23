import { elementPath } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { canvasRectangle } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { KeyCharacter } from '../../../utils/keyboard'
import { emptyModifiers, Modifiers } from '../../../utils/modifiers'
import { EditorState } from '../../editor/store/editor-state'
import { foldAndApplyCommands } from '../commands/commands'
import {
  getEditorState,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../ui-jsx.test-utils'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import {
  createInteractionViaKeyboard,
  InteractionSession,
  StrategyState,
} from './interaction-state'
import { keyboardAbsoluteMoveStrategy } from './keyboard-absolute-move-strategy'

function prepareEditorState(codeSnippet: string, selectedViews: Array<ElementPath>): EditorState {
  return {
    ...getEditorState(makeTestProjectCodeWithSnippet(codeSnippet)),
    selectedViews: selectedViews,
  }
}

function pressKeys(
  editorState: EditorState,
  keys: Array<KeyCharacter>,
  modifiers: Modifiers,
): EditorState {
  const interactionSession: InteractionSession = {
    ...createInteractionViaKeyboard(
      keys,
      modifiers,
      null as any, // the strategy does not use this
    ),
    metadata: null as any, // the strategy does not use this
  }

  const strategyResult = keyboardAbsoluteMoveStrategy.apply(
    pickCanvasStateFromEditorState(editorState),
    interactionSession,
    {
      currentStrategy: null as any, // the strategy does not use this
      currentStrategyFitness: null as any, // the strategy does not use this
      currentStrategyCommands: null as any, // the strategy does not use this
      accumulatedPatches: null as any, // the strategy does not use this
      commandDescriptions: null as any, // the strategy does not use this
      sortedApplicableStrategies: null as any, // the strategy does not use this
      startingMetadata: {
        'scene-aaa/app-entity:aaa/bbb': {
          elementPath: elementPath([
            ['scene-aaa', 'app-entity'],
            ['aaa', 'bbb'],
          ]),
          specialSizeMeasurements: {
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          } as SpecialSizeMeasurements,
        } as ElementInstanceMetadata,
      },
    } as StrategyState,
  )

  const finalEditor = foldAndApplyCommands(
    editorState,
    editorState,
    [],
    [],
    strategyResult,
    'permanent',
  ).editorState

  return finalEditor
}

const shiftModifier: Modifiers = {
  alt: false,
  cmd: false,
  ctrl: false,
  shift: true,
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
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: ${50 + moveX}, top: ${
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
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '50px', top: '50px', width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '${
            50 + moveX
          }px', top: '${50 + moveY}px', width: 250, height: 300 }}
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
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', right: 50, bottom: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', right: ${50 - moveX}
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
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50, top: 50, right: 50, bottom: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: ${50 + moveX}, top: ${
            50 + moveY
          } , right: ${50 - moveX}, bottom: ${50 - moveY}, width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
        ),
      )
    },
  )

  // TODO needs design review
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
    'Key %s with modifiers %o keeps expressions intact',
    async (keys: Array<KeyCharacter>, modifiers: Modifiers, moveX: number, moveY: number) => {
      const targetElement = elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: 50 + 5, top: 50 + props.top, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        testPrintCodeFromEditorState(initialEditor),
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
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ])

      const initialEditor: EditorState = prepareEditorState(
        `
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '25%', top: '25%', width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `,
        [targetElement],
      )

      const finalEditor = pressKeys(initialEditor, keys, modifiers)

      expect(testPrintCodeFromEditorState(finalEditor)).not.toEqual(
        expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
          makeTestProjectCodeWithSnippet(
            `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', left: '${
            25 + moveX * 0.25
          }%', top: '${25 + moveY * 0.25}%', width: 250, height: 300 }}
          data-uid='bbb'
        />
      </View>`,
          ),
        ),
      )
    },
  )
})
