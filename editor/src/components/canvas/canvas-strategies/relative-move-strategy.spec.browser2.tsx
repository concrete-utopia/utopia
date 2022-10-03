import React from 'react'
import { createBuiltInDependenciesList } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { elementPath } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import {
  canvasPoint,
  canvasRectangle,
  CanvasVector,
  localRectangle,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { emptyModifiers } from '../../../utils/modifiers'
import { EditorState } from '../../editor/store/editor-state'
import { foldAndApplyCommands } from '../commands/commands'
import {
  getEditorState,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../ui-jsx.test-utils'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { defaultCustomStrategyState } from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'

const prepareEditorState = (
  codeSnippet: string,
  selectedViews: Array<ElementPath>,
): EditorState => {
  return {
    ...getEditorState(makeTestProjectCodeWithSnippet(codeSnippet)),
    selectedViews: selectedViews,
  }
}

const defaultMetadata: ElementInstanceMetadataMap = {
  'scene-aaa': {
    elementPath: elementPath([['scene-aaa']]),
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity': {
    elementPath: elementPath([['scene-aaa', 'app-entity']]),
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa': {
    elementPath: elementPath([['scene-aaa', 'app-entity'], ['aaa']]),
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa/bbb': {
    elementPath: elementPath([
      ['scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ]),
    specialSizeMeasurements: {
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
    globalFrame: canvasRectangle({ x: 50, y: 50, width: 250, height: 300 }),
    localFrame: localRectangle({ x: 50, y: 50, width: 250, height: 300 }),
  } as ElementInstanceMetadata,
}

function dragByPixels(
  editorState: EditorState,
  vector: CanvasVector,
  metadata: ElementInstanceMetadataMap = defaultMetadata,
): EditorState {
  const interactionSession: InteractionSession = {
    ...createMouseInteractionForTests(
      null as any, // the strategy does not use this
      emptyModifiers, // the strategy does not use this
      null as any, // the strategy does not use this
      vector,
    ),
    latestMetadata: null as any, // the strategy does not use this
    latestAllElementProps: null as any, // the strategy does not use this
    startingTargetParentsToFilterOut: null,
  }

  const strategyResult = absoluteMoveStrategy.apply(
    pickCanvasStateFromEditorState(editorState, createBuiltInDependenciesList(null)),
    interactionSession,
    {
      currentStrategy: null as any, // the strategy does not use this
      currentStrategyFitness: null as any, // the strategy does not use this
      currentStrategyCommands: null as any, // the strategy does not use this
      accumulatedPatches: null as any, // the strategy does not use this
      commandDescriptions: null as any, // the strategy does not use this
      sortedApplicableStrategies: null as any, // the strategy does not use this
      startingMetadata: metadata ?? defaultMetadata,
      startingAllElementProps: {},
      customStrategyState: defaultCustomStrategyState(),
    } as StrategyState,
    'end-interaction',
  )

  expect(strategyResult.customStatePatch).toEqual({})
  expect(strategyResult.status).toEqual('success')

  const finalEditor = foldAndApplyCommands(
    editorState,
    editorState,
    [],
    [],
    strategyResult.commands,
    'end-interaction',
  ).editorState

  return finalEditor
}

const makeParentView = (uid: string, children: (() => string)[]) => {
  return `
    <View data-uid='${uid}'>
      ${children.map((c) => c()).join('\n')}
    </View>
  `
}

const makeView = (uid: string, style: React.CSSProperties) => () => {
  const defaultStyle: React.CSSProperties = {
    backgroundColor: '#f0f',
    width: 200,
    height: 200,
  }
  return `
    <View
      data-uid='${uid}'
      style={${JSON.stringify({
        ...defaultStyle,
        ...style,
      }).replace(/"/g, "'")}}
    />
  `
}

describe('Relative move', () => {
  describe('when the element position is relative', () => {
    it('does not trigger when the threshold is not met', async () => {
      const targetElement = elementPath([
        ['scene-foo', 'app-entity'],
        ['foo', 'bar'],
      ])

      const initialEditor = prepareEditorState(
        makeParentView('foo', [
          makeView('bar', {
            position: 'relative',
          }),
        ]),
        [targetElement],
      )

      const finalEditor = dragByPixels(initialEditor, canvasPoint({ x: 1, y: 1 }))
      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          makeParentView('foo', [
            makeView('bar', {
              position: 'relative',
            }),
          ]),
        ),
      )
    })

    describe('when the element has no offsets', () => {
      it('sets the TL offsets', async () => {
        const targetElement = elementPath([
          ['scene-foo', 'app-entity'],
          ['foo', 'bar'],
        ])

        const initialEditor = prepareEditorState(
          makeParentView('foo', [
            makeView('bar', {
              position: 'relative',
            }),
          ]),
          [targetElement],
        )

        const finalEditor = dragByPixels(initialEditor, canvasPoint({ x: 15, y: 15 }))
        expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
          makeTestProjectCodeWithSnippet(
            makeParentView('foo', [
              makeView('bar', {
                position: 'relative',
                left: 15,
                top: 15,
              }),
            ]),
          ),
        )
      })
    })

    describe('when the element has offsets', () => {
      it('updates the offsets', async () => {
        const targetElement = elementPath([
          ['scene-foo', 'app-entity'],
          ['foo', 'bar'],
        ])

        const initialEditor = prepareEditorState(
          makeParentView('foo', [
            makeView('bar', {
              position: 'relative',
              left: 10,
              top: 10,
            }),
          ]),
          [targetElement],
        )

        const finalEditor = dragByPixels(initialEditor, canvasPoint({ x: 15, y: 15 }))
        expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
          makeTestProjectCodeWithSnippet(
            makeParentView('foo', [
              makeView('bar', {
                position: 'relative',
                left: 25,
                top: 25,
              }),
            ]),
          ),
        )
      })

      describe('when vertical or horizontal offsets are missing', () => {
        it('sets the missing offset', async () => {
          const targetElement = elementPath([
            ['scene-foo', 'app-entity'],
            ['foo', 'bar'],
          ])

          const initialEditor = prepareEditorState(
            makeParentView('foo', [
              makeView('bar', {
                position: 'relative',
                top: 10,
              }),
            ]),
            [targetElement],
          )

          const finalEditor = dragByPixels(initialEditor, canvasPoint({ x: 15, y: 15 }))
          expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
            makeTestProjectCodeWithSnippet(
              makeParentView('foo', [
                makeView('bar', {
                  position: 'relative',
                  top: 25,
                  left: 15,
                }),
              ]),
            ),
          )
        })
      })

      describe('tlbr behavior', () => {
        describe('honoring explicitly defined properties', () => {
          it('right', async () => {
            const targetElement = elementPath([
              ['scene-foo', 'app-entity'],
              ['foo', 'bar'],
            ])

            const initialEditor = prepareEditorState(
              makeParentView('foo', [
                makeView('bar', {
                  position: 'relative',
                  top: 100,
                  right: 25,
                }),
              ]),
              [targetElement],
            )

            const finalEditor = dragByPixels(initialEditor, canvasPoint({ x: 15, y: 15 }))
            expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
              makeTestProjectCodeWithSnippet(
                makeParentView('foo', [
                  makeView('bar', {
                    position: 'relative',
                    top: 115,
                    right: 10,
                  }),
                ]),
              ),
            )
          })

          it('bottom', async () => {
            const targetElement = elementPath([
              ['scene-foo', 'app-entity'],
              ['foo', 'bar'],
            ])

            const initialEditor = prepareEditorState(
              makeParentView('foo', [
                makeView('bar', {
                  position: 'relative',
                  bottom: 10,
                  left: 25,
                }),
              ]),
              [targetElement],
            )

            const finalEditor = dragByPixels(initialEditor, canvasPoint({ x: 15, y: 15 }))
            expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
              makeTestProjectCodeWithSnippet(
                makeParentView('foo', [
                  makeView('bar', {
                    position: 'relative',
                    bottom: -5,
                    left: 40,
                  }),
                ]),
              ),
            )
          })

          it('mixed', async () => {
            const targetElement = elementPath([
              ['scene-foo', 'app-entity'],
              ['foo', 'bar'],
            ])

            const initialEditor = prepareEditorState(
              makeParentView('foo', [
                makeView('bar', {
                  position: 'relative',
                  bottom: 10,
                  left: 25,
                  top: 10,
                }),
              ]),
              [targetElement],
            )

            const finalEditor = dragByPixels(initialEditor, canvasPoint({ x: 15, y: 15 }))
            expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
              makeTestProjectCodeWithSnippet(
                makeParentView('foo', [
                  makeView('bar', {
                    position: 'relative',
                    bottom: -5,
                    left: 40,
                    top: 25,
                  }),
                ]),
              ),
            )
          })
        })
      })
    })
  })

  describe('when the element position is absolute', () => {
    it('does not change it', async () => {
      const targetElement = elementPath([
        ['scene-foo', 'app-entity'],
        ['foo', 'bar'],
      ])

      const initialEditor = prepareEditorState(
        makeParentView('foo', [
          makeView('bar', {
            position: 'absolute',
          }),
        ]),
        [targetElement],
      )

      const finalEditor = dragByPixels(initialEditor, canvasPoint({ x: 15, y: 15 }))
      expect(testPrintCodeFromEditorState(finalEditor)).toEqual(
        makeTestProjectCodeWithSnippet(
          makeParentView('foo', [
            makeView('bar', {
              position: 'absolute',
              left: 15,
              top: 15,
            }),
          ]),
        ),
      )
    })
  })
})
