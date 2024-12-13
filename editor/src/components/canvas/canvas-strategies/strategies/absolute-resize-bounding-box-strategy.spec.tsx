import { left, right } from '../../../../core/shared/either'
import { elementPath, fromString } from '../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  SpecialSizeMeasurements,
} from '../../../../core/shared/element-template'
import { jsxElement, jsxElementName } from '../../../../core/shared/element-template'
import type { CanvasPoint } from '../../../../core/shared/math-utils'
import { canvasPoint, canvasRectangle } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { Modifiers } from '../../../../utils/modifiers'
import {
  altModifier,
  altShiftModifier,
  emptyModifiers,
  shiftModifier,
} from '../../../../utils/modifiers'
import type { AllElementProps, EditorState } from '../../../editor/store/editor-state'
import type { EdgePosition } from '../../canvas-types'
import { foldAndApplyCommands } from '../../commands/commands'
import {
  getEditorStateWithSelectedViews,
  makeTestProjectCodeWithSnippet,
  testPrintCodeFromEditorState,
} from '../../ui-jsx.test-utils'
import { pickCanvasStateFromEditorStateWithMetadata } from '../canvas-strategies'
import { createMouseInteractionForTests } from '../interaction-state.test-utils'
import { absoluteResizeBoundingBoxStrategy } from './absolute-resize-bounding-box-strategy'
import { createBuiltInDependenciesList } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'

function multiselectResizeElements(
  snippet: string,
  targetElements: Array<ElementPath>,
  edgePosition: EdgePosition,
  drag: CanvasPoint,
  modifiers: Modifiers,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): EditorState {
  const initialEditor = getEditorStateWithSelectedViews(
    makeTestProjectCodeWithSnippet(snippet),
    targetElements,
  )

  const interactionSessionWithoutMetadata = createMouseInteractionForTests(
    null as any, // the strategy does not use this
    modifiers,
    { type: 'RESIZE_HANDLE', edgePosition: edgePosition },
    drag,
  )

  const strategyResult = absoluteResizeBoundingBoxStrategy(
    pickCanvasStateFromEditorStateWithMetadata(
      initialEditor.selectedViews,
      initialEditor,
      createBuiltInDependenciesList(null),
      metadata,
      allElementProps,
    ),
    {
      ...interactionSessionWithoutMetadata,
      latestMetadata: {},
      latestAllElementProps: {},
      latestElementPathTree: {},
      latestVariablesInScope: {},
    },
  )!.apply('end-interaction')

  expect(strategyResult.customStatePatch).toEqual({})
  expect(strategyResult.status).toEqual('success')

  return foldAndApplyCommands(
    initialEditor,
    initialEditor,
    [],
    strategyResult.commands,
    'end-interaction',
  ).editorState
}

const testMetadata: ElementInstanceMetadataMap = {
  'scene-aaa/app-entity:aaa/bbb': {
    elementPath: fromString('scene-aaa/app-entity:aaa/bbb'),
    element: right(jsxElement(jsxElementName('div', []), 'bbb', [], [])),
    specialSizeMeasurements: {
      position: 'absolute',
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
    globalFrame: { height: 120, width: 100, x: 30, y: 50 },
  } as ElementInstanceMetadata,
  'scene-aaa/app-entity:aaa/ccc': {
    elementPath: fromString('scene-aaa/app-entity:aaa/ccc'),
    element: right(jsxElement(jsxElementName('div', []), 'bbb', [], [])),
    specialSizeMeasurements: {
      position: 'absolute',
      immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
      coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    } as SpecialSizeMeasurements,
    globalFrame: { height: 110, width: 100, x: 90, y: 40 },
  } as ElementInstanceMetadata,
}

const testAllElementProps: AllElementProps = {
  'scene-aaa/app-entity:aaa/bbb': {
    style: { width: 100, height: 80 },
  },
  'scene-aaa/app-entity:aaa/ccc': {
    style: { width: 100, height: 80 },
  },
}

describe('Absolute Resize Bounding Box Strategy single select', () => {
  it.each([
    [
      'top left corner, drag threshold not reached',
      {
        edgePosition: { x: 0, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 1,
          y: 1,
        }),
        modifiers: emptyModifiers,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 50, top: 50, width: 250, height: 300 },
      },
    ],
    [
      'top left corner',
      {
        edgePosition: { x: 0, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: emptyModifiers,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 65, top: 75, width: 235, height: 275 },
      },
    ],
    [
      'top left corner, flipping over',
      {
        edgePosition: { x: 0, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 350,
          y: 400,
        }),
        modifiers: emptyModifiers,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 300, top: 350, width: 100, height: 100 },
      },
    ],
    [
      'bottom left corner',
      {
        edgePosition: { x: 0, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: emptyModifiers,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 65, top: 50, width: 235, height: 325 },
      },
    ],
    [
      'top right corner',
      {
        edgePosition: { x: 1, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: emptyModifiers,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 50, top: 75, width: 265, height: 275 },
      },
    ],
    [
      'bottom right corner',
      {
        edgePosition: { x: 1, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: emptyModifiers,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 50, top: 50, width: 265, height: 325 },
      },
    ],
    [
      'top side',
      {
        edgePosition: { x: 0.5, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: emptyModifiers,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 50, top: 75, width: 250, height: 275 },
      },
    ],
    [
      'left side',
      {
        edgePosition: { x: 0, y: 0.5 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: emptyModifiers,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 65, top: 50, width: 235, height: 300 },
      },
    ],
    [
      'bottom side',
      {
        edgePosition: { x: 0.5, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: emptyModifiers,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 50, top: 50, width: 250, height: 325 },
      },
    ],
    [
      'right side',
      {
        edgePosition: { x: 1, y: 0.5 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: emptyModifiers,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 50, top: 50, width: 265, height: 300 },
      },
    ],
    [
      'top left corner, center mode',
      {
        edgePosition: { x: 0, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 65, top: 75, width: 220, height: 250 },
      },
    ],
    [
      'bottom left corner, center mode',
      {
        edgePosition: { x: 0, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 65, top: 25, width: 220, height: 350 },
      },
    ],
    [
      'top right corner, center mode',
      {
        edgePosition: { x: 1, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 35, top: 75, width: 280, height: 250 },
      },
    ],
    [
      'bottom right corner, center mode',
      {
        edgePosition: { x: 1, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 35, top: 25, width: 280, height: 350 },
      },
    ],
    [
      'top side, center mode',
      {
        edgePosition: { x: 0.5, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 50, top: 75, width: 250, height: 250 },
      },
    ],
    [
      'left side, center mode',
      {
        edgePosition: { x: 0, y: 0.5 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 65, top: 50, width: 220, height: 300 },
      },
    ],
    [
      'bottom side, center mode',
      {
        edgePosition: { x: 0.5, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 50, top: 25, width: 250, height: 350 },
      },
    ],
    [
      'right side, center mode',
      {
        edgePosition: { x: 1, y: 0.5 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 35, top: 50, width: 280, height: 300 },
      },
    ],
    [
      'top left corner, aspect ratio locked',
      {
        edgePosition: { x: 0, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: shiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 65, top: 68, width: 235, height: 282 },
      },
    ],
    [
      'top left corner, aspect ratio locked, flipping over',
      {
        edgePosition: { x: 0, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 350,
          y: 400,
        }),
        modifiers: shiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 300, top: 350, width: 100, height: 120 },
      },
    ],
    [
      'bottom left corner, aspect ratio locked',
      {
        edgePosition: { x: 0, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: shiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 29, top: 50, width: 271, height: 325 },
      },
    ],
    [
      'top right corner, aspect ratio locked',
      {
        edgePosition: { x: 1, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: shiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 50, top: 32, width: 265, height: 318 },
      },
    ],
    [
      'bottom right corner, aspect ratio locked',
      {
        edgePosition: { x: 1, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: shiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 50, top: 50, width: 271, height: 325 },
      },
    ],
    [
      'top side, aspect ratio locked',
      {
        edgePosition: { x: 0.5, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: shiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 60, top: 75, width: 229, height: 275 },
      },
    ],
    [
      'left side, aspect ratio locked',
      {
        edgePosition: { x: 0, y: 0.5 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: shiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 65, top: 59, width: 235, height: 282 },
      },
    ],
    [
      'bottom side, aspect ratio locked',
      {
        edgePosition: { x: 0.5, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: shiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 40, top: 50, width: 271, height: 325 },
      },
    ],
    [
      'right side, aspect ratio locked',
      {
        edgePosition: { x: 1, y: 0.5 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: shiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 50, top: 41, width: 265, height: 318 },
      },
    ],
    [
      'top left corner, center mode, aspect ratio locked',
      {
        edgePosition: { x: 0, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altShiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 65, top: 68, width: 220, height: 264 },
      },
    ],
    [
      'bottom left corner, center mode, aspect ratio locked',
      {
        edgePosition: { x: 0, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altShiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 29, top: 25, width: 292, height: 350 },
      },
    ],
    [
      'top right corner, center mode, aspect ratio locked',
      {
        edgePosition: { x: 1, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altShiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 35, top: 32, width: 280, height: 336 },
      },
    ],
    [
      'bottom right corner, center mode, aspect ratio locked',
      {
        edgePosition: { x: 1, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altShiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 29, top: 25, width: 292, height: 350 },
      },
    ],
    [
      'top side, center mode, aspect ratio locked',
      {
        edgePosition: { x: 0.5, y: 0 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altShiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 71, top: 75, width: 208, height: 250 },
      },
    ],
    [
      'left side, center mode, aspect ratio locked',
      {
        edgePosition: { x: 0, y: 0.5 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altShiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 65, top: 68, width: 220, height: 264 },
      },
    ],
    [
      'bottom side, center mode, aspect ratio locked',
      {
        edgePosition: { x: 0.5, y: 1 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altShiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 29, top: 25, width: 292, height: 350 },
      },
    ],
    [
      'right side, center mode, aspect ratio locked',
      {
        edgePosition: { x: 1, y: 0.5 } as EdgePosition,
        drag: canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers: altShiftModifier,
        bounding: { left: 50, top: 50, width: 250, height: 300 },
        expectedBounding: { left: 35, top: 32, width: 280, height: 336 },
      },
    ],
  ])(
    `Single select absolute resize: %s`,
    async (_, { edgePosition, drag, modifiers, bounding, expectedBounding }) => {
      const snippet = `
        <View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${bounding.left}, top: ${bounding.top}, width: ${bounding.width}, height: ${bounding.height} }}
            data-uid='bbb'
          />
        </View>
      `
      const selectedElements = [
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
      ]
      const editorAfterStrategy = multiselectResizeElements(
        snippet,
        selectedElements,
        edgePosition,
        drag,
        modifiers,
        {
          'scene-aaa/app-entity:aaa/bbb': {
            elementPath: fromString('scene-aaa/app-entity:aaa/bbb'),
            element: right(jsxElement(jsxElementName('div', []), 'bbb', [], [])),
            specialSizeMeasurements: {
              position: 'absolute',
              immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
              coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
            } as SpecialSizeMeasurements,
            globalFrame: {
              height: bounding.height,
              width: bounding.width,
              x: bounding.left,
              y: bounding.top,
            },
          } as ElementInstanceMetadata,
        },
        {
          'scene-aaa/app-entity:aaa/bbb': {
            style: {
              height: bounding.height,
              width: bounding.width,
              x: bounding.left,
              y: bounding.top,
            },
          },
        },
      )
      expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
        makeTestProjectCodeWithSnippet(`
        <View style={{ ...(props.style || {}) }} data-uid='aaa'>
          <View
            style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: ${expectedBounding.left}, top: ${expectedBounding.top}, width: ${expectedBounding.width}, height: ${expectedBounding.height} }}
            data-uid='bbb'
          />
        </View>
      `),
      )
    },
  )
  describe('Absolute Resize Bounding Box Strategy multi select', () => {
    it('works with element resized from TL corner', async () => {
      const edgePosition: EdgePosition = { x: 0, y: 0 }
      const snippet = `
    <div style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{ 
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 50,
          left: 30,
          width: 100,
          height: 120,
         }}
        data-uid='bbb'
      />
      <div
        style={{ 
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 40,
          left: 90,
          bottom: 250,
          right: 210
         }}
        data-uid='ccc'
      />
    </div>
    `
      const selectedElements = [
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
      ]
      const editorAfterStrategy = multiselectResizeElements(
        snippet,
        selectedElements,
        edgePosition,
        canvasPoint({
          x: 15,
          y: 25,
        }),
        emptyModifiers,
        testMetadata,
        testAllElementProps,
      )
      expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
            <div
              style={{ 
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 73,
                left: 45,
                width: 91,
                height: 97,
              }}
              data-uid='bbb'
            />
            <div
              style={{ 
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 65,
                left: 99,
                bottom: 246,
                right: 210
              }}
              data-uid='ccc'
            />
          </div>`,
        ),
      )
    })
    it('works with element aspect ratio locked resized from TL corner', async () => {
      const edgePosition: EdgePosition = { x: 0, y: 0 }
      const snippet = `
    <div style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 50,
          left: 30,
          width: 100,
          height: 120,
         }}
        data-uid='bbb'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 40,
          left: 90,
          bottom: 250,
          right: 210
         }}
        data-uid='ccc'
      />
    </div>
    `
      const selectedElements = [
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
      ]
      const editorAfterStrategy = multiselectResizeElements(
        snippet,
        selectedElements,
        edgePosition,
        canvasPoint({
          x: 15,
          y: 25,
        }),
        shiftModifier,
        testMetadata,
        testAllElementProps,
      )
      expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 61,
                left: 45,
                width: 91,
                height: 109,
              }}
              data-uid='bbb'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 52,
                left: 99,
                bottom: 248,
                right: 210
              }}
              data-uid='ccc'
            />
          </div>`,
        ),
      )
    })
    it('works with element resized from BR corner', async () => {
      const edgePosition: EdgePosition = { x: 1, y: 1 }
      const snippet = `
    <div style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 50,
          left: 30,
          width: 100,
          height: 120,
         }}
        data-uid='bbb'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 40,
          left: 90,
          bottom: 250,
          right: 210
         }}
        data-uid='ccc'
      />
    </div>
    `
      const selectedElements = [
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
      ]
      const editorAfterStrategy = multiselectResizeElements(
        snippet,
        selectedElements,
        edgePosition,
        canvasPoint({
          x: 15,
          y: 25,
        }),
        emptyModifiers,
        testMetadata,
        testAllElementProps,
      )
      expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 52,
                left: 30,
                width: 109,
                height: 143,
              }}
              data-uid='bbb'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 40,
                left: 96,
                bottom: 229,
                right: 195
              }}
              data-uid='ccc'
            />
          </div>`,
        ),
      )
    })
    it('works with element aspect ratio locked resized from BR corner', async () => {
      const edgePosition: EdgePosition = { x: 1, y: 1 }
      const snippet = `
    <div style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 50,
          left: 30,
          width: 100,
          height: 120,
         }}
        data-uid='bbb'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 40,
          left: 90,
          bottom: 250,
          right: 210
         }}
        data-uid='ccc'
      />
    </div>
    `
      const selectedElements = [
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
      ]
      const editorAfterStrategy = multiselectResizeElements(
        snippet,
        selectedElements,
        edgePosition,
        canvasPoint({
          x: 15,
          y: 25,
        }),
        shiftModifier,
        testMetadata,
        testAllElementProps,
      )
      expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 52,
                left: 30,
                width: 119,
                height: 143,
              }}
              data-uid='bbb'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 40,
                left: 102,
                bottom: 229,
                right: 179
              }}
              data-uid='ccc'
            />
          </div>`,
        ),
      )
    })
    it('works with element resized from RIGHT edge', async () => {
      const edgePosition: EdgePosition = { x: 1, y: 0.5 }
      const snippet = `
    <div style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 50,
          left: 30,
          width: 100,
          height: 120,
         }}
        data-uid='bbb'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 40,
          left: 90,
          bottom: 250,
          right: 210
         }}
        data-uid='ccc'
      />
    </div>
    `
      const selectedElements = [
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
      ]
      const editorAfterStrategy = multiselectResizeElements(
        snippet,
        selectedElements,
        edgePosition,
        canvasPoint({
          x: 15,
          y: 25,
        }),
        emptyModifiers,
        testMetadata,
        testAllElementProps,
      )
      expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 50,
                left: 30,
                width: 109,
                height: 120,
              }}
              data-uid='bbb'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 40,
                left: 96,
                bottom: 250,
                right: 195
              }}
              data-uid='ccc'
            />
          </div>`,
        ),
      )
    })
    it('works with element aspect ratio locked resized from RIGHT edge', async () => {
      const edgePosition: EdgePosition = { x: 1, y: 0.5 }
      const snippet = `
    <div style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 50,
          left: 30,
          width: 100,
          height: 120,
         }}
        data-uid='bbb'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 40,
          left: 90,
          bottom: 250,
          right: 210
         }}
        data-uid='ccc'
      />
    </div>
    `
      const selectedElements = [
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
      ]
      const editorAfterStrategy = multiselectResizeElements(
        snippet,
        selectedElements,
        edgePosition,
        canvasPoint({
          x: 15,
          y: 25,
        }),
        shiftModifier,
        testMetadata,
        testAllElementProps,
      )
      expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 45,
                left: 30,
                width: 109,
                height: 131,
              }}
              data-uid='bbb'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 34,
                left: 96,
                bottom: 246,
                right: 195
              }}
              data-uid='ccc'
            />
          </div>`,
        ),
      )
    })
    it('works with element resized from TOP edge', async () => {
      const edgePosition: EdgePosition = { x: 0.5, y: 0 }
      const snippet = `
    <div style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 50,
          left: 30,
          width: 100,
          height: 120,
         }}
        data-uid='bbb'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 40,
          left: 90,
          bottom: 250,
          right: 210
         }}
        data-uid='ccc'
      />
    </div>
    `
      const selectedElements = [
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
      ]
      const editorAfterStrategy = multiselectResizeElements(
        snippet,
        selectedElements,
        edgePosition,
        canvasPoint({
          x: 15,
          y: 25,
        }),
        emptyModifiers,
        testMetadata,
        testAllElementProps,
      )
      expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 73,
                left: 30,
                width: 100,
                height: 97,
              }}
              data-uid='bbb'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 65,
                left: 90,
                bottom: 246,
                right: 210
              }}
              data-uid='ccc'
            />
          </div>`,
        ),
      )
    })
    it('works with element aspect ratio locked resized from TOP edge', async () => {
      const edgePosition: EdgePosition = { x: 0.5, y: 0 }
      const snippet = `
    <div style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 50,
          left: 30,
          width: 100,
          height: 120,
         }}
        data-uid='bbb'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 40,
          left: 90,
          bottom: 250,
          right: 210
         }}
        data-uid='ccc'
      />
    </div>
    `
      const selectedElements = [
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
      ]
      const editorAfterStrategy = multiselectResizeElements(
        snippet,
        selectedElements,
        edgePosition,
        canvasPoint({
          x: 15,
          y: 25,
        }),
        shiftModifier,
        testMetadata,
        testAllElementProps,
      )
      expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 73,
                left: 45,
                width: 81,
                height: 97,
              }}
              data-uid='bbb'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 65,
                left: 93,
                bottom: 246,
                right: 226
              }}
              data-uid='ccc'
            />
          </div>`,
        ),
      )
    })
  })

  describe('Center based resize strategy', () => {
    it('works with element resized from TL corner', async () => {
      const edgePosition: EdgePosition = { x: 0, y: 0 }
      const snippet = `
    <div style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 50,
          left: 30,
          width: 100,
          height: 120,
         }}
        data-uid='bbb'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 40,
          left: 90,
          bottom: 250,
          right: 210
         }}
        data-uid='ccc'
      />
    </div>
    `
      const selectedElements = [
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
      ]
      const modifiers: Modifiers = {
        alt: true,
        cmd: false,
        ctrl: false,
        shift: false,
      }
      const editorAfterStrategy = multiselectResizeElements(
        snippet,
        selectedElements,
        edgePosition,
        canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers,
        testMetadata,
        testAllElementProps,
      )
      expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 71,
                left: 45,
                width: 81,
                height: 74,
              }}
              data-uid='bbb'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 65,
                left: 94,
                bottom: 267,
                right: 225
              }}
              data-uid='ccc'
            />
          </div>`,
        ),
      )
    })
    it('works with element resized from Bottom edge', async () => {
      const edgePosition: EdgePosition = { x: 0.5, y: 1 }
      const snippet = `
    <div style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 50,
          left: 30,
          width: 100,
          height: 120,
         }}
        data-uid='bbb'
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          top: 40,
          left: 90,
          bottom: 250,
          right: 210
         }}
        data-uid='ccc'
      />
    </div>
    `
      const selectedElements = [
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        elementPath([
          ['scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
      ]
      const modifiers: Modifiers = {
        alt: true,
        cmd: false,
        ctrl: false,
        shift: false,
      }
      const editorAfterStrategy = multiselectResizeElements(
        snippet,
        selectedElements,
        edgePosition,
        canvasPoint({
          x: 15,
          y: 25,
        }),
        modifiers,
        testMetadata,
        testAllElementProps,
      )
      expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
        makeTestProjectCodeWithSnippet(
          `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 29,
                left: 30,
                width: 100,
                height: 166,
              }}
              data-uid='bbb'
            />
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                top: 15,
                left: 90,
                bottom: 233,
                right: 210
              }}
              data-uid='ccc'
            />
          </div>`,
        ),
      )
    })
  })
})

describe('Absolute Resize Strategy with missing props', () => {
  it('works with element (no height, no width) resized from Bottom Right Corner', async () => {
    const edgePosition: EdgePosition = { x: 1, y: 1 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        top: 50,
        left: 30,
        display: 'flex',
       }}
      data-uid='bbb'
    >
      <div data-uid='ccc' style={{width: 100, height: 80}} />
    </div>
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
    ]
    const editorAfterStrategy = multiselectResizeElements(
      snippet,
      selectedElements,
      edgePosition,
      canvasPoint({
        x: 15,
        y: 25,
      }),
      emptyModifiers,
      {
        'scene-aaa/app-entity:aaa/bbb': {
          elementPath: fromString('scene-aaa/app-entity:aaa/bbb'),
          element: right(jsxElement(jsxElementName('div', []), 'bbb', [], [])),
          specialSizeMeasurements: {
            position: 'absolute',
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
            coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          } as SpecialSizeMeasurements,
          globalFrame: { x: 30, y: 50, width: 100, height: 80 },
        } as ElementInstanceMetadata,
        'scene-aaa/app-entity:aaa/bbb/ccc': {
          elementPath: fromString('scene-aaa/app-entity:aaa/bbb/ccc'),
          element: right(jsxElement(jsxElementName('div', []), 'bbb', [], [])),
          specialSizeMeasurements: {
            position: 'absolute',
            immediateParentBounds: canvasRectangle({ x: 30, y: 50, width: 100, height: 80 }),
            coordinateSystemBounds: canvasRectangle({ x: 30, y: 50, width: 100, height: 80 }),
          } as SpecialSizeMeasurements,
          globalFrame: { x: 30, y: 50, width: 100, height: 80 },
        } as ElementInstanceMetadata,
      },
      {
        'scene-aaa/app-entity:aaa/bbb': {
          style: { width: 100, height: 80 },
        },
        'scene-aaa/app-entity:aaa/bbb/ccc': {
          style: { width: 100, height: 80 },
        },
      },
    )
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              top: 50,
              left: 30,
              display: 'flex',
              width: 115,
              height: 105,
            }}
            data-uid='bbb'
          >
            <div data-uid='ccc' style={{width: 100, height: 80}} />
          </div>
        </div>`,
      ),
    )
  })
  it('works with element (no height, no width) resized from Left Edge, only adds width', async () => {
    const edgePosition: EdgePosition = { x: 0, y: 0.5 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        top: 50,
        left: 30,
        display: 'flex',
       }}
      data-uid='bbb'
    >
      <div data-uid='ccc' style={{width: 100, height: 80}} />
    </div>
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
    ]
    const editorAfterStrategy = multiselectResizeElements(
      snippet,
      selectedElements,
      edgePosition,
      canvasPoint({
        x: 15,
        y: 25,
      }),
      emptyModifiers,
      {
        'scene-aaa/app-entity:aaa/bbb': {
          elementPath: fromString('scene-aaa/app-entity:aaa/bbb'),
          element: right(jsxElement(jsxElementName('div', []), 'bbb', [], [])),
          specialSizeMeasurements: {
            position: 'absolute',
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          } as SpecialSizeMeasurements,
          globalFrame: { x: 30, y: 50, width: 100, height: 80 },
        } as ElementInstanceMetadata,
        'scene-aaa/app-entity:aaa/bbb/ccc': {
          elementPath: fromString('scene-aaa/app-entity:aaa/bbb/ccc'),
          element: right(jsxElement(jsxElementName('div', []), 'bbb', [], [])),
          specialSizeMeasurements: {
            position: 'absolute',
            immediateParentBounds: canvasRectangle({ x: 30, y: 50, width: 100, height: 80 }),
          } as SpecialSizeMeasurements,
          globalFrame: { x: 30, y: 50, width: 100, height: 80 },
        } as ElementInstanceMetadata,
      },
      {
        'scene-aaa/app-entity:aaa/bbb': {
          style: { width: 100, height: 80 },
        },
        'scene-aaa/app-entity:aaa/bbb/ccc': {
          style: { width: 100, height: 80 },
        },
      },
    )
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              top: 50,
              left: 45,
              display: 'flex',
              width: 85,
            }}
            data-uid='bbb'
          >
            <div data-uid='ccc' style={{width: 100, height: 80}} />
          </div>
        </div>`,
      ),
    )
  })
  it('works with element without props resized from Bottom Edge, only adds height', async () => {
    const edgePosition: EdgePosition = { x: 0.5, y: 1 }
    const snippet = `
  <div style={{ ...(props.style || {}) }} data-uid='aaa'>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        display: 'flex',
       }}
      data-uid='bbb'
    >
      <div data-uid='ccc' style={{width: 100, height: 80}} />
    </div>
  </div>
  `
    const selectedElements = [
      elementPath([
        ['scene-aaa', 'app-entity'],
        ['aaa', 'bbb'],
      ]),
    ]
    const editorAfterStrategy = multiselectResizeElements(
      snippet,
      selectedElements,
      edgePosition,
      canvasPoint({
        x: 15,
        y: 25,
      }),
      emptyModifiers,
      {
        'scene-aaa/app-entity:aaa/bbb': {
          elementPath: fromString('scene-aaa/app-entity:aaa/bbb'),
          element: right(jsxElement(jsxElementName('div', []), 'bbb', [], [])),
          specialSizeMeasurements: {
            position: 'absolute',
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
            coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          } as SpecialSizeMeasurements,
          globalFrame: { x: 0, y: 0, width: 100, height: 80 },
        } as ElementInstanceMetadata,
        'scene-aaa/app-entity:aaa/bbb/ccc': {
          elementPath: fromString('scene-aaa/app-entity:aaa/bbb/ccc'),
          element: right(jsxElement(jsxElementName('div', []), 'bbb', [], [])),
          specialSizeMeasurements: {
            position: 'absolute',
            immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 100, height: 80 }),
            coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 100, height: 80 }),
          } as SpecialSizeMeasurements,
          globalFrame: { x: 0, y: 0, width: 100, height: 80 },
        } as ElementInstanceMetadata,
      },
      {
        'scene-aaa/app-entity:aaa/bbb': {
          style: { width: 100, height: 80 },
        },
        'scene-aaa/app-entity:aaa/bbb/ccc': {
          style: { width: 100, height: 80 },
        },
      },
    )
    expect(testPrintCodeFromEditorState(editorAfterStrategy)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<div style={{ ...(props.style || {}) }} data-uid='aaa'>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              display: 'flex',
              height: 105,
            }}
            data-uid='bbb'
          >
            <div data-uid='ccc' style={{width: 100, height: 80}} />
          </div>
        </div>`,
      ),
    )
  })
})
