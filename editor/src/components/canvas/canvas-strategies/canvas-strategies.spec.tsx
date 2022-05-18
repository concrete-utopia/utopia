import { elementPath } from '../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  SpecialSizeMeasurements,
} from '../../../core/shared/element-template'
import { canvasPoint, canvasRectangle, localRectangle } from '../../../core/shared/math-utils'
import { EditorState } from '../../editor/store/editor-state'
import { emptyModifiers } from '../../../utils/modifiers'
import {
  findCanvasStrategy,
  pickCanvasStateFromEditorState,
  RegisteredCanvasStrategies,
} from './canvas-strategies'
import { CustomStrategyState } from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { createMouseInteractionForTests } from './interaction-state.test-utils'
import { getEditorState, makeTestProjectCodeWithSnippet } from '../ui-jsx.test-utils'
import { ElementPath } from '../../../core/shared/project-file-types'

function prepareEditorState(codeSnippet: string, selectedViews: Array<ElementPath>): EditorState {
  return {
    ...getEditorState(makeTestProjectCodeWithSnippet(codeSnippet)),
    selectedViews: selectedViews,
  }
}

const baseStrategyState = (
  metadata: ElementInstanceMetadataMap,
  customState: CustomStrategyState = {
    escapeHatchActivated: true,
  },
) =>
  ({
    currentStrategy: null as any, // the strategy does not use this
    currentStrategyFitness: null as any, // the strategy does not use this
    currentStrategyCommands: null as any, // the strategy does not use this
    accumulatedPatches: null as any, // the strategy does not use this
    commandDescriptions: null as any, // the strategy does not use this
    sortedApplicableStrategies: null as any, // the strategy does not use this
    startingMetadata: metadata,
    customStrategyState: customState,
  } as StrategyState)

describe('Strategy Fitness', () => {
  it('fits Escape Hatch Strategy when dragging a flow element', async () => {
    const targetElement = elementPath([
      ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <div style={{ ...(props.style || {}) }} data-uid='aaa'>
      <div
        style={{ backgroundColor: '#0091FFAA', width: 250, height: 300 }}
        data-uid='bbb'
      />
    </div>
    `,
      [targetElement],
    )

    // This is a metadata copied from the editor, please update with real values.
    const metadata = {
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa:': {
        attributeMetadatada: null,
        componentInstance: false,
        computedStyle: null,
        element: {
          type: 'RIGHT',
          value: {
            children: [
              {
                children: [],
                name: {
                  baseVariable: 'div',
                  propertyPath: { propertyElements: [] },
                },
                props: [],
                type: 'JSX_ELEMENT',
                uid: 'bbb',
              },
            ],
            name: {
              baseVariable: 'div',
              propertyPath: { propertyElements: [] },
            },
            props: [],
            type: 'JSX_ELEMENT',
            uid: 'aaa',
          },
        },
        elementPath: elementPath([['utopia-storyboard-uid', 'scene-aaa', 'app-entity'], ['aaa']]),
        isEmotionOrStyledComponent: false,
        label: null,
        localFrame: localRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        globalFrame: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        importInfo: {
          type: 'LEFT',
          value: 'NOT_IMPORTED',
        },
        props: { style: { position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 } },
        specialSizeMeasurements: {
          clientHeight: 400,
          clientWidth: 400,
          immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          globalContentBox: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          display: 'block',
          flexDirection: 'row',
          htmlElementName: 'div',
          immediateParentProvidesLayout: true,
          layoutSystemForChildren: 'flow',
          margin: { top: 0, right: 0, bottom: 0, left: 0 },
          naturalHeight: null,
          naturalWidth: null,
          offset: { x: 0, y: 0 },
          padding: { top: 0, right: 0, bottom: 0, left: 0 },
          parentFlexDirection: 'row',
          parentLayoutSystem: 'flow',
          position: 'absolute',
          providesBoundsForChildren: true,
          renderedChildrenCount: 1,
          usesParentBounds: true,
        } as SpecialSizeMeasurements,
      } as ElementInstanceMetadata,
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb': {
        attributeMetadatada: null,
        componentInstance: false,
        computedStyle: null,
        element: {
          type: 'RIGHT',
          value: {
            children: [],
            name: {
              baseVariable: 'div',
              propertyPath: { propertyElements: [] },
            },
            props: [],
            type: 'JSX_ELEMENT',
            uid: 'bbb',
          },
        },
        elementPath: elementPath([
          ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        localFrame: localRectangle({ x: 0, y: 0, width: 250, height: 300 }),
        globalFrame: canvasRectangle({ x: 0, y: 0, width: 250, height: 300 }),
        isEmotionOrStyledComponent: false,
        label: null,
        props: {
          style: {
            backgroundColor: '#0091FFAA',
            height: 300,
            width: 250,
          },
        },
        importInfo: {
          type: 'LEFT',
          value: 'NOT_IMPORTED',
        },
        specialSizeMeasurements: {
          clientHeight: 300,
          clientWidth: 250,
          immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          globalContentBox: canvasRectangle({ x: 0, y: 0, width: 250, height: 300 }),
          position: 'static',
          display: 'block',
          flexDirection: 'row',
          htmlElementName: 'div',
          immediateParentProvidesLayout: true,
          layoutSystemForChildren: 'flow',
          margin: { top: 0, right: 0, bottom: 0, left: 0 },
          naturalHeight: null,
          naturalWidth: null,
          offset: { x: 0, y: 0 },
          padding: { top: 0, right: 0, bottom: 0, left: 0 },
          parentFlexDirection: 'row',
          parentLayoutSystem: 'flow',
          providesBoundsForChildren: false,
          renderedChildrenCount: 0,
          usesParentBounds: true,
        } as SpecialSizeMeasurements,
      } as ElementInstanceMetadata,
    }

    const interactionSession: InteractionSession = {
      ...createMouseInteractionForTests(
        canvasPoint({ x: 0, y: 0 }),
        emptyModifiers,
        { type: 'BOUNDING_AREA', target: targetElement },
        canvasPoint({ x: 15, y: 15 }),
      ),
      metadata: null as any, // the strategy does not use this
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(initialEditor),
      interactionSession,
      baseStrategyState(metadata, { escapeHatchActivated: false }),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('ESCAPE_HATCH_STRATEGY')
  })
  it('fits Escape Hatch Strategy when dragging a flex element without siblings', async () => {
    const targetElement = elementPath([
      ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <div style={{ ...(props.style || {}), display: 'flex' }} data-uid='aaa'>
      <div
        style={{ backgroundColor: '#0091FFAA', width: 250, height: 300 }}
        data-uid='bbb'
      />
    </div>
    `,
      [targetElement],
    )

    // This is a metadata copied from the editor, please update with real values.
    const metadata = {
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa:': {
        attributeMetadatada: null,
        componentInstance: false,
        computedStyle: null,
        element: {
          type: 'RIGHT',
          value: {
            children: [
              {
                children: [],
                name: {
                  baseVariable: 'div',
                  propertyPath: { propertyElements: [] },
                },
                props: [],
                type: 'JSX_ELEMENT',
                uid: 'bbb',
              },
            ],
            name: {
              baseVariable: 'div',
              propertyPath: { propertyElements: [] },
            },
            props: [],
            type: 'JSX_ELEMENT',
            uid: 'aaa',
          },
        },
        elementPath: elementPath([['utopia-storyboard-uid', 'scene-aaa', 'app-entity'], ['aaa']]),
        isEmotionOrStyledComponent: false,
        label: null,
        localFrame: localRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        globalFrame: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        importInfo: {
          type: 'LEFT',
          value: 'NOT_IMPORTED',
        },
        props: { style: { position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 } },
        specialSizeMeasurements: {
          clientHeight: 400,
          clientWidth: 400,
          immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          globalContentBox: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          display: 'block',
          flexDirection: 'row',
          htmlElementName: 'div',
          immediateParentProvidesLayout: true,
          layoutSystemForChildren: 'flex',
          margin: { top: 0, right: 0, bottom: 0, left: 0 },
          naturalHeight: null,
          naturalWidth: null,
          offset: { x: 0, y: 0 },
          padding: { top: 0, right: 0, bottom: 0, left: 0 },
          parentFlexDirection: 'row',
          parentLayoutSystem: 'flow',
          position: 'absolute',
          providesBoundsForChildren: true,
          renderedChildrenCount: 1,
          usesParentBounds: true,
        } as SpecialSizeMeasurements,
      } as ElementInstanceMetadata,
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb': {
        attributeMetadatada: null,
        componentInstance: false,
        computedStyle: null,
        element: {
          type: 'RIGHT',
          value: {
            children: [],
            name: {
              baseVariable: 'div',
              propertyPath: { propertyElements: [] },
            },
            props: [],
            type: 'JSX_ELEMENT',
            uid: 'bbb',
          },
        },
        elementPath: elementPath([
          ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        localFrame: localRectangle({ x: 0, y: 0, width: 250, height: 300 }),
        globalFrame: canvasRectangle({ x: 0, y: 0, width: 250, height: 300 }),
        isEmotionOrStyledComponent: false,
        label: null,
        props: {
          style: {
            backgroundColor: '#0091FFAA',
            height: 300,
            width: 250,
          },
        },
        importInfo: {
          type: 'LEFT',
          value: 'NOT_IMPORTED',
        },
        specialSizeMeasurements: {
          clientHeight: 300,
          clientWidth: 250,
          immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          globalContentBox: canvasRectangle({ x: 0, y: 0, width: 250, height: 300 }),
          position: 'static',
          display: 'block',
          flexDirection: 'row',
          htmlElementName: 'div',
          immediateParentProvidesLayout: true,
          layoutSystemForChildren: 'flow',
          margin: { top: 0, right: 0, bottom: 0, left: 0 },
          naturalHeight: null,
          naturalWidth: null,
          offset: { x: 0, y: 0 },
          padding: { top: 0, right: 0, bottom: 0, left: 0 },
          parentFlexDirection: 'row',
          parentLayoutSystem: 'flex',
          providesBoundsForChildren: false,
          renderedChildrenCount: 0,
          usesParentBounds: true,
        } as SpecialSizeMeasurements,
      } as ElementInstanceMetadata,
    }

    const interactionSession: InteractionSession = {
      ...createMouseInteractionForTests(
        canvasPoint({ x: 0, y: 0 }),
        emptyModifiers,
        { type: 'BOUNDING_AREA', target: targetElement },
        canvasPoint({ x: 15, y: 15 }),
      ),
      metadata: null as any, // the strategy does not use this
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(initialEditor),
      interactionSession,
      baseStrategyState(metadata, { escapeHatchActivated: false }),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('ESCAPE_HATCH_STRATEGY')
  })
  it('fits Flex Reorder Strategy when dragging a flex element with siblings', async () => {
    const targetElement = elementPath([
      ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <div style={{ ...(props.style || {}), display: 'flex' }} data-uid='aaa'>
      <div
        style={{ backgroundColor: '#0091FFAA', width: 100, height: 200 }}
        data-uid='bbb'
      />
      <div
        style={{ backgroundColor: '#0091FFAA', width: 100, height: 50 }}
        data-uid='ccc'
      />
    </div>
    `,
      [targetElement],
    )

    // This is a metadata copied from the editor, please update with real values.
    const metadata = {
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa:': {
        attributeMetadatada: null,
        componentInstance: false,
        computedStyle: null,
        element: {
          type: 'RIGHT',
          value: {
            children: [
              {
                children: [],
                name: {
                  baseVariable: 'div',
                  propertyPath: { propertyElements: [] },
                },
                props: [],
                type: 'JSX_ELEMENT',
                uid: 'bbb',
              },
              {
                children: [],
                name: {
                  baseVariable: 'div',
                  propertyPath: { propertyElements: [] },
                },
                props: [],
                type: 'JSX_ELEMENT',
                uid: 'ccc',
              },
            ],
            name: {
              baseVariable: 'div',
              propertyPath: { propertyElements: [] },
            },
            props: [],
            type: 'JSX_ELEMENT',
            uid: 'aaa',
          },
        },
        elementPath: elementPath([['utopia-storyboard-uid', 'scene-aaa', 'app-entity'], ['aaa']]),
        isEmotionOrStyledComponent: false,
        label: null,
        localFrame: localRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        globalFrame: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        importInfo: {
          type: 'LEFT',
          value: 'NOT_IMPORTED',
        },
        props: { style: { position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 } },
        specialSizeMeasurements: {
          clientHeight: 400,
          clientWidth: 400,
          immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          globalContentBox: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          display: 'block',
          flexDirection: 'row',
          htmlElementName: 'div',
          immediateParentProvidesLayout: true,
          layoutSystemForChildren: 'flex',
          margin: { top: 0, right: 0, bottom: 0, left: 0 },
          naturalHeight: null,
          naturalWidth: null,
          offset: { x: 0, y: 0 },
          padding: { top: 0, right: 0, bottom: 0, left: 0 },
          parentFlexDirection: 'row',
          parentLayoutSystem: 'flow',
          position: 'absolute',
          providesBoundsForChildren: true,
          renderedChildrenCount: 1,
          usesParentBounds: true,
        } as SpecialSizeMeasurements,
      } as ElementInstanceMetadata,
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb': {
        attributeMetadatada: null,
        componentInstance: false,
        computedStyle: null,
        element: {
          type: 'RIGHT',
          value: {
            children: [],
            name: {
              baseVariable: 'div',
              propertyPath: { propertyElements: [] },
            },
            props: [],
            type: 'JSX_ELEMENT',
            uid: 'bbb',
          },
        },
        elementPath: elementPath([
          ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 200 }),
        globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 200 }),
        isEmotionOrStyledComponent: false,
        label: null,
        props: {
          style: {
            backgroundColor: '#0091FFAA',
            height: 200,
            width: 100,
          },
        },
        importInfo: {
          type: 'LEFT',
          value: 'NOT_IMPORTED',
        },
        specialSizeMeasurements: {
          clientHeight: 200,
          clientWidth: 100,
          immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          globalContentBox: canvasRectangle({ x: 0, y: 0, width: 100, height: 200 }),
          position: 'static',
          display: 'block',
          flexDirection: 'row',
          htmlElementName: 'div',
          immediateParentProvidesLayout: true,
          layoutSystemForChildren: 'flow',
          margin: { top: 0, right: 0, bottom: 0, left: 0 },
          naturalHeight: null,
          naturalWidth: null,
          offset: { x: 0, y: 0 },
          padding: { top: 0, right: 0, bottom: 0, left: 0 },
          parentFlexDirection: 'row',
          parentLayoutSystem: 'flex',
          providesBoundsForChildren: false,
          renderedChildrenCount: 0,
          usesParentBounds: true,
        } as SpecialSizeMeasurements,
      } as ElementInstanceMetadata,
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/ccc': {
        attributeMetadatada: null,
        componentInstance: false,
        computedStyle: null,
        element: {
          type: 'RIGHT',
          value: {
            children: [],
            name: {
              baseVariable: 'div',
              propertyPath: { propertyElements: [] },
            },
            props: [],
            type: 'JSX_ELEMENT',
            uid: 'ccc',
          },
        },
        elementPath: elementPath([
          ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
        localFrame: localRectangle({ x: 100, y: 0, width: 100, height: 50 }),
        globalFrame: canvasRectangle({ x: 100, y: 0, width: 100, height: 50 }),
        isEmotionOrStyledComponent: false,
        label: null,
        props: {
          style: {
            backgroundColor: '#0091FFAA',
            height: 50,
            width: 100,
          },
        },
        importInfo: {
          type: 'LEFT',
          value: 'NOT_IMPORTED',
        },
        specialSizeMeasurements: {
          clientHeight: 50,
          clientWidth: 100,
          immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          globalContentBox: canvasRectangle({ x: 100, y: 0, width: 100, height: 50 }),
          position: 'static',
          display: 'block',
          flexDirection: 'row',
          htmlElementName: 'div',
          immediateParentProvidesLayout: true,
          layoutSystemForChildren: 'flow',
          margin: { top: 0, right: 0, bottom: 0, left: 0 },
          naturalHeight: null,
          naturalWidth: null,
          offset: { x: 100, y: 0 },
          padding: { top: 0, right: 0, bottom: 0, left: 0 },
          parentFlexDirection: 'row',
          parentLayoutSystem: 'flex',
          providesBoundsForChildren: false,
          renderedChildrenCount: 0,
          usesParentBounds: true,
        } as SpecialSizeMeasurements,
      } as ElementInstanceMetadata,
    }

    const interactionSession: InteractionSession = {
      ...createMouseInteractionForTests(
        canvasPoint({ x: 0, y: 0 }),
        emptyModifiers,
        { type: 'BOUNDING_AREA', target: targetElement },
        canvasPoint({ x: 15, y: 15 }),
      ),
      metadata: null as any, // the strategy does not use this
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(initialEditor),
      interactionSession,
      baseStrategyState(metadata, { escapeHatchActivated: false }),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('FLEX_REORDER')
  })
  it('fits Escape Hatch Strategy when dragging a flex element with siblings the cursor is outside of the parent', async () => {
    const targetElement = elementPath([
      ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
      ['aaa', 'bbb'],
    ])

    const initialEditor: EditorState = prepareEditorState(
      `
    <div style={{ ...(props.style || {}), display: 'flex' }} data-uid='aaa'>
      <div
        style={{ backgroundColor: '#0091FFAA', width: 100, height: 200 }}
        data-uid='bbb'
      />
      <div
        style={{ backgroundColor: '#0091FFAA', width: 100, height: 50 }}
        data-uid='ccc'
      />
    </div>
    `,
      [targetElement],
    )

    // This is a metadata copied from the editor, please update with real values.
    const metadata = {
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa:': {
        attributeMetadatada: null,
        componentInstance: false,
        computedStyle: null,
        element: {
          type: 'RIGHT',
          value: {
            children: [
              {
                children: [],
                name: {
                  baseVariable: 'div',
                  propertyPath: { propertyElements: [] },
                },
                props: [],
                type: 'JSX_ELEMENT',
                uid: 'bbb',
              },
              {
                children: [],
                name: {
                  baseVariable: 'div',
                  propertyPath: { propertyElements: [] },
                },
                props: [],
                type: 'JSX_ELEMENT',
                uid: 'ccc',
              },
            ],
            name: {
              baseVariable: 'div',
              propertyPath: { propertyElements: [] },
            },
            props: [],
            type: 'JSX_ELEMENT',
            uid: 'aaa',
          },
        },
        elementPath: elementPath([['utopia-storyboard-uid', 'scene-aaa', 'app-entity'], ['aaa']]),
        isEmotionOrStyledComponent: false,
        label: null,
        localFrame: localRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        globalFrame: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
        importInfo: {
          type: 'LEFT',
          value: 'NOT_IMPORTED',
        },
        props: { style: { position: 'absolute', bottom: 0, left: 0, right: 0, top: 0 } },
        specialSizeMeasurements: {
          clientHeight: 400,
          clientWidth: 400,
          immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          globalContentBox: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          display: 'block',
          flexDirection: 'row',
          htmlElementName: 'div',
          immediateParentProvidesLayout: true,
          layoutSystemForChildren: 'flex',
          margin: { top: 0, right: 0, bottom: 0, left: 0 },
          naturalHeight: null,
          naturalWidth: null,
          offset: { x: 0, y: 0 },
          padding: { top: 0, right: 0, bottom: 0, left: 0 },
          parentFlexDirection: 'row',
          parentLayoutSystem: 'flow',
          position: 'absolute',
          providesBoundsForChildren: true,
          renderedChildrenCount: 1,
          usesParentBounds: true,
        } as SpecialSizeMeasurements,
      } as ElementInstanceMetadata,
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/bbb': {
        attributeMetadatada: null,
        componentInstance: false,
        computedStyle: null,
        element: {
          type: 'RIGHT',
          value: {
            children: [],
            name: {
              baseVariable: 'div',
              propertyPath: { propertyElements: [] },
            },
            props: [],
            type: 'JSX_ELEMENT',
            uid: 'bbb',
          },
        },
        elementPath: elementPath([
          ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
          ['aaa', 'bbb'],
        ]),
        localFrame: localRectangle({ x: 0, y: 0, width: 100, height: 200 }),
        globalFrame: canvasRectangle({ x: 0, y: 0, width: 100, height: 200 }),
        isEmotionOrStyledComponent: false,
        label: null,
        props: {
          style: {
            backgroundColor: '#0091FFAA',
            height: 200,
            width: 100,
          },
        },
        importInfo: {
          type: 'LEFT',
          value: 'NOT_IMPORTED',
        },
        specialSizeMeasurements: {
          clientHeight: 200,
          clientWidth: 100,
          immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          globalContentBox: canvasRectangle({ x: 0, y: 0, width: 100, height: 200 }),
          position: 'static',
          display: 'block',
          flexDirection: 'row',
          htmlElementName: 'div',
          immediateParentProvidesLayout: true,
          layoutSystemForChildren: 'flow',
          margin: { top: 0, right: 0, bottom: 0, left: 0 },
          naturalHeight: null,
          naturalWidth: null,
          offset: { x: 0, y: 0 },
          padding: { top: 0, right: 0, bottom: 0, left: 0 },
          parentFlexDirection: 'row',
          parentLayoutSystem: 'flex',
          providesBoundsForChildren: false,
          renderedChildrenCount: 0,
          usesParentBounds: true,
        } as SpecialSizeMeasurements,
      } as ElementInstanceMetadata,
      'utopia-storyboard-uid/scene-aaa/app-entity:aaa/ccc': {
        attributeMetadatada: null,
        componentInstance: false,
        computedStyle: null,
        element: {
          type: 'RIGHT',
          value: {
            children: [],
            name: {
              baseVariable: 'div',
              propertyPath: { propertyElements: [] },
            },
            props: [],
            type: 'JSX_ELEMENT',
            uid: 'ccc',
          },
        },
        elementPath: elementPath([
          ['utopia-storyboard-uid', 'scene-aaa', 'app-entity'],
          ['aaa', 'ccc'],
        ]),
        localFrame: localRectangle({ x: 100, y: 0, width: 100, height: 50 }),
        globalFrame: canvasRectangle({ x: 100, y: 0, width: 100, height: 50 }),
        isEmotionOrStyledComponent: false,
        label: null,
        props: {
          style: {
            backgroundColor: '#0091FFAA',
            height: 50,
            width: 100,
          },
        },
        importInfo: {
          type: 'LEFT',
          value: 'NOT_IMPORTED',
        },
        specialSizeMeasurements: {
          clientHeight: 50,
          clientWidth: 100,
          immediateParentBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          coordinateSystemBounds: canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
          globalContentBox: canvasRectangle({ x: 100, y: 0, width: 100, height: 50 }),
          position: 'static',
          display: 'block',
          flexDirection: 'row',
          htmlElementName: 'div',
          immediateParentProvidesLayout: true,
          layoutSystemForChildren: 'flow',
          margin: { top: 0, right: 0, bottom: 0, left: 0 },
          naturalHeight: null,
          naturalWidth: null,
          offset: { x: 100, y: 0 },
          padding: { top: 0, right: 0, bottom: 0, left: 0 },
          parentFlexDirection: 'row',
          parentLayoutSystem: 'flex',
          providesBoundsForChildren: false,
          renderedChildrenCount: 0,
          usesParentBounds: true,
        } as SpecialSizeMeasurements,
      } as ElementInstanceMetadata,
    }

    const interactionSession: InteractionSession = {
      ...createMouseInteractionForTests(
        canvasPoint({ x: 0, y: 0 }),
        emptyModifiers,
        { type: 'BOUNDING_AREA', target: targetElement },
        canvasPoint({ x: -15, y: -15 }),
      ),
      metadata: null as any, // the strategy does not use this
    }

    const canvasStrategy = findCanvasStrategy(
      RegisteredCanvasStrategies,
      pickCanvasStateFromEditorState(initialEditor),
      interactionSession,
      baseStrategyState(metadata, { escapeHatchActivated: false }),
      null,
    )

    expect(canvasStrategy.strategy?.strategy.id).toEqual('ESCAPE_HATCH_STRATEGY')
  })
})
