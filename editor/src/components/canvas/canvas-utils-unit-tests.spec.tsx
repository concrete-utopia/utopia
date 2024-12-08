import * as EP from '../../core/shared/element-path'
import {
  makeTestProjectCodeWithSnippet,
  TestScenePath,
  testPrintCodeFromEditorState,
  getEditorState,
} from './ui-jsx.test-utils'
import type { EdgePosition, ScreenSize } from './canvas-types'
import { singleResizeChange, pinMoveChange, pinFrameChange } from './canvas-types'
import type { CanvasVector } from '../../core/shared/math-utils'
import { canvasRectangle } from '../../core/shared/math-utils'
import type { MediaQuery } from './canvas-utils'
import {
  extractScreenSizeFromCss,
  mediaQueryToScreenSize,
  updateFramesOfScenesAndComponents,
} from './canvas-utils'
import * as csstree from 'css-tree'
import { NO_OP } from '../../core/shared/utils'
import { editorModelFromPersistentModel } from '../editor/store/editor-state'
import { complexDefaultProjectPreParsed } from '../../sample-projects/sample-project-utils.test-utils'

describe('updateFramesOfScenesAndComponents - multi-file', () => {
  it('a simple TLWH pin change works', async () => {
    const testProject = editorModelFromPersistentModel(complexDefaultProjectPreParsed(), NO_OP)
    const targetPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div', 'card-inner-spring'],
    ])

    const pinChange = singleResizeChange(
      targetPath,
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(testProject, [pinChange], null)

    expect(testPrintCodeFromEditorState(updatedProject, '/src/card.js')).toMatchInlineSnapshot(`
      "import * as React from 'react'
      import { Spring } from 'non-existant-dummy-library'
      export var Card = (props) => {
        return (
          <div
            data-uid='card-outer-div'
            style={{ ...props.style }}
          >
            <div
              data-uid='card-inner-div'
              data-testid='card-inner-div'
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: 50,
                height: 50,
                backgroundColor: 'red',
              }}
            />
            <Spring
              data-uid='card-inner-spring'
              data-testid='spring'
              style={{
                position: 'absolute',
                left: 100,
                top: 200,
                width: 110,
                height: 90,
                backgroundColor: 'blue',
              }}
            />
          </div>
        )
      }
      "
    `)
  })

  it('an element move works', async () => {
    const testProject = editorModelFromPersistentModel(complexDefaultProjectPreParsed(), NO_OP)
    const targetPath = EP.elementPath([
      ['storyboard-entity', 'scene-1-entity', 'app-entity'],
      ['app-outer-div', 'card-instance'],
      ['card-outer-div', 'card-inner-spring'],
    ])

    const pinChange = pinMoveChange(targetPath, { x: 60, y: 40 } as CanvasVector)

    const updatedProject = updateFramesOfScenesAndComponents(testProject, [pinChange], null)

    expect(testPrintCodeFromEditorState(updatedProject, '/src/card.js')).toMatchInlineSnapshot(`
      "import * as React from 'react'
      import { Spring } from 'non-existant-dummy-library'
      export var Card = (props) => {
        return (
          <div
            data-uid='card-outer-div'
            style={{ ...props.style }}
          >
            <div
              data-uid='card-inner-div'
              data-testid='card-inner-div'
              style={{
                position: 'absolute',
                left: 0,
                top: 0,
                width: 50,
                height: 50,
                backgroundColor: 'red',
              }}
            />
            <Spring
              data-uid='card-inner-spring'
              data-testid='spring'
              style={{
                position: 'absolute',
                left: 160,
                top: 240,
                width: 50,
                height: 50,
                backgroundColor: 'blue',
              }}
            />
          </div>
        )
      }
      "
    `)
  })
})

describe('updateFramesOfScenesAndComponents - singleResizeChange -', () => {
  it('a simple TLWH pin change works', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 250, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 310, height: 340 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('TLW, missing H resizing from bottom right edge adds height', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, width: 256 }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 40, y: 30 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, width: 296, height: 30 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('TLWHBR, too many frame points work', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, width: 256, height: 202, bottom: 137, right: 93 }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 50, y: 50 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 52,
            top: 61,
            width: 306,
            height: 252,
            bottom: 87,
            right: 43,
          }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })

  it('TLRB pin change works, dragged from topleft point', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='outer-view'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, right: 50, bottom: 20 }}
        data-uid='inner-view'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      EP.appendNewElementPath(TestScenePath, ['outer-view', 'inner-view']),
      { x: 0, y: 0 } as EdgePosition,
      { x: 50, y: 20 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='outer-view'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 2, top: 41, right: 50, bottom: 20 }}
          data-uid='inner-view'
        />
      </View>`,
      ),
    )
  })
  it('TLRB pin change works, dragged from bottom right point', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='outer-view'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, right: 50, bottom: 20 }}
        data-uid='inner-view'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      EP.appendNewElementPath(TestScenePath, ['outer-view', 'inner-view']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 80, y: -10 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='outer-view'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 52, top: 61, right: -30, bottom: 30 }}
          data-uid='inner-view'
        />
      </View>`,
      ),
    )
  })
  it('a simple TLWH pin change with values in string pixels', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50px', top: '50px', width: '250px', height: '350px' }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50px', top: '50px', width: 310, height: 390 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('a simple TLWH pin change with values in percentages', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50px', top: '50px', width: '30%', height: '40%' }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50px', top: '50px', width: '45%', height: '50%' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('a simple TLWH pin change with expression, the expression is not changed', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 200 + 50, height: 300 }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: 200 + 50, height: 340 }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
  it('a simple TLWH pin change with values in exotic units', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50pt', top: '5em', width: '50vw', height: '10cm' }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = singleResizeChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      { x: 1, y: 1 } as EdgePosition,
      { x: 60, y: 40 } as CanvasVector,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50pt', top: '5em', width: '50vw', height: '10cm' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})

describe('updateFramesOfScenesAndComponents - pinFrameChange -', () => {
  it('a simple TLWH pin change with values in percentages', async () => {
    const testProject = getEditorState(
      makeTestProjectCodeWithSnippet(`
    <View style={{ ...(props.style || {}) }} data-uid='aaa'>
      <View
        style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: '50px', top: '50px', width: '30%', height: '40%' }}
        data-uid='bbb'
      />
    </View>
    `),
    )

    const pinChange = pinFrameChange(
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
      canvasRectangle({ x: 50, y: 50, width: 200, height: 300 }),
      { x: 1, y: 1 } as EdgePosition,
    )

    const updatedProject = updateFramesOfScenesAndComponents(
      testProject,
      [pinChange],
      canvasRectangle({ x: 0, y: 0, width: 400, height: 400 }),
    )

    expect(testPrintCodeFromEditorState(updatedProject)).toEqual(
      makeTestProjectCodeWithSnippet(
        `<View style={{ ...(props.style || {}) }} data-uid='aaa'>
        <View
          style={{ backgroundColor: '#aaaaaa33', position: 'absolute', left: 50, top: 50, width: '50%', height: '75%' }}
          data-uid='bbb'
        />
      </View>`,
      ),
    )
  })
})

describe('mediaQueryToScreenSize', () => {
  it('converts simple screen size queries', () => {
    const testCases: { input: string; expected: ScreenSize }[] = [
      {
        input: '@media (100px <width < 500px)',
        expected: { min: { value: 100, unit: 'px' }, max: { value: 500, unit: 'px' } },
      },
      {
        input: '@media (min-width: 100px) and (max-width: 500px)',
        expected: { min: { value: 100, unit: 'px' }, max: { value: 500, unit: 'px' } },
      },
      {
        input: '@media screen and (min-width: 100px)',
        expected: { min: { value: 100, unit: 'px' } },
      },
      {
        input: '@media (100px < width) and (max-width: 500px)',
        expected: { min: { value: 100, unit: 'px' }, max: { value: 500, unit: 'px' } },
      },
      {
        input: '@media (width > 100px)',
        expected: { min: { value: 100, unit: 'px' } },
      },
    ]
    testCases.forEach((testCase) => {
      csstree.walk(csstree.parse(testCase.input), (node) => {
        if (node.type === 'MediaQuery') {
          const result = mediaQueryToScreenSize(node as unknown as MediaQuery)
          expect(result).toEqual(testCase.expected)
        }
      })
    })
  })
})

describe('extractScreenSizeFromCss', () => {
  beforeEach(() => {
    // Clear the cache before each test
    ;(extractScreenSizeFromCss as any).screenSizeCache?.clear()
  })

  it('extracts screen size from simple media query', () => {
    const css = '@media (min-width: 100px) and (max-width: 500px)'
    const result = extractScreenSizeFromCss(css)
    expect(result).toEqual({
      min: { value: 100, unit: 'px' },
      max: { value: 500, unit: 'px' },
    })
  })

  it('returns null for invalid media query', () => {
    const css = 'not-a-media-query'
    const result = extractScreenSizeFromCss(css)
    expect(result).toBeNull()
  })

  it('uses cache for repeated calls with same CSS', () => {
    const css = '@media (min-width: 100px)'

    // First call
    const result1 = extractScreenSizeFromCss(css)
    // Second call - should return same object reference
    const result2 = extractScreenSizeFromCss(css)

    expect(result1).toBe(result2) // Use toBe for reference equality
    expect(result1).toEqual({
      min: { value: 100, unit: 'px' },
    })
  })

  it('caches null results', () => {
    const css = 'invalid-css'

    // First call
    const result1 = extractScreenSizeFromCss(css)
    // Second call - should return same null reference
    const result2 = extractScreenSizeFromCss(css)

    expect(result1).toBe(result2)
    expect(result1).toBeNull()
  })

  it('handles different CSS strings independently in cache', () => {
    const css1 = '@media (min-width: 100px)'
    const css2 = '@media (max-width: 500px)'

    // First string
    const result1a = extractScreenSizeFromCss(css1)
    const result1b = extractScreenSizeFromCss(css1)
    expect(result1a).toBe(result1b)
    expect(result1a).toEqual({
      min: { value: 100, unit: 'px' },
    })

    // Second string
    const result2a = extractScreenSizeFromCss(css2)
    const result2b = extractScreenSizeFromCss(css2)
    expect(result2a).toBe(result2b)
    expect(result2a).toEqual({
      max: { value: 500, unit: 'px' },
    })

    // Different strings should have different references
    expect(result1a).not.toBe(result2a)
  })
})
