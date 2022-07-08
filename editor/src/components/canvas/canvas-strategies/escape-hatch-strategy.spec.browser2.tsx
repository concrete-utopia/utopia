import { act, fireEvent } from '@testing-library/react'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { localRectangle, LocalRectangle } from '../../../core/shared/math-utils'
import { runEscapeHatch } from '../../editor/actions/action-creators'
import {
  getPrintedUiJsCode,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
  TestScenePath,
} from '../ui-jsx.test-utils'

const absoluteDescendantProject = (absoluteFrame: LocalRectangle) => {
  return `
    <div
      style={{
        position: 'absolute',
        width: 100,
        height: 200,
        left: 10,
        top: 20,
      }}
      data-uid='aaa'
    >
      <div
        style={{
          backgroundColor: '#FF00BFAB',
          width: 80,
          height: 80,
        }}
        data-uid='bbb'
      />
      <div
        style={{
          display: 'flex',
          padding: 8,
          flexDirection: 'column',
        }}
        data-uid='ccc'
      >
        <div style={{ display: 'flex' }} data-uid='ddd'>
          <div
            data-uid='eee'
            style={{
              position: 'absolute',
              width: ${absoluteFrame.width},
              top: ${absoluteFrame.y},
              height: ${absoluteFrame.height},
              left: ${absoluteFrame.x},
            }}
          >
            Hello Text
          </div>
          <div
            style={{
              position: 'relative',
              flexBasis: 100,
              height: 150,
              backgroundColor: '#6CF8C0',
            }}
            data-uid='fff'
          >
            <span
              data-uid='ggg'
              style={{
                position: 'absolute',
                width: ${absoluteFrame.width},
                top: ${absoluteFrame.y},
                height: ${absoluteFrame.height},
                left: ${absoluteFrame.x},
              }}
            >
              Hello absolute with relative parent
            </span>
          </div>
        </div>
      </div>
    </div>`
}

describe('Convert to Absolute/runEscapeHatch action', () => {
  it('Converts 1 element to absolute', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 80 }}
            data-uid='bbb'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 120 }}
            data-uid='ccc'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc'])

    await renderResult.dispatch([runEscapeHatch([target])], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 80 }}
            data-uid='bbb'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 120, position: 'absolute', left: 0, top: 80 }}
            data-uid='ccc'
          />
        </div>
      `),
    )
  })
  it('Converts 1 element to absolute from flex containers, keeping the visual position', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%', position: 'relative', display: 'flex' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 80 }}
            data-uid='bbb'
          />
          <div
            style={{ display: 'flex', flexDirection: 'column' }}
            data-uid='ccc'
          >
            <div style={{ width: 100, height: 100, backgroundColor: 'hotpink'}} data-uid='ddd' />
            <div style={{ width: 100, height: 100, backgroundColor: 'yellow'}} data-uid='eee' />
          </div>
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc', 'eee'])

    await renderResult.dispatch([runEscapeHatch([target])], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ width: '100%', height: '100%', position: 'relative', display: 'flex' }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#0091FFAA', width: 200, height: 80 }}
          data-uid='bbb'
        />
        <div
          style={{ display: 'flex', flexDirection: 'column' }}
          data-uid='ccc'
        >
          <div style={{ width: 100, height: 100, backgroundColor: 'hotpink'}} data-uid='ddd' />
          <div style={{ width: 100, height: 100, backgroundColor: 'yellow', position: 'absolute', left: 200, top: 100 }} data-uid='eee' />
        </div>
      </div>
      `),
    )
  })
  it('Converts a static element to absolute where the parent is absolute', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ position: 'relative', width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', position: 'absolute', top: 45, left: 55, width: 200, height: 120 }}
            data-uid='ccc'
          >
            <div style={{ lineHeight: '20px' }} data-uid='ddd'>
              hello there
            </div>
          </div>
        </div>
      `),
      'await-first-dom-report',
    )

    const target = EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc', 'ddd'])

    await renderResult.dispatch([runEscapeHatch([target])], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
      <div style={{ position: 'relative', width: '100%', height: '100%' }} data-uid='aaa'>
        <div
          style={{ backgroundColor: '#0091FFAA', position: 'absolute', top: 45, left: 55, width: 200, height: 120 }}
          data-uid='ccc'
        >
          <div style={{ lineHeight: '20px', position: 'absolute', left: 0, width: 200, top: 0, height: 20 }} data-uid='ddd'>
            hello there
          </div>
        </div>
      </div>
      `),
    )
  })
  it('Converts multiselected static siblings to absolute', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 80 }}
            data-uid='bbb'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 120 }}
            data-uid='ccc'
          />
        </div>
      `),
      'await-first-dom-report',
    )

    const targets = [
      EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc']),
      EP.appendNewElementPath(TestScenePath, ['aaa', 'bbb']),
    ]
    await renderResult.dispatch([runEscapeHatch(targets)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 80, position: 'absolute', left: 0, top: 0 }}
            data-uid='bbb'
          />
          <div
            style={{ backgroundColor: '#0091FFAA', width: 200, height: 120, position: 'absolute', left: 0, top: 80 }}
            data-uid='ccc'
          />
        </div>
      `),
    )
  })
  it('Converts multiselect in hierarchy', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`
      <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
        <div
          style={{ width: 80, height: 80 }}
          data-uid='bbb'
        />
        <div
          style={{
            display: 'flex',
            padding: 8,
            gap: 8,
            flexDirection: 'column',
          }}
          data-uid='ccc'
        >
          <div
            style={{
              display: 'flex',
              gap: 70,
              flexDirection: 'row',
              justifyContent: 'space-between',
              alignItems: 'baseline',
            }}
            data-uid='ddd'
          >
            <div data-uid='eee'>Hello Text</div>
            <div
              style={{
                display: 'flex',
                flexDirection: 'row',
                gap: 15,
              }}
              data-uid='fff'
            >
              <div
                style={{
                  height: 57,
                  width: 54,
                  border: '1px solid rgb(0, 0, 0, 1)',
                }}
                data-uid='ggg'
              />
                <div
                  style={{
                    display: 'flex',
                    flexDirection: 'row',
                    gap: 15,
                  }}
                  data-uid='hhh'
                >
                <div
                  style={{
                    backgroundColor: '#0091FFAA',
                    height: 15,
                    width: 15,
                    border: '1px solid rgb(0, 0, 0, 1)',
                  }}
                  data-uid='iii'
                />
                <div
                  style={{
                    backgroundColor: '#0091FFAA',
                    border: '1px solid rgb(0, 0, 0, 1)',
                    width: 15,
                    height: 15,
                  }}
                  data-uid='jjj'
                />
              </div>
            </div>
          </div>
        </div>
      </div>
      `),
      'await-first-dom-report',
    )

    const targets = [
      EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc', 'ddd']),
      EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc', 'ddd', 'fff', 'hhh']),
      EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc', 'ddd', 'fff']),
    ]
    await renderResult.dispatch([runEscapeHatch(targets)], true)

    expect(getPrintedUiJsCode(renderResult.getEditorState())).toEqual(
      makeTestProjectCodeWithSnippet(`
        <div style={{ width: '100%', height: '100%' }} data-uid='aaa'>
          <div
            style={{ width: 80, height: 80 }}
            data-uid='bbb'
          />
          <div
            style={{
              display: 'flex',
              padding: 8,
              gap: 8,
              flexDirection: 'column',
            }}
            data-uid='ccc'
          >
            <div
              style={{
                display: 'flex',
                gap: 70,
                flexDirection: 'row',
                justifyContent: 'space-between',
                alignItems: 'baseline',
                position: 'absolute',
                left: 8,
                width: 384,
                top: 88,
                height: 63,
              }}
              data-uid='ddd'
            >
              <div data-uid='eee'>Hello Text</div>
            </div>
            <div
              style={{
                display: 'flex',
                flexDirection: 'row',
                gap: 15,
                position: 'absolute',
                left: 343,
                width: 49,
                top: 88,
                height: 59,
              }}
              data-uid='hhh'
            >
              <div
                style={{
                  backgroundColor: '#0091FFAA',
                  height: 15,
                  width: 15,
                  border: '1px solid rgb(0, 0, 0, 1)',
                }}
                data-uid='iii'
              />
              <div
                style={{
                  backgroundColor: '#0091FFAA',
                  border: '1px solid rgb(0, 0, 0, 1)',
                  width: 15,
                  height: 15,
                }}
                data-uid='jjj'
              />
            </div>
            <div
              style={{
                display: 'flex',
                flexDirection: 'row',
                gap: 15,
                position: 'absolute',
                left: 272,
                width: 120,
                top: 88,
                height: 59
              }}
              data-uid='fff'
            >
              <div
                style={{
                  height: 57,
                  width: 54,
                  border: '1px solid rgb(0, 0, 0, 1)',
                }}
                data-uid='ggg'
              />
            </div>
        </div>
      </div>
      `),
    )
  })
  it('Converts element with absolute descendants, the absolute descedants are moved to keep their visual position to the new containing block', async () => {
    const startingAbsoluteFrame = localRectangle({ x: 42, y: 218, width: 100, height: 20 })
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(absoluteDescendantProject(startingAbsoluteFrame)),
      'await-first-dom-report',
    )

    const target = [EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc'])]
    await renderResult.dispatch([runEscapeHatch(target)], true)

    const absoluteElementPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc', 'ddd', 'eee'])
    const absoluteFrame = MetadataUtils.getFrame(
      absoluteElementPath,
      renderResult.getEditorState().editor.jsxMetadata,
    )
    const expectedFrame = localRectangle({ x: 34, y: 130, width: 100, height: 20 })
    expect(absoluteFrame).toEqual(expectedFrame)

    const absoluteFromRelativeElementPath = EP.appendNewElementPath(TestScenePath, [
      'aaa',
      'ccc',
      'ddd',
      'fff',
      'ggg',
    ])
    const absoluteFromRelativeFrame = MetadataUtils.getFrame(
      absoluteFromRelativeElementPath,
      renderResult.getEditorState().editor.jsxMetadata,
    )
    expect(absoluteFromRelativeFrame).toEqual(startingAbsoluteFrame)
  })
  it('Converts multiple elements with absolute descendant, it is moved to keep their visual position to the new containing block', async () => {
    const startingAbsoluteFrame = localRectangle({ x: 42, y: 218, width: 100, height: 20 })
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(absoluteDescendantProject(startingAbsoluteFrame)),
      'await-first-dom-report',
    )

    const targets = [
      EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc']),
      EP.appendNewElementPath(TestScenePath, ['aaa', 'ccc', 'ddd']),
    ]
    await renderResult.dispatch([runEscapeHatch(targets)], true)

    const absoluteElementPath = EP.appendNewElementPath(TestScenePath, ['aaa', 'ddd', 'eee'])
    const absoluteFrame = MetadataUtils.getFrame(
      absoluteElementPath,
      renderResult.getEditorState().editor.jsxMetadata,
    )
    const expectedFrame = localRectangle({ x: 34, y: 130, width: 100, height: 20 })
    expect(absoluteFrame).toEqual(expectedFrame)
  })
})
