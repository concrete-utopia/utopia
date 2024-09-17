import { matchInlineSnapshotBrowser } from '../../../../../test/karma-snapshots'
import { runDOMWalker } from '../../../editor/actions/action-creators'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import { getGlobalFramesOfGridCells } from './grid-helpers'

describe('Grids', () => {
  it('can calculate global frames of grid cells', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(`<div
      style={{
        backgroundColor: '#fff',
        position: 'absolute',
        left: 127,
        top: 84,
        width: 519,
        height: 443,
        display: 'grid',
        gridTemplateColumns: '150px min-content 1fr 1fr',
        gridTemplateRows: 'min-content 1fr 1fr 1fr 1fr',
        rowGap: 10,
        columnGap: 20,
        padding: 5,
      }}
      data-uid={'grid'}
    >
      <div
        style={{
          backgroundColor: '#09f',
          width: '100%',
          height: '100%',
          gridColumn: 1,
          gridRow: 1,
          borderRadius: 10,
        }}
        data-uid={'child'}
      />
      <div
        style={{
          backgroundColor: '#9f0',
          width: 80,
          height: 50,
          gridColumn: 2,
          gridRow: 1,
          borderTopLeftRadius: 10,
          borderTopRightRadius: 10,
          borderBottomRightRadius: 10,
          borderBottomLeftRadius: 10,
        }}
      />
      <div
        style={{
          backgroundColor: '#f09',
          width: '100%',
          height: '100%',
          gridColumn: 1,
          gridRowStart: 2,
          gridRowEnd: 6,
          borderTopLeftRadius: 10,
          borderTopRightRadius: 10,
          borderBottomRightRadius: 10,
          borderBottomLeftRadius: 10,
        }}
      />
      <div
        style={{
          backgroundColor: '#f90',
          width: '100%',
          height: '100%',
          gridColumn: 2,
          gridRow: 3,
          borderRadius: 10,
        }}
      />
      <div
        style={{
          backgroundColor: '#f90',
          width: '100%',
          height: '100%',
          gridColumn: 2,
          gridRow: 3,
          borderRadius: 10,
        }}
      />
      <div
        style={{
          backgroundColor: '#90f',
          width: '100%',
          height: '100%',
          gridColumn: 2,
          gridRow: 4,
          borderRadius: 10,
        }}
      />
      <div
        style={{
          backgroundColor: '#0f9',
          width: 39,
          height: 39,
          alignSelf: 'center',
          justifySelf: 'center',
          gridColumn: 2,
          gridRow: 5,
          borderRadius: 10,
        }}
      />
    </div>`),
      'await-first-dom-report',
    )

    await editor.dispatch([runDOMWalker()], true)

    // non-grids don't have cell measurements:
    expect(
      getGlobalFramesOfGridCells(
        editor.getEditorState().editor.jsxMetadata[
          'utopia-storyboard-uid/scene-aaa/app-entity:grid/child'
        ],
      ),
    ).toBeNull()

    // grids have cell measurements:
    matchInlineSnapshotBrowser(
      getGlobalFramesOfGridCells(
        editor.getEditorState().editor.jsxMetadata[
          'utopia-storyboard-uid/scene-aaa/app-entity:grid'
        ],
      ),
      `Array [
  Array [
    Object {
      \"height\": 50,
      \"width\": 150,
      \"x\": 132,
      \"y\": 89,
    },
    Object {
      \"height\": 50,
      \"width\": 80,
      \"x\": 302,
      \"y\": 89,
    },
    Object {
      \"height\": 50,
      \"width\": 109.5,
      \"x\": 402,
      \"y\": 89,
    },
    Object {
      \"height\": 50,
      \"width\": 109.5,
      \"x\": 531.5,
      \"y\": 89,
    },
  ],
  Array [
    Object {
      \"height\": 85.75,
      \"width\": 150,
      \"x\": 132,
      \"y\": 149,
    },
    Object {
      \"height\": 85.75,
      \"width\": 80,
      \"x\": 302,
      \"y\": 149,
    },
    Object {
      \"height\": 85.75,
      \"width\": 109.5,
      \"x\": 402,
      \"y\": 149,
    },
    Object {
      \"height\": 85.75,
      \"width\": 109.5,
      \"x\": 531.5,
      \"y\": 149,
    },
  ],
  Array [
    Object {
      \"height\": 85.75,
      \"width\": 150,
      \"x\": 132,
      \"y\": 244.75,
    },
    Object {
      \"height\": 85.75,
      \"width\": 80,
      \"x\": 302,
      \"y\": 244.75,
    },
    Object {
      \"height\": 85.75,
      \"width\": 109.5,
      \"x\": 402,
      \"y\": 244.75,
    },
    Object {
      \"height\": 85.75,
      \"width\": 109.5,
      \"x\": 531.5,
      \"y\": 244.75,
    },
  ],
  Array [
    Object {
      \"height\": 85.75,
      \"width\": 150,
      \"x\": 132,
      \"y\": 340.5,
    },
    Object {
      \"height\": 85.75,
      \"width\": 80,
      \"x\": 302,
      \"y\": 340.5,
    },
    Object {
      \"height\": 85.75,
      \"width\": 109.5,
      \"x\": 402,
      \"y\": 340.5,
    },
    Object {
      \"height\": 85.75,
      \"width\": 109.5,
      \"x\": 531.5,
      \"y\": 340.5,
    },
  ],
  Array [
    Object {
      \"height\": 85.75,
      \"width\": 150,
      \"x\": 132,
      \"y\": 436.25,
    },
    Object {
      \"height\": 85.75,
      \"width\": 80,
      \"x\": 302,
      \"y\": 436.25,
    },
    Object {
      \"height\": 85.75,
      \"width\": 109.5,
      \"x\": 402,
      \"y\": 436.25,
    },
    Object {
      \"height\": 85.75,
      \"width\": 109.5,
      \"x\": 531.5,
      \"y\": 436.25,
    },
  ],
]`,
    )
  })
})
