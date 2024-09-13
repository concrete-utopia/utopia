import { matchInlineSnapshotBrowser } from '../../../../../test/karma-snapshots'
import { runDOMWalker } from '../../../editor/actions/action-creators'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import { getGlobalFramesOfGridCellsFromMetadata } from './grid-helpers'

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
        gridGap: 10,
        padding: 10,
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
      getGlobalFramesOfGridCellsFromMetadata(
        editor.getEditorState().editor.jsxMetadata[
          'utopia-storyboard-uid/scene-aaa/app-entity:grid/child'
        ],
      ),
    ).toBeNull()

    // grids have cell measurements:
    matchInlineSnapshotBrowser(
      getGlobalFramesOfGridCellsFromMetadata(
        editor.getEditorState().editor.jsxMetadata[
          'utopia-storyboard-uid/scene-aaa/app-entity:grid'
        ],
      ),
      `Array [
  Array [
    Object {
      \"height\": 50,
      \"width\": 150,
      \"x\": 10,
      \"y\": 10,
    },
    Object {
      \"height\": 50,
      \"width\": 80,
      \"x\": 170,
      \"y\": 10,
    },
    Object {
      \"height\": 50,
      \"width\": 119.5,
      \"x\": 260,
      \"y\": 10,
    },
    Object {
      \"height\": 50,
      \"width\": 119.5,
      \"x\": 389.5,
      \"y\": 10,
    },
  ],
  Array [
    Object {
      \"height\": 83.25,
      \"width\": 150,
      \"x\": 10,
      \"y\": 70,
    },
    Object {
      \"height\": 83.25,
      \"width\": 80,
      \"x\": 170,
      \"y\": 70,
    },
    Object {
      \"height\": 83.25,
      \"width\": 119.5,
      \"x\": 260,
      \"y\": 70,
    },
    Object {
      \"height\": 83.25,
      \"width\": 119.5,
      \"x\": 389.5,
      \"y\": 70,
    },
  ],
  Array [
    Object {
      \"height\": 83.25,
      \"width\": 150,
      \"x\": 10,
      \"y\": 163.25,
    },
    Object {
      \"height\": 83.25,
      \"width\": 80,
      \"x\": 170,
      \"y\": 163.25,
    },
    Object {
      \"height\": 83.25,
      \"width\": 119.5,
      \"x\": 260,
      \"y\": 163.25,
    },
    Object {
      \"height\": 83.25,
      \"width\": 119.5,
      \"x\": 389.5,
      \"y\": 163.25,
    },
  ],
  Array [
    Object {
      \"height\": 83.25,
      \"width\": 150,
      \"x\": 10,
      \"y\": 256.5,
    },
    Object {
      \"height\": 83.25,
      \"width\": 80,
      \"x\": 170,
      \"y\": 256.5,
    },
    Object {
      \"height\": 83.25,
      \"width\": 119.5,
      \"x\": 260,
      \"y\": 256.5,
    },
    Object {
      \"height\": 83.25,
      \"width\": 119.5,
      \"x\": 389.5,
      \"y\": 256.5,
    },
  ],
  Array [
    Object {
      \"height\": 83.25,
      \"width\": 150,
      \"x\": 10,
      \"y\": 349.75,
    },
    Object {
      \"height\": 83.25,
      \"width\": 80,
      \"x\": 170,
      \"y\": 349.75,
    },
    Object {
      \"height\": 83.25,
      \"width\": 119.5,
      \"x\": 260,
      \"y\": 349.75,
    },
    Object {
      \"height\": 83.25,
      \"width\": 119.5,
      \"x\": 389.5,
      \"y\": 349.75,
    },
  ],
]`,
    )
  })
})
