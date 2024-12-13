import { add, baseAssetPromise, init, redo, undo } from './history'
import { createEditorState, deriveState } from './store/editor-state'
import { updateAssetFileName } from './server'
import { right } from '../../core/shared/either'

jest.mock('./server')

describe('history', () => {
  describe('asset renames', () => {
    beforeEach(() => {
      return jest.clearAllMocks()
    })

    it('should be added to the history', async () => {
      const editorState = {
        ...createEditorState(() => {}),
        id: 'testproject',
      }
      const derivedState = deriveState(editorState, null, 'unpatched', () => right(null))
      const stateHistory = init(editorState, derivedState)
      const updatedStateHistory = add(stateHistory, editorState, derivedState, [
        { filenameChangedFrom: 'thing.jpg', filenameChangedTo: 'otherthing.jpg' },
      ])
      await baseAssetPromise
      expect(updatedStateHistory.current.assetRenames).toMatchInlineSnapshot(`
        Array [
          Object {
            "filenameChangedFrom": "thing.jpg",
            "filenameChangedTo": "otherthing.jpg",
          },
        ]
      `)
    })
    it('should be undone with 1 undo if they were the last change', async () => {
      const editorState = {
        ...createEditorState(() => {}),
        id: 'testproject',
      }
      const derivedState = deriveState(editorState, null, 'unpatched', () => right(null))
      const stateHistory = init(editorState, derivedState)
      const updatedStateHistory = add(stateHistory, editorState, derivedState, [
        { filenameChangedFrom: 'thing.jpg', filenameChangedTo: 'otherthing.jpg' },
      ])
      const undoneStateHistory = undo('testproject', updatedStateHistory, 'run-side-effects')
      await baseAssetPromise
      expect(undoneStateHistory.current.assetRenames).toMatchInlineSnapshot(`Array []`)
      expect((updateAssetFileName as any).mock.calls).toHaveLength(1)
      expect((updateAssetFileName as any).mock.calls[0]).toMatchInlineSnapshot(`
        Array [
          "testproject",
          "otherthing.jpg",
          "thing.jpg",
        ]
      `)
    })
    it('should be redone with 1 redo if they are the next change', async () => {
      const editorState = {
        ...createEditorState(() => {}),
        id: 'testproject',
      }
      const derivedState = deriveState(editorState, null, 'unpatched', () => right(null))
      const stateHistory = init(editorState, derivedState)
      const updatedStateHistory = add(stateHistory, editorState, derivedState, [
        { filenameChangedFrom: 'thing.jpg', filenameChangedTo: 'otherthing.jpg' },
      ])
      const undoneStateHistory = undo('testproject', updatedStateHistory, 'run-side-effects')
      const redoneStateHistory = redo('testproject', undoneStateHistory, 'run-side-effects')
      await baseAssetPromise
      expect(redoneStateHistory.current.assetRenames).toMatchInlineSnapshot(`
        Array [
          Object {
            "filenameChangedFrom": "thing.jpg",
            "filenameChangedTo": "otherthing.jpg",
          },
        ]
      `)
      expect((updateAssetFileName as any).mock.calls).toHaveLength(2)
      expect((updateAssetFileName as any).mock.calls[0]).toMatchInlineSnapshot(`
        Array [
          "testproject",
          "otherthing.jpg",
          "thing.jpg",
        ]
      `)
      expect((updateAssetFileName as any).mock.calls[1]).toMatchInlineSnapshot(`
        Array [
          "testproject",
          "thing.jpg",
          "otherthing.jpg",
        ]
      `)
    })
  })
})
