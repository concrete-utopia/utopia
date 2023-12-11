import { set, unsafeGet } from '../../core/shared/optics/optic-utilities'
import { runLocalCanvasAction } from '../../templates/editor-canvas'
import { canvasPoint, point } from '../../core/shared/math-utils'
import CanvasActions from '../canvas/canvas-actions'
import { editorStateToolbarModeOptic, maybeClearPseudoInsertMode } from './canvas-toolbar-states'
import {
  defaultUserState,
  emptyCollaborativeEditingSupport,
  StoryboardFilePath,
} from './store/editor-state'
import { setFocus } from '../common/actions'
import { runLocalEditorAction } from './store/editor-update'
import type { CreateEditorStatesResult } from '../../utils/utils.test-utils'
import { createEditorStates } from '../../utils/utils.test-utils'
import { fromField } from '../../core/shared/optics/optic-creators'
import { emptyUiJsxCanvasContextData } from '../canvas/ui-jsx-canvas'
import * as History from '../editor/history'
import { MockUtopiaTsWorkers } from '../../core/workers/workers'
import {
  updateFromWorker,
  updateJSXElementName,
  workerParsedUpdate,
} from './actions/action-creators'
import { ScenePathForTestUiJsFile } from '../../core/model/test-ui-js-file.test-utils'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import {
  boundingArea,
  createInteractionViaMouse,
} from '../canvas/canvas-strategies/interaction-state'
import { emptyModifiers } from '../../utils/modifiers'
import { unparsed } from '../../core/shared/project-file-types'
import { emptyProjectServerState } from './store/project-server-state'

const workers = new MockUtopiaTsWorkers()

describe('maybeClearPseudoInsertMode', () => {
  it('keeps the insert mode when scrolling', () => {
    const { editor, derivedState, dispatch } = set(
      fromField<CreateEditorStatesResult, 'editor'>('editor').compose(editorStateToolbarModeOptic),
      'pseudo-insert',
      createEditorStates(),
    )
    expect(unsafeGet(editorStateToolbarModeOptic, editor)).toEqual('pseudo-insert')
    const scrollAction = CanvasActions.scrollCanvas(canvasPoint(point(100, 200)))
    const editorStateAfterAction = runLocalCanvasAction(
      dispatch,
      editor,
      derivedState,
      [],
      scrollAction,
    )
    const editorStateAfterPseudoInsertUpdate = maybeClearPseudoInsertMode(
      editor,
      editorStateAfterAction,
      scrollAction,
    )
    expect(editorStateAfterPseudoInsertUpdate).toBe(editorStateAfterAction)
    expect(unsafeGet(editorStateToolbarModeOptic, editorStateAfterPseudoInsertUpdate)).toEqual(
      'pseudo-insert',
    )
  })
  it('loses the insert mode when the focused panel changes', () => {
    const { editor, derivedState, dispatch } = set(
      fromField<CreateEditorStatesResult, 'editor'>('editor').compose(editorStateToolbarModeOptic),
      'pseudo-insert',
      createEditorStates(),
    )
    expect(unsafeGet(editorStateToolbarModeOptic, editor)).toEqual('pseudo-insert')

    const focusAction = setFocus('inspector')
    const editorStateAfterAction = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      focusAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      [],
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const editorStateAfterPseudoInsertUpdate = maybeClearPseudoInsertMode(
      editor,
      editorStateAfterAction,
      focusAction,
    )
    expect(editorStateAfterPseudoInsertUpdate).not.toEqual(editorStateAfterAction)
    expect(unsafeGet(editorStateToolbarModeOptic, editorStateAfterPseudoInsertUpdate)).toEqual(
      'none',
    )
  })
  it('keeps the insert mode when project contents change but that originated from a worker', () => {
    const { editor, derivedState, dispatch } = set(
      fromField<CreateEditorStatesResult, 'editor'>('editor').compose(editorStateToolbarModeOptic),
      'pseudo-insert',
      createEditorStates(),
    )
    expect(unsafeGet(editorStateToolbarModeOptic, editor)).toEqual('pseudo-insert')

    const updateFromWorkerAction = updateFromWorker([
      workerParsedUpdate(StoryboardFilePath, unparsed, 5000),
    ])

    const editorStateAfterAction = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      updateFromWorkerAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      [],
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const editorStateAfterPseudoInsertUpdate = maybeClearPseudoInsertMode(
      editor,
      editorStateAfterAction,
      updateFromWorkerAction,
    )
    expect(editorStateAfterPseudoInsertUpdate).toEqual(editorStateAfterAction)
    expect(unsafeGet(editorStateToolbarModeOptic, editorStateAfterPseudoInsertUpdate)).toEqual(
      'pseudo-insert',
    )
  })
  it('loses the insert mode when project contents change', () => {
    const { editor, derivedState, dispatch } = set(
      fromField<CreateEditorStatesResult, 'editor'>('editor').compose(editorStateToolbarModeOptic),
      'pseudo-insert',
      createEditorStates(),
    )
    expect(unsafeGet(editorStateToolbarModeOptic, editor)).toEqual('pseudo-insert')

    const updateJSXElementAction = updateJSXElementName(
      ScenePathForTestUiJsFile,
      { type: 'JSX_FRAGMENT' },
      emptyImports(),
    )
    const editorStateAfterAction = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      updateJSXElementAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      [],
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const editorStateAfterPseudoInsertUpdate = maybeClearPseudoInsertMode(
      editor,
      editorStateAfterAction,
      updateJSXElementAction,
    )
    expect(editorStateAfterPseudoInsertUpdate).not.toEqual(editorStateAfterAction)
    expect(unsafeGet(editorStateToolbarModeOptic, editorStateAfterPseudoInsertUpdate)).toEqual(
      'none',
    )
  })
  it('loses the insert mode when an interaction session is created', () => {
    const { editor, derivedState, dispatch } = set(
      fromField<CreateEditorStatesResult, 'editor'>('editor').compose(editorStateToolbarModeOptic),
      'pseudo-insert',
      createEditorStates(),
    )
    expect(unsafeGet(editorStateToolbarModeOptic, editor)).toEqual('pseudo-insert')

    const createInteractionSessionAction = CanvasActions.createInteractionSession(
      createInteractionViaMouse(
        canvasPoint({ x: 0, y: 0 }),
        emptyModifiers,
        boundingArea(),
        'zero-drag-permitted',
      ),
    )
    const editorStateAfterAction = runLocalCanvasAction(
      dispatch,
      editor,
      derivedState,
      [],
      createInteractionSessionAction,
    )
    const editorStateAfterPseudoInsertUpdate = maybeClearPseudoInsertMode(
      editor,
      editorStateAfterAction,
      createInteractionSessionAction,
    )
    expect(editorStateAfterPseudoInsertUpdate).not.toEqual(editorStateAfterAction)
    expect(unsafeGet(editorStateToolbarModeOptic, editorStateAfterPseudoInsertUpdate)).toEqual(
      'none',
    )
  })
  it('leaves the mode unchanged when the pseudo mode is not set', () => {
    const { editor, derivedState, dispatch } = createEditorStates()
    expect(unsafeGet(editorStateToolbarModeOptic, editor)).toEqual('none')

    const updateJSXElementAction = updateJSXElementName(
      ScenePathForTestUiJsFile,
      { type: 'JSX_FRAGMENT' },
      emptyImports(),
    )
    const editorStateAfterAction = runLocalEditorAction(
      editor,
      derivedState,
      defaultUserState,
      workers,
      updateJSXElementAction,
      History.init(editor, derivedState),
      dispatch,
      emptyUiJsxCanvasContextData(),
      [],
      emptyCollaborativeEditingSupport(),
      emptyProjectServerState(),
    )
    const editorStateAfterPseudoInsertUpdate = maybeClearPseudoInsertMode(
      editor,
      editorStateAfterAction,
      updateJSXElementAction,
    )
    expect(editorStateAfterPseudoInsertUpdate).toBe(editorStateAfterAction)
    expect(unsafeGet(editorStateToolbarModeOptic, editorStateAfterPseudoInsertUpdate)).toEqual(
      'none',
    )
  })
})
