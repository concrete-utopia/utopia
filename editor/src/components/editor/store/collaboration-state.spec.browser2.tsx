import { canvasPoint } from '../../../core/shared/math-utils'
import { CanvasContainerID } from '../../canvas/canvas-types'
import { mouseClickAtPoint } from '../../canvas/event-helpers.test-utils'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../canvas/ui-jsx.test-utils'
import { loggedInUser, notLoggedIn } from '../action-types'
import { setLoginState, updateProjectServerState } from '../actions/action-creators'
import { CollaborationEndpoints } from '../collaborative-endpoints'
import Sinon from 'sinon'

describe('CollaborationStateUpdater', () => {
  var sandbox = Sinon.createSandbox()
  afterEach(() => {
    sandbox.restore()
  })

  describe('when logged in', () => {
    it('snatching control on click should gain control', async () => {
      const snatchControlStub = sandbox.stub(CollaborationEndpoints, 'snatchControlOverProject')
      snatchControlStub.callsFake(async () => {
        return true
      })
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root' style={{top: 0, left: 0, width: 100, height: 100}} />
      `),
        'await-first-dom-report',
      )
      await renderResult.dispatch(
        [
          setLoginState(loggedInUser({ userId: '1' })),
          updateProjectServerState({
            currentlyHolderOfTheBaton: false,
          }),
        ],
        true,
      )
      const canvasRootContainer = renderResult.renderedDOM.getByTestId(CanvasContainerID)

      const rootRect = canvasRootContainer.getBoundingClientRect()
      const rootRectCenter = canvasPoint({
        x: rootRect.x + rootRect.width / 2,
        y: rootRect.y + rootRect.height / 2,
      })
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      Sinon.assert.calledOnce(snatchControlStub)
      expect(renderResult.getEditorState().projectServerState.currentlyHolderOfTheBaton).toEqual(
        true,
      )
    })
    it('snatching control on a bunch of fast clicks should gain control and not make lots of calls to the server', async () => {
      const snatchControlStub = sandbox.stub(CollaborationEndpoints, 'snatchControlOverProject')
      snatchControlStub.callsFake(async () => {
        return true
      })
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root' style={{top: 0, left: 0, width: 100, height: 100}} />
      `),
        'await-first-dom-report',
      )
      await renderResult.dispatch(
        [
          setLoginState(loggedInUser({ userId: '1' })),
          updateProjectServerState({
            currentlyHolderOfTheBaton: false,
            isMyProject: 'yes',
          }),
        ],
        true,
      )
      const canvasRootContainer = renderResult.renderedDOM.getByTestId(CanvasContainerID)

      const rootRect = canvasRootContainer.getBoundingClientRect()
      const rootRectCenter = canvasPoint({
        x: rootRect.x + rootRect.width / 2,
        y: rootRect.y + rootRect.height / 2,
      })
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      Sinon.assert.calledOnce(snatchControlStub)
      expect(renderResult.getEditorState().projectServerState.currentlyHolderOfTheBaton).toEqual(
        true,
      )
    })
  })

  describe('when not logged in', () => {
    it('snatching control on click should gain control', async () => {
      const snatchControlStub = sandbox.stub(CollaborationEndpoints, 'snatchControlOverProject')
      snatchControlStub.callsFake(async () => {
        return true
      })
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root' style={{top: 0, left: 0, width: 100, height: 100}} />
      `),
        'await-first-dom-report',
      )
      await renderResult.dispatch(
        [
          setLoginState(notLoggedIn),
          updateProjectServerState({
            currentlyHolderOfTheBaton: false,
          }),
        ],
        true,
      )
      const canvasRootContainer = renderResult.renderedDOM.getByTestId(CanvasContainerID)

      const rootRect = canvasRootContainer.getBoundingClientRect()
      const rootRectCenter = canvasPoint({
        x: rootRect.x + rootRect.width / 2,
        y: rootRect.y + rootRect.height / 2,
      })
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      Sinon.assert.notCalled(snatchControlStub)
      expect(renderResult.getEditorState().projectServerState.currentlyHolderOfTheBaton).toEqual(
        false,
      )
    })
    it('snatching control on a bunch of fast clicks should gain control and not make lots of calls to the server', async () => {
      const snatchControlStub = sandbox.stub(CollaborationEndpoints, 'snatchControlOverProject')
      snatchControlStub.callsFake(async () => {
        return true
      })
      const renderResult = await renderTestEditorWithCode(
        makeTestProjectCodeWithSnippet(`
        <div data-uid='root' style={{top: 0, left: 0, width: 100, height: 100}} />
      `),
        'await-first-dom-report',
      )
      await renderResult.dispatch(
        [
          setLoginState(notLoggedIn),
          updateProjectServerState({
            currentlyHolderOfTheBaton: false,
          }),
        ],
        true,
      )
      const canvasRootContainer = renderResult.renderedDOM.getByTestId(CanvasContainerID)

      const rootRect = canvasRootContainer.getBoundingClientRect()
      const rootRectCenter = canvasPoint({
        x: rootRect.x + rootRect.width / 2,
        y: rootRect.y + rootRect.height / 2,
      })
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      await mouseClickAtPoint(canvasRootContainer, rootRectCenter)
      Sinon.assert.notCalled(snatchControlStub)
      expect(renderResult.getEditorState().projectServerState.currentlyHolderOfTheBaton).toEqual(
        false,
      )
    })
  })
})
