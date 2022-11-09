import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import { pressKey } from '../../event-helpers.test-utils'
import * as EP from '../../../../core/shared/element-path'
import { KeyboardInteractionTimeout } from '../interaction-state'
import sinon, { SinonFakeTimers } from 'sinon'
import { selectComponents } from '../../../editor/actions/action-creators'

const TestProject = (
  display: 'block' | 'inline-block',
  parentDisplay: 'flex' | 'block',
  parentFlexDirection: 'row' | 'column' | 'row-reverse' | 'column-reverse' = 'row',
) => `
<div style={{ width: '100%', height: '100%', position: 'absolute', display: '${parentDisplay}', flexDirection: '${parentFlexDirection}' }} data-uid='container'>
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#CA1E4C80',
      display: '${display}',
    }}
    data-uid='aaa'
    data-testid='aaa'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#297374',
      display: '${display}',
    }}
    data-uid='bbb'
    data-testid='bbb'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#292E74',
      display: '${display}',
    }}
    data-uid='ccc'
    data-testid='ccc'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#FF00B3AB',
      display: '${display}',
    }}
    data-uid='ddd'
    data-testid='ddd'
  />
</div>
`

const TestProjectMixedInlineFlow = `
<div style={{ width: '100%', height: '100%', position: 'absolute' }} data-uid='container'>
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#CA1E4C80',
      display: 'block',
    }}
    data-uid='aaa'
    data-testid='aaa'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#297374',
      display: 'inline-block',
    }}
    data-uid='bbb'
    data-testid='bbb'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#292E74',
      display: 'inline-block',
    }}
    data-uid='ccc'
    data-testid='ccc'
  />
  <div
    style={{
      width: 50,
      height: 50,
      backgroundColor: '#FF00B3AB',
      display: 'block',
    }}
    data-uid='ddd'
    data-testid='ddd'
  />
</div>
`

function configureClock() {
  let clock: { current: SinonFakeTimers } = { current: null as any } // it will be non-null thanks to beforeEach
  beforeEach(function () {
    // TODO there is something wrong with sinon fake timers here that remotely break other tests that come after these. If your new browser tests are broken, this may be the reason.
    clock.current = sinon.useFakeTimers({
      // the timers will tick so the editor is not totally broken, but we can fast-forward time at will
      // WARNING: the Sinon fake timers will advance in 20ms increments
      shouldAdvanceTime: true,
    })
  })
  afterEach(function () {
    clock.current?.restore()
  })
  return { clock: clock }
}

async function pressKeysRepeat(
  clock: { current: SinonFakeTimers },
  renderResult: any,
  direction: 'ArrowLeft' | 'ArrowRight' | 'ArrowUp' | 'ArrowDown',
  repeat: number,
) {
  for (var i = 1; i <= repeat; i++) {
    pressKey(direction)
  }
  clock.current.tick(KeyboardInteractionTimeout)
  await renderResult.getDispatchFollowUpActionsFinished()
}

describe('Keyboard Reorder Strategy', () => {
  const { clock } = configureClock()

  it('pressing the arrow keys reorders in a flow layout', async () => {
    const TestProjectFlow = TestProject('block', 'block')
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectFlow),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/aaa')],
          false,
        ),
      ],
      true,
    )

    // pressing keyboard up and down reorders elements
    await pressKeysRepeat(clock, renderResult, 'ArrowDown', 2)

    const expectedNavigatorTargetsAfterArrowDown = [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(renderResult.getEditorState().derived.visibleNavigatorTargets.map(EP.toString)).toEqual(
      expectedNavigatorTargetsAfterArrowDown,
    )

    await pressKeysRepeat(clock, renderResult, 'ArrowUp', 1)

    const expectedNavigatorTargetsAfterArrowUp = [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(renderResult.getEditorState().derived.visibleNavigatorTargets.map(EP.toString)).toEqual(
      expectedNavigatorTargetsAfterArrowUp,
    )
  })

  it('pressing the arrow keys reorders in a flex layout', async () => {
    const TestProjectFlexRow = TestProject('block', 'flex', 'row')
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectFlexRow),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/aaa')],
          false,
        ),
      ],
      true,
    )

    // pressing keyboard left and right reorders elements
    await pressKeysRepeat(clock, renderResult, 'ArrowRight', 3)

    const expectedNavigatorTargetsAfterArrowRight = [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
    ]
    expect(renderResult.getEditorState().derived.visibleNavigatorTargets.map(EP.toString)).toEqual(
      expectedNavigatorTargetsAfterArrowRight,
    )

    await pressKeysRepeat(clock, renderResult, 'ArrowLeft', 1)

    const expectedNavigatorTargetsAfterLeft = [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(renderResult.getEditorState().derived.visibleNavigatorTargets.map(EP.toString)).toEqual(
      expectedNavigatorTargetsAfterLeft,
    )
  })
  it('pressing the arrow keys reorders in a flex column-reverse layout', async () => {
    const TestProjectFlexColumnReverse = TestProject('block', 'flex', 'column-reverse')
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectFlexColumnReverse),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/aaa')],
          false,
        ),
      ],
      true,
    )

    // pressing keyboard up and down reorders elements
    await pressKeysRepeat(clock, renderResult, 'ArrowUp', 2)

    const expectedNavigatorTargetsAfterArrowUp = [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(renderResult.getEditorState().derived.visibleNavigatorTargets.map(EP.toString)).toEqual(
      expectedNavigatorTargetsAfterArrowUp,
    )

    await pressKeysRepeat(clock, renderResult, 'ArrowDown', 1)

    const expectedNavigatorTargetsAfterArrowDown = [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(renderResult.getEditorState().derived.visibleNavigatorTargets.map(EP.toString)).toEqual(
      expectedNavigatorTargetsAfterArrowDown,
    )
  })

  it('pressing the arrow keys reorders in a 2d flow layout', async () => {
    const renderResult = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet(TestProjectMixedInlineFlow),
      'await-first-dom-report',
    )

    await renderResult.dispatch(
      [
        selectComponents(
          [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:container/aaa')],
          false,
        ),
      ],
      true,
    )

    // pressing keyboard up and down reorders elements
    await pressKeysRepeat(clock, renderResult, 'ArrowDown', 2)

    const expectedNavigatorTargetsAfterArrowDown = [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(renderResult.getEditorState().derived.visibleNavigatorTargets.map(EP.toString)).toEqual(
      expectedNavigatorTargetsAfterArrowDown,
    )

    await pressKeysRepeat(clock, renderResult, 'ArrowUp', 1)

    const expectedNavigatorTargetsAfterArrowUp = [
      'utopia-storyboard-uid/scene-aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity',
      'utopia-storyboard-uid/scene-aaa/app-entity:container',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(renderResult.getEditorState().derived.visibleNavigatorTargets.map(EP.toString)).toEqual(
      expectedNavigatorTargetsAfterArrowUp,
    )
  })
})
