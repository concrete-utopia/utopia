import * as EP from '../../../../core/shared/element-path'
import { setFeatureForBrowserTests } from '../../../../utils/utils.test-utils'
import { pressKey } from '../../event-helpers.test-utils'
import {
  EditorRenderResult,
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../ui-jsx.test-utils'
import { KeyboardInteractionTimeout } from '../interaction-state'
import sinon, { SinonFakeTimers } from 'sinon'
import { selectComponents } from '../../../editor/actions/action-creators'
import {
  NavigatorEntry,
  navigatorEntryToKey,
} from '../../../../components/editor/store/editor-state'
import {
  getClosingFragmentLikeTag,
  getOpeningFragmentLikeTag,
  getRegularNavigatorTargets,
} from './group-like-helpers.test-utils'
import { AllFragmentLikeTypes, FragmentLikeType } from './group-like-helpers'
import { assertNever } from '../../../../core/shared/utils'

const TestProject = (
  display: 'block' | 'inline-block',
  parentDisplay: 'flex' | 'block',
  parentTextDirection: 'ltr' | 'rtl' = 'ltr',
  parentFlexDirection: 'row' | 'column' | 'row-reverse' | 'column-reverse' = 'row',
) => `
<div style={{ width: '100%', height: '100%', position: 'absolute', display: '${parentDisplay}', flexDirection: '${parentFlexDirection}', direction: '${parentTextDirection}' }} data-uid='container'>
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

const TestProjectWithFragment = (type: FragmentLikeType) => `
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 47,
        top: 89,
        width: 669,
        height: 338,
        display: 'flex',
        gap: 33,
      }}
      data-uid='parent'
    >
      <div
        style={{
          backgroundColor: '#00abff',
          width: 123,
          height: 215,
          contain: 'layout',
        }}
        data-uid='child1'
      />
      ${getOpeningFragmentLikeTag(type)}
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 94,
            height: 171,
            contain: 'layout',
          }}
          data-uid='fragment-like-child1'
        />
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            width: 156,
            height: 184,
            contain: 'layout',
          }}
          data-uid='fragment-like-child2'
        />
      ${getClosingFragmentLikeTag(type)}
      <div
        style={{
          backgroundColor: '#ff0000',
          width: 156,
          height: 184,
          contain: 'layout',
        }}
        data-uid='child2'
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
  renderResult: EditorRenderResult,
  direction: 'ArrowLeft' | 'ArrowRight' | 'ArrowUp' | 'ArrowDown',
  repeat: number,
) {
  for (var i = 1; i <= repeat; i++) {
    await pressKey(direction)
  }
  clock.current.tick(KeyboardInteractionTimeout)
  await renderResult.getDispatchFollowUpActionsFinished()
}

// In standard layouts keyboard up and left moves backward and keyboard down and right moves forward.
// In flex reverse layouts this is fully reversed: keyboard up and left moves forward and keyboard down and right moves backward.
// If you have rtl text direction on top of any layouts, that should switch the effect of the left and right keys (but leave up and down as it is)
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
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowDown)

    await pressKeysRepeat(clock, renderResult, 'ArrowUp', 1)

    const expectedNavigatorTargetsAfterArrowUp = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowUp)

    // pressing keyboard left and right reorders elements
    await pressKeysRepeat(clock, renderResult, 'ArrowRight', 2)

    const expectedNavigatorTargetsAfterArrowRight = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowRight)

    await pressKeysRepeat(clock, renderResult, 'ArrowLeft', 1)

    const expectedNavigatorTargetsAfterArrowLeft = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowLeft)
  })

  it('pressing the arrow keys reorders in a flow layout with rtl text direction', async () => {
    const TestProjectFlow = TestProject('block', 'block', 'rtl')
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

    // pressing keyboard up and down reorders elements as in ltr case
    await pressKeysRepeat(clock, renderResult, 'ArrowDown', 2)

    const expectedNavigatorTargetsAfterArrowDown = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowDown)

    await pressKeysRepeat(clock, renderResult, 'ArrowUp', 1)

    const expectedNavigatorTargetsAfterArrowUp = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowUp)

    // pressing keyboard left and right reorders elements in the opposite direction than in the ltr case
    await pressKeysRepeat(clock, renderResult, 'ArrowLeft', 2)

    const expectedNavigatorTargetsAfterArrowRight = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowRight)

    await pressKeysRepeat(clock, renderResult, 'ArrowRight', 1)

    const expectedNavigatorTargetsAfterArrowLeft = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowLeft)
  })

  it('pressing the arrow keys reorders in a flex layout', async () => {
    const TestProjectFlexRow = TestProject('block', 'flex', 'ltr', 'row')
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

    // pressing keyboard up and down reorders elements
    await pressKeysRepeat(clock, renderResult, 'ArrowDown', 2)

    const expectedNavigatorTargetsAfterArrowDown = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowDown)

    await pressKeysRepeat(clock, renderResult, 'ArrowUp', 1)

    const expectedNavigatorTargetsAfterArrowUp = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowUp)

    // pressing keyboard left and right reorders elements
    await pressKeysRepeat(clock, renderResult, 'ArrowRight', 2)

    const expectedNavigatorTargetsAfterArrowRight = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowRight)

    await pressKeysRepeat(clock, renderResult, 'ArrowLeft', 1)

    const expectedNavigatorTargetsAfterArrowLeft = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowLeft)
  })

  it('pressing the arrow keys reorders in a flex layout with rtl text direction', async () => {
    const TestProjectFlexRow = TestProject('block', 'flex', 'rtl', 'row')
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

    // pressing keyboard up and down reorders elements
    await pressKeysRepeat(clock, renderResult, 'ArrowDown', 2)

    const expectedNavigatorTargetsAfterArrowDown = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowDown)

    await pressKeysRepeat(clock, renderResult, 'ArrowUp', 1)

    const expectedNavigatorTargetsAfterArrowUp = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowUp)

    // pressing keyboard left and right reorders elements in the opposite direction than in the ltr case
    await pressKeysRepeat(clock, renderResult, 'ArrowLeft', 2)

    const expectedNavigatorTargetsAfterArrowRight = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowRight)

    await pressKeysRepeat(clock, renderResult, 'ArrowRight', 1)

    const expectedNavigatorTargetsAfterArrowLeft = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowLeft)
  })
  it('pressing the arrow keys reorders in a flex column-reverse layout', async () => {
    const TestProjectFlexColumnReverse = TestProject('block', 'flex', 'ltr', 'column-reverse')
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

    // pressing keyboard up and down reorders elements in reverse order
    await pressKeysRepeat(clock, renderResult, 'ArrowUp', 2)

    const expectedNavigatorTargetsAfterArrowDown = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowDown)

    await pressKeysRepeat(clock, renderResult, 'ArrowDown', 1)

    const expectedNavigatorTargetsAfterArrowUp = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowUp)

    // pressing keyboard left and right reorders elements in reverse order
    await pressKeysRepeat(clock, renderResult, 'ArrowLeft', 2)

    const expectedNavigatorTargetsAfterArrowRight = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowRight)

    await pressKeysRepeat(clock, renderResult, 'ArrowRight', 1)

    const expectedNavigatorTargetsAfterArrowLeft = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowLeft)
  })
  it('pressing the arrow keys reorders in a flex column-reverse layout with rtl text direction', async () => {
    const TestProjectFlexColumnReverse = TestProject('block', 'flex', 'ltr', 'column-reverse')
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

    // pressing keyboard up and down reorders elements in reverse order, same is in ltr text direction
    await pressKeysRepeat(clock, renderResult, 'ArrowUp', 2)

    const expectedNavigatorTargetsAfterArrowDown = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowDown)

    await pressKeysRepeat(clock, renderResult, 'ArrowDown', 1)

    const expectedNavigatorTargetsAfterArrowUp = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowUp)

    // pressing keyboard left and right reorders elements in "normal order", because flex reverse and rtl together reverses the directions twice
    await pressKeysRepeat(clock, renderResult, 'ArrowLeft', 2)

    const expectedNavigatorTargetsAfterArrowRight = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowRight)

    await pressKeysRepeat(clock, renderResult, 'ArrowRight', 1)

    const expectedNavigatorTargetsAfterArrowLeft = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowLeft)
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
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowDown)

    await pressKeysRepeat(clock, renderResult, 'ArrowUp', 1)

    const expectedNavigatorTargetsAfterArrowUp = [
      'regular-utopia-storyboard-uid/scene-aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/bbb',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/aaa',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ccc',
      'regular-utopia-storyboard-uid/scene-aaa/app-entity:container/ddd',
    ]
    expect(
      renderResult.getEditorState().derived.visibleNavigatorTargets.map(navigatorEntryToKey),
    ).toEqual(expectedNavigatorTargetsAfterArrowUp)
  })

  AllFragmentLikeTypes.forEach((type) => {
    describe('with fragment-like elements', () => {
      it(`pressing the arrow keys reorders in a flex layout, in a ${type}`, async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(TestProjectWithFragment(type)),
          'await-first-dom-report',
        )

        await renderResult.dispatch(
          [
            selectComponents(
              [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:parent/child1')],
              false,
            ),
          ],
          true,
        )

        // pressing keyboard up and down reorders elements
        await pressKeysRepeat(clock, renderResult, 'ArrowDown', 1)

        const expectedNavigatorTargetsAfterArrowDown: string[] = [
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child1',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child2',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child1', // child1 moves to the right of fragment-like
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child2',
        ]
        expect(getRegularNavigatorTargets(renderResult)).toEqual(
          expectedNavigatorTargetsAfterArrowDown,
        )

        await pressKeysRepeat(clock, renderResult, 'ArrowUp', 1)

        const expectedNavigatorTargetsAfterArrowUp = [
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child1', // child1 moves to the left of fragment-like
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child1',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child2',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child2',
        ]
        expect(getRegularNavigatorTargets(renderResult)).toEqual(
          expectedNavigatorTargetsAfterArrowUp,
        )

        // pressing keyboard left and right reorders elements
        await pressKeysRepeat(clock, renderResult, 'ArrowRight', 1)

        const expectedNavigatorTargetsAfterArrowRight = [
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child1',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child2',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child1', // child1 moves to the right of fragment-like
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child2',
        ]
        expect(getRegularNavigatorTargets(renderResult)).toEqual(
          expectedNavigatorTargetsAfterArrowRight,
        )

        await pressKeysRepeat(clock, renderResult, 'ArrowLeft', 1)

        const expectedNavigatorTargetsAfterArrowLeft = [
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child1', // child1 moves to the left of fragment-like
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child1',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child2',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child2',
        ]
        expect(getRegularNavigatorTargets(renderResult)).toEqual(
          expectedNavigatorTargetsAfterArrowLeft,
        )
      })

      it(`pressing the arrow keys reorders in a flex layout, with a ${type} selected`, async () => {
        const renderResult = await renderTestEditorWithCode(
          makeTestProjectCodeWithSnippet(TestProjectWithFragment(type)),
          'await-first-dom-report',
        )

        await renderResult.dispatch(
          [
            selectComponents(
              [EP.fromString('utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like')],
              false,
            ),
          ],
          true,
        )

        await pressKeysRepeat(clock, renderResult, 'ArrowLeft', 1)

        const expectedNavigatorTargetsAfterArrowLeft = [
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like', // <- the fragment-like element moves to the left of child1
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child1',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child2',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child1',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child2',
        ]
        expect(getRegularNavigatorTargets(renderResult)).toEqual(
          expectedNavigatorTargetsAfterArrowLeft,
        )

        await pressKeysRepeat(clock, renderResult, 'ArrowRight', 1)

        const expectedNavigatorTargetsAfterArrowRight = [
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child1', // <- the fragment-like element moves to the right of child1
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child1',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child2',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child2',
        ]
        expect(getRegularNavigatorTargets(renderResult)).toEqual(
          expectedNavigatorTargetsAfterArrowRight,
        )

        await pressKeysRepeat(clock, renderResult, 'ArrowUp', 1)
        const expectedNavigatorTargetsAfterArrowUp = [
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like', // <- the fragment-like element moves to the left of child1
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child1',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child2',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child1',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child2',
        ]
        expect(getRegularNavigatorTargets(renderResult)).toEqual(
          expectedNavigatorTargetsAfterArrowUp,
        )

        // pressing keyboard up and down reorders elements
        await pressKeysRepeat(clock, renderResult, 'ArrowDown', 1)

        const expectedNavigatorTargetsAfterArrowDown = [
          'utopia-storyboard-uid/scene-aaa',
          'utopia-storyboard-uid/scene-aaa/app-entity',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child1',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like', // <- the fragment-like element moves to the right of child1
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child1',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/fragment-like/inner-fragment/fragment-like-child2',
          'utopia-storyboard-uid/scene-aaa/app-entity:parent/child2',
        ]
        expect(getRegularNavigatorTargets(renderResult)).toEqual(
          expectedNavigatorTargetsAfterArrowDown,
        )
      })
    })
  })
})
