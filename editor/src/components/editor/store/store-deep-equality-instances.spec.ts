import { jsxElement, utopiaJSXComponent } from '../../../core/shared/element-template'
import * as TP from '../../../core/shared/template-path'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { addToComplexMap, emptyComplexMap } from '../../../utils/map'
import {
  defaultElementWarnings,
  DerivedState,
  transientCanvasState,
  TransientCanvasState,
  transientFileState,
} from './editor-state'
import {
  DerivedStateKeepDeepEquality,
  TransientCanvasStateKeepDeepEquality,
} from './store-deep-equality-instances'

describe('TransientCanvasStateKeepDeepEquality', () => {
  it('same reference returns the same reference', () => {
    const state: TransientCanvasState = transientCanvasState(
      [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      [TP.instancePath(['scene'], ['aaa', 'ccc'])],
      transientFileState(
        [utopiaJSXComponent('App', false, null, [], jsxElement('div', {}, []), null, false, [])],
        emptyImports(),
      ),
    )

    const result = TransientCanvasStateKeepDeepEquality()(state, state)
    expect(result.value).toBe(state)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const oldState: TransientCanvasState = transientCanvasState(
      [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      [TP.instancePath(['scene'], ['aaa', 'ccc'])],
      transientFileState(
        [utopiaJSXComponent('App', false, null, [], jsxElement('div', {}, []), null, false, [])],
        emptyImports(),
      ),
    )
    const newState: TransientCanvasState = transientCanvasState(
      [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      [TP.instancePath(['scene'], ['aaa', 'ccc'])],
      transientFileState(
        [utopiaJSXComponent('App', false, null, [], jsxElement('div', {}, []), null, false, [])],
        emptyImports(),
      ),
    )

    const result = TransientCanvasStateKeepDeepEquality()(oldState, newState)
    expect(result.value).toBe(oldState)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const oldState: TransientCanvasState = transientCanvasState(
      [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      [TP.instancePath(['scene'], ['aaa', 'ccc'])],
      transientFileState(
        [utopiaJSXComponent('App', false, null, [], jsxElement('span', {}, []), null, false, [])],
        emptyImports(),
      ),
    )
    const newState: TransientCanvasState = transientCanvasState(
      [TP.instancePath(['scene'], ['aaa', 'ddd'])],
      [TP.instancePath(['scene'], ['aaa', 'ccc'])],
      transientFileState(
        [utopiaJSXComponent('App', false, null, [], jsxElement('div', {}, []), null, false, [])],
        emptyImports(),
      ),
    )

    const result = TransientCanvasStateKeepDeepEquality()(oldState, newState)
    expect(result.value).toEqual(newState)
    expect(result.value.selectedViews).toEqual(newState.selectedViews)
    expect(result.value.highlightedViews).toBe(oldState.highlightedViews)
    expect(result.value.fileState).toEqual(newState.fileState)
    expect(result.areEqual).toEqual(false)
  })
})

describe('DerivedStateKeepDeepEquality', () => {
  it('same reference returns the same reference', () => {
    const state: DerivedState = {
      navigatorTargets: [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      visibleNavigatorTargets: [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      canvas: {
        descendantsOfHiddenInstances: [],
        controls: [],
        transientState: transientCanvasState(
          [TP.instancePath(['scene'], ['aaa', 'bbb'])],
          [TP.instancePath(['scene'], ['aaa', 'ccc'])],
          transientFileState(
            [
              utopiaJSXComponent(
                'App',
                false,
                null,
                [],
                jsxElement('div', {}, []),
                null,
                false,
                [],
              ),
            ],
            emptyImports(),
          ),
        ),
      },
      elementWarnings: addToComplexMap(
        TP.toString,
        emptyComplexMap(),
        TP.instancePath(['scene'], ['aaa', 'bbb']),
        defaultElementWarnings,
      ),
    }
    const result = DerivedStateKeepDeepEquality()(state, state)
    expect(result.value).toBe(state)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const oldState: DerivedState = {
      navigatorTargets: [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      visibleNavigatorTargets: [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      canvas: {
        descendantsOfHiddenInstances: [],
        controls: [],
        transientState: transientCanvasState(
          [TP.instancePath(['scene'], ['aaa', 'bbb'])],
          [TP.instancePath(['scene'], ['aaa', 'ccc'])],
          transientFileState(
            [
              utopiaJSXComponent(
                'App',
                false,
                null,
                [],
                jsxElement('div', {}, []),
                null,
                false,
                [],
              ),
            ],
            emptyImports(),
          ),
        ),
      },
      elementWarnings: addToComplexMap(
        TP.toString,
        emptyComplexMap(),
        TP.instancePath(['scene'], ['aaa', 'bbb']),
        defaultElementWarnings,
      ),
    }
    const newState: DerivedState = {
      navigatorTargets: [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      visibleNavigatorTargets: [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      canvas: {
        descendantsOfHiddenInstances: [],
        controls: [],
        transientState: transientCanvasState(
          [TP.instancePath(['scene'], ['aaa', 'bbb'])],
          [TP.instancePath(['scene'], ['aaa', 'ccc'])],
          transientFileState(
            [
              utopiaJSXComponent(
                'App',
                false,
                null,
                [],
                jsxElement('div', {}, []),
                null,
                false,
                [],
              ),
            ],
            emptyImports(),
          ),
        ),
      },
      elementWarnings: addToComplexMap(
        TP.toString,
        emptyComplexMap(),
        TP.instancePath(['scene'], ['aaa', 'bbb']),
        defaultElementWarnings,
      ),
    }
    const result = DerivedStateKeepDeepEquality()(oldState, newState)
    expect(result.value).toBe(oldState)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const oldState: DerivedState = {
      navigatorTargets: [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      visibleNavigatorTargets: [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      canvas: {
        descendantsOfHiddenInstances: [],
        controls: [],
        transientState: transientCanvasState(
          [TP.instancePath(['scene'], ['aaa', 'bbb'])],
          [TP.instancePath(['scene'], ['aaa', 'ccc'])],
          transientFileState(
            [
              utopiaJSXComponent(
                'App',
                false,
                null,
                [],
                jsxElement('div', {}, []),
                null,
                false,
                [],
              ),
            ],
            emptyImports(),
          ),
        ),
      },
      elementWarnings: addToComplexMap(
        TP.toString,
        emptyComplexMap(),
        TP.instancePath(['scene'], ['aaa', 'bbb']),
        defaultElementWarnings,
      ),
    }
    const newState: DerivedState = {
      navigatorTargets: [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      visibleNavigatorTargets: [TP.instancePath(['scene'], ['aaa', 'bbb'])],
      canvas: {
        descendantsOfHiddenInstances: [],
        controls: [],
        transientState: transientCanvasState(
          [TP.instancePath(['scene'], ['aaa', 'ddd'])],
          [TP.instancePath(['scene'], ['aaa', 'ccc'])],
          transientFileState(
            [
              utopiaJSXComponent(
                'App',
                false,
                null,
                [],
                jsxElement('div', {}, []),
                null,
                false,
                [],
              ),
            ],
            emptyImports(),
          ),
        ),
      },
      elementWarnings: addToComplexMap(
        TP.toString,
        emptyComplexMap(),
        TP.instancePath(['scene'], ['aaa', 'bbb']),
        defaultElementWarnings,
      ),
    }
    const result = DerivedStateKeepDeepEquality()(oldState, newState)
    expect(result.value.navigatorTargets).toBe(oldState.navigatorTargets)
    expect(result.value.visibleNavigatorTargets).toBe(oldState.visibleNavigatorTargets)
    expect(result.value.canvas.descendantsOfHiddenInstances).toBe(
      oldState.canvas.descendantsOfHiddenInstances,
    )
    expect(result.value.canvas.controls).toBe(oldState.canvas.controls)
    expect(result.value.canvas.transientState).toEqual(newState.canvas.transientState)
    expect(result.value.elementWarnings).toBe(oldState.elementWarnings)
    expect(result.value).toEqual(newState)
    expect(result.areEqual).toEqual(false)
  })
})
