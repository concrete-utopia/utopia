import { createSelector } from 'reselect'

interface ExampleStore {
  exNumber: number
  exString: string
}

describe('Reselect Investigation', () => {
  it('selectors are memoized', () => {
    let numberOfTimesCalled = 0
    const selector = createSelector(
      (store: ExampleStore) => store.exNumber,
      (store: ExampleStore) => store.exString,
      (n, s) => {
        numberOfTimesCalled += 1
      },
    )

    const store: ExampleStore = {
      exNumber: 5,
      exString: 'hello',
    }

    selector(store)
    expect(numberOfTimesCalled).toBe(1)

    // calling it two more times will not increase numberOfTimesCalled!
    selector(store)
    selector(store)

    expect(numberOfTimesCalled).toBe(1)
  })

  it('two instances of the same selector are memoized separately', () => {
    let numberOfTimesCalled = { s1: 0, s2: 0 }
    const selectorFn = (selectorKey: 's1' | 's2') =>
      createSelector(
        (store: ExampleStore) => store.exNumber,
        (store: ExampleStore) => store.exString,
        (n, s) => {
          numberOfTimesCalled[selectorKey] += 1
        },
      )

    const selector1 = selectorFn('s1')
    const selector2 = selectorFn('s2')

    const store: ExampleStore = {
      exNumber: 5,
      exString: 'hello',
    }

    selector1(store)
    expect(numberOfTimesCalled.s1).toBe(1)
    expect(numberOfTimesCalled.s2).toBe(0)
    selector2(store)
    expect(numberOfTimesCalled.s1).toBe(1)
    expect(numberOfTimesCalled.s2).toBe(1)

    // calling it two more times will not increase numberOfTimesCalled!
    selector1(store)
    selector1(store)
    selector2(store)
    selector2(store)

    expect(numberOfTimesCalled.s1).toBe(1)
    expect(numberOfTimesCalled.s2).toBe(1)
  })

  it('!!! WARNING: accidentally creating a new will not be memoized properly!!!!!', () => {
    let numberOfTimesCalled = 0
    const selectorFn = () =>
      createSelector(
        (store: ExampleStore) => store.exNumber,
        (store: ExampleStore) => store.exString,
        (n, s) => {
          numberOfTimesCalled += 1
        },
      )

    const store: ExampleStore = {
      exNumber: 5,
      exString: 'hello',
    }

    selectorFn()(store)
    expect(numberOfTimesCalled).toBe(1)
    selectorFn()(store)
    expect(numberOfTimesCalled).toBe(2)
    selectorFn()(store)
    expect(numberOfTimesCalled).toBe(3)
    selectorFn()(store)
    expect(numberOfTimesCalled).toBe(4)

    // uh oh!!! be very careful when craeting a selector with a curry function
  })

  it('selectors that take an argument are memoized', () => {
    let numberOfTimesCalled = 0
    const selector = createSelector(
      (store: ExampleStore) => store.exNumber,
      (store: ExampleStore) => store.exString,
      (store: ExampleStore, argument: { greeting: string }) => argument.greeting,
      (n, s, argument) => {
        numberOfTimesCalled += 1
      },
    )

    const store: ExampleStore = {
      exNumber: 5,
      exString: 'hello',
    }

    const argumentReferentiallyStable = { greeting: 'hi there!' }

    selector(store, argumentReferentiallyStable)
    expect(numberOfTimesCalled).toBe(1)

    // calling it two more times will not increase numberOfTimesCalled!
    selector(store, argumentReferentiallyStable)
    selector(store, argumentReferentiallyStable)

    expect(numberOfTimesCalled).toBe(1)
  })

  it("the argument doesn't need to be referentially stable, it's enough that the selector picking the argument returns a stable value", () => {
    let numberOfTimesCalled = 0
    const selector = createSelector(
      (store: ExampleStore) => store.exNumber,
      (store: ExampleStore) => store.exString,
      (store: ExampleStore, argument: { greeting: string }) => argument.greeting,
      (n, s, argument) => {
        numberOfTimesCalled += 1
      },
    )

    const store: ExampleStore = {
      exNumber: 5,
      exString: 'hello',
    }

    selector(store, { greeting: 'hi there!' })
    expect(numberOfTimesCalled).toBe(1)

    // calling it two more times will not increase numberOfTimesCalled!
    selector(store, { greeting: 'hi there!' })
    selector(store, { greeting: 'hi there!' })

    expect(numberOfTimesCalled).toBe(1)
  })

  it('nested selectors are memoized as expected', () => {
    let numberOfTimesInnerCalled = 0
    let numberOfTimesOuterCalled = 0

    interface ComplexStore {
      outerProp: {
        innerProp: {
          value: string
        }
      }
    }
    const innerSelector = createSelector(
      (store: ComplexStore) => store.outerProp.innerProp,
      (store: ComplexStore, parameter: 'value') => parameter,
      (innerProp, parameter) => {
        numberOfTimesInnerCalled += 1
        // imagine that this is a cheap selector that goes for a narrow state slice
        return innerProp[parameter]
      },
    )

    const outerSelector = createSelector(innerSelector, (selected) => {
      numberOfTimesOuterCalled += 1
      // imagine that this is an expensive selector here!
      return selected
    })

    const referentiallyStableValue = 'hello'

    outerSelector({ outerProp: { innerProp: { value: referentiallyStableValue } } }, 'value')

    expect(numberOfTimesInnerCalled).toBe(1)
    expect(numberOfTimesOuterCalled).toBe(1)

    outerSelector({ outerProp: { innerProp: { value: referentiallyStableValue } } }, 'value')
    expect(numberOfTimesInnerCalled).toBe(2)
    expect(numberOfTimesOuterCalled).toBe(1)

    outerSelector({ outerProp: { innerProp: { value: referentiallyStableValue } } }, 'value')
    expect(numberOfTimesInnerCalled).toBe(3)
    expect(numberOfTimesOuterCalled).toBe(1)

    // the inner selector's memoization prevents the outer selector from re-running
  })
})
