export interface Optic<S, A> {
  forEach: (s: S, f: (a: A) => void) => void
  update: (s: S, modify: (a: A) => A) => S
  compose<A2>(withOptic: Optic<A, A2>): Optic<S, A2>
}

export function makeOptic<S, A>(
  forEach: (s: S, f: (a: A) => void) => void,
  update: (s: S, modify: (a: A) => A) => S,
): Optic<S, A> {
  return {
    forEach,
    update,
    compose<A2>(withOptic: Optic<A, A2>): Optic<S, A2> {
      return compose2Optics(this, withOptic)
    },
  }
}

export function compose2Optics<A, B, C>(first: Optic<A, B>, second: Optic<B, C>): Optic<A, C> {
  return {
    forEach: (a: A, f: (c: C) => void) => {
      first.forEach(a, (b) => {
        second.forEach(b, f)
      })
    },
    update: (a: A, modify: (c: C) => C) => {
      return first.update(a, (b) => {
        return second.update(b, modify)
      })
    },
    compose: function <D>(withOptic: Optic<C, D>): Optic<A, D> {
      return compose2Optics(this, withOptic)
    },
  }
}
