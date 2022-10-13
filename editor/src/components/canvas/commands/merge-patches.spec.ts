import { Spec } from 'immutability-helper'
import { mergePatches } from './merge-patches'

describe('Merge immutability-helper patches', () => {
  it('Merge when path is the same', () => {
    const patch1: Spec<any> = {
      a: {
        b: {
          $set: 10,
        },
      },
    }

    const patch2: Spec<any> = {
      a: {
        b: {
          $set: 20,
        },
      },
    }

    const mergedPatches = mergePatches<any>([patch1, patch2])

    expect(mergedPatches).toEqual([patch2])
  })
  it('Do not merge when path is different', () => {
    const patch1: Spec<any> = {
      a: {
        b: {
          $set: 10,
        },
      },
    }

    const patch2: Spec<any> = {
      a: {
        c: {
          $set: 20,
        },
      },
    }

    const mergedPatches = mergePatches<any>([patch1, patch2])

    expect(mergedPatches).toEqual([patch1, patch2])
  })
  it('Merge non-neighbour patches', () => {
    const patch1: Spec<any> = {
      a: {
        b: {
          $set: 10,
        },
      },
    }

    const patch2: Spec<any> = {
      a: {
        c: {
          $set: 20,
        },
      },
    }

    const patch3: Spec<any> = {
      a: {
        b: {
          $set: 20,
        },
      },
    }

    const mergedPatches = mergePatches<any>([patch1, patch2, patch3])

    expect(mergedPatches).toEqual([patch2, patch3])
  })
  it('Merge when newer patch contains previous path and other paths too', () => {
    const patch1: Spec<any> = {
      a: {
        b: {
          $set: 10,
        },
      },
    }

    const patch2: Spec<any> = {
      a: {
        b: {
          $set: 15,
        },
        c: {
          $set: 20,
        },
      },
    }

    const mergedPatches = mergePatches<any>([patch1, patch2])

    expect(mergedPatches).toEqual([patch2])
  })
  it("Do not merge when older patch contains newer's path and other paths too", () => {
    const patch1: Spec<any> = {
      a: {
        b: {
          $set: 15,
        },
        c: {
          $set: 20,
        },
      },
    }

    const patch2: Spec<any> = {
      a: {
        b: {
          $set: 10,
        },
      },
    }

    const mergedPatches = mergePatches<any>([patch1, patch2])

    expect(mergedPatches).toEqual([patch1, patch2])
  })
})
