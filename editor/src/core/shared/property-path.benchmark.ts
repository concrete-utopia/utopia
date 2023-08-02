import * as Benny from 'benny'
import type { PropertyPath } from './project-file-types'
import * as PP from './property-path'

export async function benchmarkPropertyPathFunction(): Promise<void> {
  await Benny.suite(
    'creating a property path, same path repeated',
    Benny.add('create', () => {
      PP.clearPropertyPathCache()
      return () => {
        PP.create('step-1', 3)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'creating a property path, same path repeated', details: true }),
  )
  PP.clearPropertyPathCache()
  await Benny.suite(
    'creating a property path, new path each time',
    Benny.add('create', () => {
      PP.clearPropertyPathCache()
      let counter: number = 3
      return () => {
        PP.create('step-2', counter++)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'creating a property path, new path each time', details: true }),
  )
  PP.clearPropertyPathCache()
  await Benny.suite(
    'creating a property path from an array, same path repeated',
    Benny.add('createFromArray', () => {
      PP.clearPropertyPathCache()
      return () => {
        PP.createFromArray(['step-3', 'step-4'])
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({
      file: 'creating a property path from an array, same path repeated',
      details: true,
    }),
  )
  PP.clearPropertyPathCache()
  await Benny.suite(
    'creating a property path from an array, new path each time',
    Benny.add('createFromArray', () => {
      PP.clearPropertyPathCache()
      let counter: number = 3
      return () => {
        PP.createFromArray(['step-4', counter++])
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({
      file: 'creating a property path from an array, new path each time',
      details: true,
    }),
  )
  PP.clearPropertyPathCache()
  await Benny.suite(
    'toString a property path, same path repeated',
    Benny.add('toString', () => {
      PP.clearPropertyPathCache()
      const path = PP.create('step-5', 3)
      return () => {
        PP.toString(path)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'toString a property path, same path repeated', details: true }),
  )
  PP.clearPropertyPathCache()
  await Benny.suite(
    'toString a property path, new path each time',
    Benny.add('toString', () => {
      PP.clearPropertyPathCache()
      let counter: number = 0
      return () => {
        // Includes the creation, which will distort things a little, but the alternative
        // is construction an array with a bajillion paths in it.
        PP.toString(PP.create('step-6', counter))
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({
      file: 'creating a property path from an array, new path each time',
      details: true,
    }),
  )
  PP.clearPropertyPathCache()
  await Benny.suite(
    'pathsEqual, same instance',
    Benny.add('pathsEqual', () => {
      PP.clearPropertyPathCache()
      const path = PP.create('step-7', 3)
      return () => {
        PP.pathsEqual(path, path)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'pathsEqual, same instance', details: true }),
  )
  PP.clearPropertyPathCache()
  await Benny.suite(
    'pathsEqual, same path different instance',
    Benny.add('pathsEqual', () => {
      PP.clearPropertyPathCache()
      const path1: PropertyPath = {
        propertyElements: ['step-8', 'path'],
      }
      const path2: PropertyPath = {
        propertyElements: ['step-8', 'path'],
      }
      return () => {
        PP.pathsEqual(path1, path2)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'pathsEqual, same path different instance', details: true }),
  )
  PP.clearPropertyPathCache()
  await Benny.suite(
    'pathsEqual, different paths',
    Benny.add('pathsEqual', () => {
      PP.clearPropertyPathCache()
      const path1 = PP.create('step-9', 'first')
      const path2 = PP.create('step-9', 'second')
      return () => {
        PP.pathsEqual(path1, path2)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'pathsEqual, different paths', details: true }),
  )
  PP.clearPropertyPathCache()
}
