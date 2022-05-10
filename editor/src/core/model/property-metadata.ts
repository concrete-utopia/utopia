import utils from '../../utils/utils'

interface StaticPropertyValue<T> {
  $$propertyType: 'staticvalue'
  value: T
}

interface NodeGraphValue {
  $$propertyType: 'nodegraphvalue'
  node: string
  field: string
}

interface PropGetterValue {
  $$propertyType: 'propsgettervalue'
  propName: string
}
type PropValueMetadata<T> = StaticPropertyValue<T> | NodeGraphValue | PropGetterValue | T
export type PropMetadata<T> = { [P in keyof T]: PropValueMetadata<MaybeMetadata<T[P]>> } & {
  $$metadata: true
}
type MaybeMetadata<T> = T extends { [key: string]: any } ? PropMetadata<T> | T : T

function isMetadata<T>(obj: T | PropMetadata<T>): obj is PropMetadata<T> {
  return obj != null && (obj as any).$$metadata
}

function isPropValueMetadata<T>(obj: T | PropValueMetadata<T>): obj is PropValueMetadata<T> {
  return obj != null && (obj as any).$$propertyType
}

function unwrapPropertyMetadataValue<T>(metadata: PropValueMetadata<T>): T
function unwrapPropertyMetadataValue<T>(metadata: T): T
function unwrapPropertyMetadataValue<T>(metadata: any): any {
  if (isPropValueMetadata(metadata)) {
    switch (metadata.$$propertyType) {
      case 'staticvalue':
        return unwrapPropertyMetadata(metadata.value)
      case 'nodegraphvalue':
      case 'propsgettervalue':
        // TODO this is where we ACTUALLY want to execute these values!!!!
        return null as any
    }
  } else {
    return metadata
  }
}

function unwrapPropertyMetadata<T>(metadata: PropMetadata<T>): T
function unwrapPropertyMetadata<T>(metadata: T): T
function unwrapPropertyMetadata<T>(metadata: any): any {
  if (isMetadata(metadata)) {
    const returnObject: any = {}
    utils.fastForEach(Object.keys(metadata), (key) => {
      if (key !== '$$metadata') {
        returnObject[key] = unwrapPropertyMetadataValue(metadata[key])
      }
    })
    return returnObject
  } else {
    return metadata
  }
}

export function dumbGetter<T, K1 extends keyof T>(obj: PropMetadata<T>, key: K1): T[K1]
export function dumbGetter<T, K1 extends keyof T, K2 extends keyof T[K1]>(
  obj: PropMetadata<T>,
  key1: K1,
  key2?: K2,
): T[K1][K2]
export function dumbGetter<
  T,
  K1 extends keyof T,
  K2 extends keyof T[K1],
  K3 extends keyof T[K1][K2],
>(obj: PropMetadata<T>, key1: K1, key2: K2, key3: K3): T[K1][K2][K3]
export function dumbGetter<
  T,
  K1 extends keyof T,
  K2 extends keyof T[K1],
  K3 extends keyof T[K1][K2],
  K4 extends keyof T[K1][K2][K3],
>(obj: PropMetadata<T>, key1: K1, key2: K2, key3: K3, key4: K4): T[K1][K2][K3][K4]
export function dumbGetter<T, K1 extends keyof T, K2 extends keyof T[K1]>(
  obj: PropMetadata<T>,
  key: K1,
  key2?: K2,
): T[K1][K2]
export function dumbGetter(obj: any, ...keys: string[]): any {
  if (keys.length === 0) {
    return obj
  }
  const value = unwrapPropertyMetadataValue(obj[keys.shift()!])
  return (dumbGetter as any)(value, ...keys)
}
