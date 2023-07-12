export interface ComplexMapValue<K, V> {
  key: K
  value: V
}

export function complexMapValue<K, V>(key: K, value: V): ComplexMapValue<K, V> {
  return {
    key: key,
    value: value,
  }
}

export type ComplexMap<K, V> = { [key: string]: ComplexMapValue<K, V> }

export type KeyToString<K> = (key: K) => string

export function emptyComplexMap<K, V>(): ComplexMap<K, V> {
  return {}
}

export function addToComplexMap<K, V>(
  keyToString: KeyToString<K>,
  map: ComplexMap<K, V>,
  key: K,
  value: V,
): ComplexMap<K, V> {
  return {
    ...map,
    [keyToString(key)]: complexMapValue(key, value),
  }
}

export function getValueFromComplexMap<K, V>(
  keyToString: KeyToString<K>,
  map: ComplexMap<K, V>,
  key: K,
): V | undefined {
  const keyAsString = keyToString(key)
  if (keyAsString in map) {
    // This `in` check proves that the value exists.
    return map[keyAsString]!.value
  } else {
    return undefined
  }
}

export function getKeysFromComplexMap<K, V>(map: ComplexMap<K, V>): ReadonlyArray<K> {
  return Object.values(map).map((entry) => entry.key)
}
