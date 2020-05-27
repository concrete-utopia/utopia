export type AnyJson = string | number | boolean | null | JsonArray | JsonMap
export interface JsonMap {
  [key: string]: AnyJson
}
export interface JsonArray extends Array<AnyJson> {}
