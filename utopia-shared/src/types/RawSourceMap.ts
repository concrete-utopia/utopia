export interface RawSourceMap {
  version: number
  sources: string[]
  names: string[]
  sourceRoot?: string
  sourcesContent?: string[]
  transpiledContentUtopia?: string
  mappings: string
  file: string
}
