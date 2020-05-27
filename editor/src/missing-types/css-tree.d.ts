export namespace Syntax {
  export interface AtKeyword {
    type: 'AtKeyword'
    name: string
  }

  export interface Comma {
    type: 'Comma'
  }
  export interface Function {
    type: 'Function'
    name: string
  }
  export interface Group {
    type: 'Group'
    terms: Array<Record<string, any>>
    combinator: string
    disallowEmpty: boolean
    explicit: boolean
  }
  export interface Keyword<T extends string = string> {
    type: 'Keyword'
    name: T
  }
  export interface Multiplier {
    type: 'Multiplier'
    comma: boolean
    min: number
    max: number
    term: Record<string, any>
  }

  export interface Property {
    type: 'Property'
    name: string
  }

  export interface Range {
    type: 'Range'
    min: number | null
    max: number | null
  }

  export interface StringSyntax {
    type: 'String'
    value: string
  }

  export interface Token {
    type: 'Token'
    value: string
  }
  export interface Type {
    type: 'Type'
    name: string
    opts: Record<string, any> | null
  }

  export type SyntaxItem =
    | AtKeyword
    | Comma
    | Function
    | Group
    | Keyword
    | Multiplier
    | Property
    | Range
    | StringSyntax
    | Token
    | Type
}
