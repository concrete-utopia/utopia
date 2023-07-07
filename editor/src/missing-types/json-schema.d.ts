import type { AnyJson } from './json'

type InspectorControlType = 'string' | 'number' | 'enum' | 'boolean'

export declare interface PropSchema {
  [property: string]: JsonSchema
}

export declare interface JsonSchema {
  $ref?: string
  /////////////////////////////////////////////////
  // Schema Metadata
  /////////////////////////////////////////////////
  /**
   * This is important because it tells refs where
   * the root of the document is located
   */
  id?: string
  /**
   * It is recommended that the meta-schema is
   * included in the root of any JSON Schema
   */
  $schema?: JsonSchema
  /**
   * Title of the schema
   */
  title?: string
  /**
   * Schema description
   */
  description?: string
  /**
   * Default json for the object represented by
   * this schema
   */
  default?: AnyJson
  defaultSpecialForSvg?: AnyJson // gigantic hack to make svg-specific defaults work for now. I pray for my salvation

  readOnly?: boolean

  /////////////////////////////////////////////////
  // Number Validation
  /////////////////////////////////////////////////
  /**
   * The value must be a multiple of the number
   * (e.g. 10 is a multiple of 5)
   */
  multipleOf?: number
  maximum?: number
  /**
   * If true maximum must be > value, >= otherwise
   */
  exclusiveMaximum?: boolean
  minimum?: number
  /**
   * If true minimum must be < value, <= otherwise
   */
  exclusiveMinimum?: boolean

  /////////////////////////////////////////////////
  // String Validation
  /////////////////////////////////////////////////
  maxLength?: number
  minLength?: number
  /**
   * This is a regex string that the value must
   * conform to
   */
  pattern?: string

  /////////////////////////////////////////////////
  // Array Validation
  /////////////////////////////////////////////////
  additionalItems?: boolean | JsonSchema
  items?: JsonSchema // momentum hack: in real jsonschema items can be `JsonSchema | JsonSchema[]` for tuples, but we don't need that and it makes traversal much uglier
  maxItems?: number
  minItems?: number
  uniqueItems?: boolean

  /////////////////////////////////////////////////
  // Object Validation
  /////////////////////////////////////////////////
  maxProperties?: number
  minProperties?: number
  required?: string[]
  additionalProperties?: boolean | JsonSchema
  /**
   * Holds simple JSON Schema definitions for
   * referencing from elsewhere.
   */
  definitions?: { [key: string]: JsonSchema }
  /**
   * The keys that can exist on the object with the
   * json schema that should validate their value
   */
  properties?: { [property: string]: JsonSchema }
  /**
   * The key of this object is a regex for which
   * properties the schema applies to
   */
  patternProperties?: { [pattern: string]: JsonSchema }
  /**
   * If the key is present as a property then the
   * string of properties must also be present.
   * If the value is a JSON Schema then it must
   * also be valid for the object if the key is
   * present.
   */
  dependencies?: { [key: string]: JsonSchema | string[] }

  /////////////////////////////////////////////////
  // Generic
  /////////////////////////////////////////////////
  /**
   * Enumerates the values that this schema can be
   * e.g.
   * {"type": "string",
   *  "enum": ["red", "green", "blue"]}
   */
  enum?: any[]

  /**
   * The name to be presented on screen
   */
  alias?: string

  /**
   * the name of the icon for the property
   */
  icon?: string

  /**
   * The basic type of this schema, can be one of
   * [string, number, object, array, boolean, null]
   * or an array of the acceptable types
   */
  type?: 'string' | 'number' | 'object' | 'array' | 'boolean' | 'null'

  /**
   * Custom number properties
   */
  stepSize?: number
  numberFormatting?: 'percent' | 'pixel' | 'degree' | 'scale'

  /**
   * The entity being validated must not match this schema
   */
  control?: InspectorControlType

  /**
   * Custom property for skipping elements from composite
   */
  excludeFromComposite?: boolean

  /////////////////////////////////////////////////
  // Combining Schemas
  /////////////////////////////////////////////////
  allOf?: JsonSchema[]
  anyOf?: JsonSchema[]
  oneOf?: JsonSchema[]
  /**
   * The entity being validated must not match this schema
   */
  not?: JsonSchema
}
