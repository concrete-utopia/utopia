import type { CSSProperties } from 'react'

// these fields are shared among all RegularControlDescription. the helper function getControlSharedFields makes sure the types line up
// Ensure that the fields are also added to the object within `getControlSharedFields` for that typechecking.
interface ControlBaseFields {
  control: RegularControlType
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: unknown
}

// Base Level Controls

export type BaseControlType =
  | 'checkbox'
  | 'color'
  | 'euler'
  | 'expression-input'
  | 'expression-popuplist'
  | 'matrix3'
  | 'matrix4'
  | 'none'
  | 'number-input'
  | 'popuplist'
  | 'radio'
  | 'string-input'
  | 'html-input'
  | 'style-controls'
  | 'vector2'
  | 'vector3'
  | 'vector4'
  | 'jsx'

export interface CheckboxControlDescription {
  control: 'checkbox'
  label?: string
  visibleByDefault?: boolean
  disabledTitle?: string
  enabledTitle?: string
  required?: boolean
  defaultValue?: unknown
}

export interface ColorControlDescription {
  control: 'color'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: unknown
}

export type AllowedEnumType = string | boolean | number | undefined | null
export interface BasicControlOption<T> {
  value: T
  label: string
}

export type BasicControlOptions<T> = AllowedEnumType[] | BasicControlOption<T>[]

export interface PopUpListControlDescription {
  control: 'popuplist'
  label?: string
  visibleByDefault?: boolean
  options: BasicControlOptions<unknown>
  required?: boolean
  defaultValue?: AllowedEnumType | BasicControlOption<unknown>
}

export interface ImportType {
  source: string // importSource
  name: string | null
  type: 'star' | 'default' | null
}

export interface ExpressionControlOption<T> {
  value: T
  expression: string
  label?: string
  requiredImport?: ImportType
}

export interface ExpressionPopUpListControlDescription {
  control: 'expression-popuplist'
  label?: string
  visibleByDefault?: boolean
  options: ExpressionControlOption<unknown>[]
  required?: boolean
  defaultValue?: unknown
}

export interface EulerControlDescription {
  control: 'euler'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: [number, number, number, string]
}

export interface NoneControlDescription {
  control: 'none'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: unknown
}

// prettier-ignore
export type Matrix3 = [
  number, number, number,
  number, number, number,
  number, number, number,
]

export interface Matrix3ControlDescription {
  control: 'matrix3'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: Matrix3
}

// prettier-ignore
export type Matrix4 = [
  number, number, number, number,
  number, number, number, number,
  number, number, number, number,
  number, number, number, number,
]

export interface Matrix4ControlDescription {
  control: 'matrix4'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: Matrix4
}

export interface NumberInputControlDescription {
  control: 'number-input'
  label?: string
  visibleByDefault?: boolean
  max?: number
  min?: number
  unit?: string
  step?: number
  displayStepper?: boolean
  required?: boolean
  defaultValue?: unknown
}

export interface RadioControlDescription {
  control: 'radio'
  label?: string
  visibleByDefault?: boolean
  options: BasicControlOptions<unknown>
  required?: boolean
  defaultValue?: AllowedEnumType | BasicControlOption<unknown>
}

export interface ExpressionInputControlDescription {
  control: 'expression-input'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: unknown
}

export interface StringInputControlDescription {
  control: 'string-input'
  label?: string
  visibleByDefault?: boolean
  placeholder?: string
  obscured?: boolean
  required?: boolean
  defaultValue?: unknown
}

export interface HtmlInputControlDescription {
  control: 'html-input'
  label?: string
  visibleByDefault?: boolean
  placeholder?: string
  obscured?: boolean
  required?: boolean
  defaultValue?: unknown
}

export interface StyleControlsControlDescription {
  control: 'style-controls'
  label?: string
  visibleByDefault?: boolean
  placeholder?: CSSProperties
  required?: boolean
  defaultValue?: unknown
}

export type Vector2 = [number, number]

export interface Vector2ControlDescription {
  control: 'vector2'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: Vector2
}

export type Vector3 = [number, number, number]

export interface Vector3ControlDescription {
  control: 'vector3'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: Vector3
}

export type Vector4 = [number, number, number, number]

export interface Vector4ControlDescription {
  control: 'vector4'
  label?: string
  visibleByDefault?: boolean
  required?: boolean
  defaultValue?: Vector4
}

export interface JSXControlDescription {
  control: 'jsx'
  label?: string
  visibleByDefault?: boolean
  renders?: Array<RendersComponent>
  placeholder?: PlaceholderSpec
  required?: boolean
  defaultValue?: unknown
}

export type BaseControlDescription =
  | CheckboxControlDescription
  | ColorControlDescription
  | ExpressionInputControlDescription
  | ExpressionPopUpListControlDescription
  | EulerControlDescription
  | NoneControlDescription
  | Matrix3ControlDescription
  | Matrix4ControlDescription
  | NumberInputControlDescription
  | RadioControlDescription
  | PopUpListControlDescription
  | StringInputControlDescription
  | HtmlInputControlDescription
  | StyleControlsControlDescription
  | Vector2ControlDescription
  | Vector3ControlDescription
  | Vector4ControlDescription
  | JSXControlDescription

export type PropertyControlsOptions<T> = Omit<ControlBaseFields, 'control'> & { defaultValue?: T }

// Higher Level Controls

export type HigherLevelControlType = 'array' | 'tuple' | 'object' | 'union'
export type RegularControlType = BaseControlType | HigherLevelControlType
export type ControlType = RegularControlType | 'folder'

export interface ArrayControlDescription {
  control: 'array'
  label?: string
  visibleByDefault?: boolean
  propertyControl: RegularControlDescription
  maxCount?: number
  required?: boolean
  defaultValue?: unknown
}

export interface ObjectControlDescription {
  control: 'object'
  label?: string
  visibleByDefault?: boolean
  object: { [prop: string]: RegularControlDescription }
  required?: boolean
  defaultValue?: unknown
}

export interface UnionControlDescription {
  control: 'union'
  label?: string
  visibleByDefault?: boolean
  controls: Array<RegularControlDescription>
  required?: boolean
  defaultValue?: unknown
}
export interface TupleControlDescription {
  control: 'tuple'
  label?: string
  visibleByDefault?: boolean
  propertyControls: RegularControlDescription[]
  required?: boolean
  defaultValue?: unknown
}

export interface FolderControlDescription {
  control: 'folder'
  label?: string
  controls: PropertyControls
}

export type HigherLevelControlDescription =
  | ArrayControlDescription
  | ObjectControlDescription
  | TupleControlDescription
  | UnionControlDescription

export type RegularControlDescription = BaseControlDescription | HigherLevelControlDescription

// with any changes to this or the component types.
export type ControlDescription = RegularControlDescription | FolderControlDescription

export function isBaseControlDescription(
  control: ControlDescription,
): control is BaseControlDescription {
  switch (control.control) {
    case 'checkbox':
    case 'color':
    case 'euler':
    case 'expression-input':
    case 'expression-popuplist':
    case 'matrix3':
    case 'matrix4':
    case 'none':
    case 'number-input':
    case 'popuplist':
    case 'radio':
    case 'string-input':
    case 'html-input':
    case 'style-controls':
    case 'vector2':
    case 'vector3':
    case 'vector4':
    case 'jsx':
      return true
    case 'array':
    case 'object':
    case 'tuple':
    case 'union':
    case 'folder':
      return false
    default:
      const _exhaustiveCheck: never = control
      throw new Error(`Unhandled controls ${JSON.stringify(control)}`)
  }
}

export function isHigherLevelControlDescription(
  control: ControlDescription,
): control is HigherLevelControlDescription {
  return !isBaseControlDescription(control)
}

type AdditionalImports = string | string[]

export type ComponentVariant =
  // if only the component name is specified, and the element is not an
  // intrinsic element, we try to infer the necessary info from the existing
  // component registration. If we can't, a validation error is shown
  | { component: string }

  // detailed description with the path to import the element from and component
  // variants
  | {
      imports?: AdditionalImports
      label: string
      code: string
    }

export type RendersComponent =
  // if only the component name is specified, and the element is not an
  // intrinsic element, we try to infer the necessary info from the existing
  // component registration. If we can't, a validation error is shown
  | { component: string }
  // detailed description with the path to import the element from and component
  // variants
  | {
      imports?: AdditionalImports
      variant: ComponentVariant | ComponentVariant[]
    }

export interface ControlDescriptionFilter {
  type: 'filter'
  allow: Record<string, ControlDescription>
}
export interface ControlDescriptionInfer {
  type: 'infer'
  properties: Record<string, ControlDescription>
}

export type ControlDescriptionSpec = ControlDescriptionFilter | ControlDescriptionInfer

export type InspectorSection = 'style' | 'classname' | 'text' | 'layout'

export type InspectorSpec =
  // inspector sections to show when the component is selected
  | { allow: InspectorSection[] }
  // inspector sections to hide when the component is selected
  | { suppress: InspectorSection[] }

export type AutoFocus = 'always' | 'never'

export type PropertyControls = Record<string, ControlDescription>

export type PlaceholderSpec =
  // a placeholder that inserts a span with that wraps `contents`. This will be
  // editable with the text editor
  | { type: 'text'; contents: string }
  // inserts a `div` with the specified width and height
  | { type: 'spacer'; width: number; height: number }

export interface ChildrenSpec {
  // specifies what component(s) are preferred.
  // `undefined`: any component can be rendered
  // `'text'`: means only JSX text is accepted
  // `RendersComponent`: detailed spec
  renders?: 'text' | RendersComponent | RendersComponent[]

  // specifies the placeholder to use
  placeholder?: PlaceholderSpec
}

export interface ComponentToRegister {
  // The component to register, passed as the component definition
  component: any

  // nothing annotated: detect
  // some/all props: do not detect
  // specific flag: do the inference
  properties?: ControlDescriptionSpec | PropertyControls

  // inspector-related configuration
  // by default, nothing is changed in the inspector
  inspector?: InspectorSpec

  // speciies whether the component should be autofocused in the navigator
  autofocus?: AutoFocus

  // specifies how the `children` prop should be used
  // `undefined`: Utopia assumes that the component doesn't support children
  // `'supported'`: any child element or elements are supported
  // `ChildrenSpec`: a detailed description of which element(s) can be used as children
  children?: 'supported' | ChildrenSpec

  // lists templates to insert
  // if left undefined, any empty element will be inserted
  variants?: ComponentVariant[]
}

export type ComponentRegistrations = {
  // registrations are grouped by module name...
  [moduleName: string]: {
    // ...and component name, until we can reliably infer this
    [componentName: string]: ComponentToRegister
  }
}
