import type {
  AllowedEnumType,
  ArrayControlDescription,
  BasicControlOption,
  BasicControlOptions,
  CheckboxControlDescription,
  ColorControlDescription,
  EulerControlDescription,
  ExpressionControlOption,
  ExpressionInputControlDescription,
  ExpressionPopUpListControlDescription,
  FolderControlDescription,
  HtmlInputControlDescription,
  ImportType,
  JSXControlDescription,
  Matrix3,
  Matrix3ControlDescription,
  Matrix4,
  Matrix4ControlDescription,
  NoneControlDescription,
  NumberInputControlDescription,
  ObjectControlDescription,
  PopUpListControlDescription,
  PropertyControls,
  PropertyControlsOptions,
  RadioControlDescription,
  RegularControlDescription,
  StringInputControlDescription,
  StyleControlsControlDescription,
  TupleControlDescription,
  UnionControlDescription,
  Vector2,
  Vector2ControlDescription,
  Vector3,
  Vector3ControlDescription,
  Vector4,
  Vector4ControlDescription,
} from './property-controls'

export function checkboxControl(): CheckboxControlDescription {
  return {
    control: 'checkbox',
  }
}

export function colorControl(): ColorControlDescription {
  return {
    control: 'color',
  }
}

export function expressionControl(): ExpressionInputControlDescription {
  return {
    control: 'expression-input',
  }
}

export function importStar(source: string, name: string): ImportType {
  return {
    source: source,
    name: name,
    type: 'star',
  }
}
export function importDefault(source: string, name: string): ImportType {
  return {
    source: source,
    name: name,
    type: 'default',
  }
}
export function importNamed(source: string, name: string): ImportType {
  return {
    source: source,
    name: name,
    type: null,
  }
}

export function expression<T>(
  value: T,
  expressionString: string,
  requiredImport?: ImportType,
): ExpressionControlOption<T> {
  let result: ExpressionControlOption<T> = {
    value: value,
    expression: expressionString,
  }

  if (requiredImport !== undefined) {
    result.requiredImport = requiredImport
  }

  return result
}

export function expressionPopupListControl(
  options: ExpressionControlOption<unknown>[],
  baseOptions?: PropertyControlsOptions<unknown>,
): ExpressionPopUpListControlDescription {
  return {
    control: 'expression-popuplist',
    options: options,
    defaultValue: baseOptions?.defaultValue,
    label: baseOptions?.label,
    required: baseOptions?.required,
    visibleByDefault: baseOptions?.visibleByDefault,
  }
}

export function eulerControl(
  options?: PropertyControlsOptions<[number, number, number, string]>,
): EulerControlDescription {
  return {
    control: 'euler',
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function matrix3Control(
  options?: PropertyControlsOptions<Matrix3>,
): Matrix3ControlDescription {
  return {
    control: 'matrix3',
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function matrix4Control(
  options?: PropertyControlsOptions<Matrix4>,
): Matrix4ControlDescription {
  return {
    control: 'matrix4',
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function noControl(options?: PropertyControlsOptions<number>): NoneControlDescription {
  return {
    control: 'none',
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function numberControl(
  unit?: string,
  options?: PropertyControlsOptions<number>,
): NumberInputControlDescription {
  let result: NumberInputControlDescription = {
    control: 'number-input',
  }

  if (unit !== undefined) {
    result.unit = unit
  }

  if (options != null) {
    result.defaultValue = options?.defaultValue
    result.label = options?.label
    result.required = options?.required
    result.visibleByDefault = options?.visibleByDefault
  }

  return result
}

export function popupListControl(
  options: BasicControlOptions<unknown>,
  baseOptions?: PropertyControlsOptions<AllowedEnumType | BasicControlOption<unknown>>,
): PopUpListControlDescription {
  return {
    control: 'popuplist',
    options: options,
    defaultValue: baseOptions?.defaultValue,
    label: baseOptions?.label,
    required: baseOptions?.required,
    visibleByDefault: baseOptions?.visibleByDefault,
  }
}

export function radioControl(
  options: BasicControlOptions<unknown>,
  baseOptions?: PropertyControlsOptions<AllowedEnumType | BasicControlOption<unknown>>,
): RadioControlDescription {
  return {
    control: 'radio',
    options: options,
    defaultValue: baseOptions?.defaultValue,
    label: baseOptions?.label,
    required: baseOptions?.required,
    visibleByDefault: baseOptions?.visibleByDefault,
  }
}

export function sliderControl(
  min: number,
  max: number,
  step: number,
  unit?: string,
  options?: PropertyControlsOptions<number>,
): NumberInputControlDescription {
  let result: NumberInputControlDescription = {
    control: 'number-input',
    min: min,
    max: max,
    step: step,
  }

  if (unit !== undefined) {
    result.unit = unit
  }

  if (options != null) {
    result.defaultValue = options?.defaultValue
    result.label = options?.label
    result.required = options?.required
    result.visibleByDefault = options?.visibleByDefault
  }

  return result
}

export function stringControl(
  placeholder?: string,
  options?: PropertyControlsOptions<string>,
): StringInputControlDescription {
  let result: StringInputControlDescription = {
    control: 'string-input',
  }

  if (placeholder !== undefined) {
    result.placeholder = placeholder
  }

  if (options != null) {
    result.defaultValue = options?.defaultValue
    result.label = options?.label
    result.required = options?.required
    result.visibleByDefault = options?.visibleByDefault
  }

  return result
}

export function htmlControl(
  placeholder?: string,
  options?: PropertyControlsOptions<unknown>,
): HtmlInputControlDescription {
  let result: HtmlInputControlDescription = {
    control: 'html-input',
  }

  if (placeholder !== undefined) {
    result.placeholder = placeholder
  }

  if (options != null) {
    result.defaultValue = options?.defaultValue
    result.label = options?.label
    result.required = options?.required
    result.visibleByDefault = options?.visibleByDefault
  }
  return result
}

export function styleControl(
  options?: PropertyControlsOptions<unknown>,
): StyleControlsControlDescription {
  return {
    control: 'style-controls',
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function vector2Control(
  options?: PropertyControlsOptions<Vector2>,
): Vector2ControlDescription {
  return {
    control: 'vector2',
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function vector3Control(
  options?: PropertyControlsOptions<Vector3>,
): Vector3ControlDescription {
  return {
    control: 'vector3',
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function vector4Control(
  options?: PropertyControlsOptions<Vector4>,
): Vector4ControlDescription {
  return {
    control: 'vector4',
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function jsxControl(options?: PropertyControlsOptions<unknown>): JSXControlDescription {
  return {
    control: 'jsx',
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function arrayControl(
  propertyControl: RegularControlDescription,
  options?: PropertyControlsOptions<Vector2>,
): ArrayControlDescription {
  return {
    control: 'array',
    propertyControl: propertyControl,
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function fixedSizeArrayControl(
  propertyControl: RegularControlDescription,
  maxCount: number,
  options?: PropertyControlsOptions<Vector2>,
): ArrayControlDescription {
  return {
    control: 'array',
    propertyControl: propertyControl,
    maxCount: maxCount,
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function objectControl(
  object: {
    [prop: string]: RegularControlDescription
  },
  options?: PropertyControlsOptions<unknown>,
): ObjectControlDescription {
  return {
    control: 'object',
    object: object,
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function tupleControl(
  propertyControls: RegularControlDescription[],
  options?: PropertyControlsOptions<unknown>,
): TupleControlDescription {
  return {
    control: 'tuple',
    propertyControls: propertyControls,
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function unionControl(
  controls: Array<RegularControlDescription>,
  options?: PropertyControlsOptions<unknown>,
): UnionControlDescription {
  return {
    control: 'union',
    controls: controls,
    defaultValue: options?.defaultValue,
    label: options?.label,
    required: options?.required,
    visibleByDefault: options?.visibleByDefault,
  }
}

export function folderControl(controls: PropertyControls): FolderControlDescription {
  return {
    control: 'folder',
    controls: controls,
  }
}
