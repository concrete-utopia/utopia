import type {
  AllowedEnumType,
  ArrayControlDescription,
  BaseControlDescription,
  BasicControlOption,
  BasicControlOptions,
  CheckboxControlDescription,
  ColorControlDescription,
  EulerControlDescription,
  ExpressionControlOption,
  ExpressionInputControlDescription,
  ExpressionPopUpListControlDescription,
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
  let result: ExpressionPopUpListControlDescription = {
    control: 'expression-popuplist',
    options: options,
  }

  mutateControlWithOptions(result, baseOptions)

  return result
}

export function eulerControl(
  options?: PropertyControlsOptions<[number, number, number, string]>,
): EulerControlDescription {
  let result: EulerControlDescription = {
    control: 'euler',
  }
  mutateControlWithOptions(result, options)

  return result
}

export function matrix3Control(
  options?: PropertyControlsOptions<Matrix3>,
): Matrix3ControlDescription {
  let result: Matrix3ControlDescription = {
    control: 'matrix3',
  }
  mutateControlWithOptions(result, options)

  return result
}

export function matrix4Control(
  options?: PropertyControlsOptions<Matrix4>,
): Matrix4ControlDescription {
  let result: Matrix4ControlDescription = {
    control: 'matrix4',
  }

  mutateControlWithOptions(result, options)

  return result
}

export function noControl(options?: PropertyControlsOptions<unknown>): NoneControlDescription {
  let result: NoneControlDescription = {
    control: 'none',
  }
  mutateControlWithOptions(result, options)

  return result
}

export function numberControl(
  unit?: string,
  options?: PropertyControlsOptions<unknown>,
): NumberInputControlDescription {
  let result: NumberInputControlDescription = {
    control: 'number-input',
  }

  if (unit !== undefined) {
    result.unit = unit
  }

  mutateControlWithOptions(result, options)

  return result
}

export function popupListControl(
  options: BasicControlOptions<unknown>,
  baseOptions?: PropertyControlsOptions<AllowedEnumType | BasicControlOption<unknown>>,
): PopUpListControlDescription {
  let result: PopUpListControlDescription = {
    control: 'popuplist',
    options: options,
  }

  mutateControlWithOptions(result, baseOptions)

  return result
}

export function radioControl(
  options: BasicControlOptions<unknown>,
  baseOptions?: PropertyControlsOptions<AllowedEnumType | BasicControlOption<unknown>>,
): RadioControlDescription {
  let result: RadioControlDescription = {
    control: 'radio',
    options: options,
  }

  mutateControlWithOptions(result, baseOptions)

  return result
}

export function sliderControl(
  min: number,
  max: number,
  step: number,
  unit?: string,
  options?: PropertyControlsOptions<unknown>,
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

  mutateControlWithOptions(result, options)

  return result
}

export function stringControl(
  placeholder?: string,
  options?: PropertyControlsOptions<unknown>,
): StringInputControlDescription {
  let result: StringInputControlDescription = {
    control: 'string-input',
  }

  if (placeholder !== undefined) {
    result.placeholder = placeholder
  }

  mutateControlWithOptions(result, options)

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

  mutateControlWithOptions(result, options)

  return result
}

export function styleControl(
  options?: PropertyControlsOptions<unknown>,
): StyleControlsControlDescription {
  let result: StyleControlsControlDescription = {
    control: 'style-controls',
  }
  mutateControlWithOptions(result, options)

  return result
}

export function vector2Control(
  options?: PropertyControlsOptions<Vector2>,
): Vector2ControlDescription {
  let result: Vector2ControlDescription = {
    control: 'vector2',
  }

  mutateControlWithOptions(result, options)

  return result
}

export function vector3Control(
  options?: PropertyControlsOptions<Vector3>,
): Vector3ControlDescription {
  let result: Vector3ControlDescription = {
    control: 'vector3',
  }

  mutateControlWithOptions(result, options)

  return result
}

export function vector4Control(
  options?: PropertyControlsOptions<Vector4>,
): Vector4ControlDescription {
  let result: Vector4ControlDescription = {
    control: 'vector4',
  }

  mutateControlWithOptions(result, options)

  return result
}

export function jsxControl(options?: PropertyControlsOptions<unknown>): JSXControlDescription {
  let result: JSXControlDescription = {
    control: 'jsx',
  }
  mutateControlWithOptions(result, options)

  return result
}

export function arrayControl(
  propertyControl: RegularControlDescription,
  options?: PropertyControlsOptions<unknown>,
): ArrayControlDescription {
  let result: ArrayControlDescription = {
    control: 'array',
    propertyControl: propertyControl,
  }

  mutateControlWithOptions(result, options)

  return result
}

export function fixedSizeArrayControl(
  propertyControl: RegularControlDescription,
  maxCount: number,
  options?: PropertyControlsOptions<unknown>,
): ArrayControlDescription {
  let result: ArrayControlDescription = {
    control: 'array',
    propertyControl: propertyControl,
    maxCount: maxCount,
  }
  mutateControlWithOptions(result, options)

  return result
}

export function objectControl(
  object: {
    [prop: string]: RegularControlDescription
  },
  options?: PropertyControlsOptions<unknown>,
): ObjectControlDescription {
  let result: ObjectControlDescription = {
    control: 'object',
    object: object,
  }

  mutateControlWithOptions(result, options)

  return result
}

export function tupleControl(
  propertyControls: RegularControlDescription[],
  options?: PropertyControlsOptions<unknown>,
): TupleControlDescription {
  let result: TupleControlDescription = {
    control: 'tuple',
    propertyControls: propertyControls,
  }

  mutateControlWithOptions(result, options)

  return result
}

export function unionControl(
  controls: Array<RegularControlDescription>,
  options?: PropertyControlsOptions<unknown>,
): UnionControlDescription {
  let result: UnionControlDescription = {
    control: 'union',
    controls: controls,
  }

  mutateControlWithOptions(result, options)

  return result
}

function mutateControlWithOptions<T, U extends PropertyControlsOptions<T>>(
  control: U,
  options?: PropertyControlsOptions<T>,
) {
  if (options?.defaultValue !== undefined) {
    control.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    control.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    control.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    control.label = options.label
  }
}
