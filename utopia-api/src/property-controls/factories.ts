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
  let result: ExpressionPopUpListControlDescription = {
    control: 'expression-popuplist',
    options: options,
  }

  if (baseOptions?.defaultValue !== undefined) {
    result.defaultValue = baseOptions.defaultValue
  }

  if (baseOptions?.required !== undefined) {
    result.required = baseOptions.required
  }

  if (baseOptions?.visibleByDefault !== undefined) {
    result.visibleByDefault = baseOptions.visibleByDefault
  }

  if (baseOptions?.label !== undefined) {
    result.label = baseOptions.label
  }

  return result
}

export function eulerControl(
  options?: PropertyControlsOptions<[number, number, number, string]>,
): EulerControlDescription {
  let result: EulerControlDescription = {
    control: 'euler',
  }
  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
}

export function matrix3Control(
  options?: PropertyControlsOptions<Matrix3>,
): Matrix3ControlDescription {
  let result: Matrix3ControlDescription = {
    control: 'matrix3',
  }
  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
}

export function matrix4Control(
  options?: PropertyControlsOptions<Matrix4>,
): Matrix4ControlDescription {
  let result: Matrix4ControlDescription = {
    control: 'matrix4',
  }

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
}

export function noControl(options?: PropertyControlsOptions<number>): NoneControlDescription {
  let result: NoneControlDescription = {
    control: 'none',
  }
  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
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

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

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

  if (baseOptions?.defaultValue !== undefined) {
    result.defaultValue = baseOptions.defaultValue
  }

  if (baseOptions?.required !== undefined) {
    result.required = baseOptions.required
  }

  if (baseOptions?.visibleByDefault !== undefined) {
    result.visibleByDefault = baseOptions.visibleByDefault
  }

  if (baseOptions?.label !== undefined) {
    result.label = baseOptions.label
  }

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

  if (baseOptions?.defaultValue !== undefined) {
    result.defaultValue = baseOptions.defaultValue
  }

  if (baseOptions?.required !== undefined) {
    result.required = baseOptions.required
  }

  if (baseOptions?.visibleByDefault !== undefined) {
    result.visibleByDefault = baseOptions.visibleByDefault
  }

  if (baseOptions?.label !== undefined) {
    result.label = baseOptions.label
  }

  return result
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

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
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

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
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

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
}

export function styleControl(
  options?: PropertyControlsOptions<unknown>,
): StyleControlsControlDescription {
  let result: StyleControlsControlDescription = {
    control: 'style-controls',
  }
  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
}

export function vector2Control(
  options?: PropertyControlsOptions<Vector2>,
): Vector2ControlDescription {
  let result: Vector2ControlDescription = {
    control: 'vector2',
  }

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
}

export function vector3Control(
  options?: PropertyControlsOptions<Vector3>,
): Vector3ControlDescription {
  let result: Vector3ControlDescription = {
    control: 'vector3',
  }

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
}

export function vector4Control(
  options?: PropertyControlsOptions<Vector4>,
): Vector4ControlDescription {
  let result: Vector4ControlDescription = {
    control: 'vector4',
  }

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
}

export function jsxControl(options?: PropertyControlsOptions<unknown>): JSXControlDescription {
  let result: JSXControlDescription = {
    control: 'jsx',
  }
  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
}

export function arrayControl(
  propertyControl: RegularControlDescription,
  options?: PropertyControlsOptions<Vector2>,
): ArrayControlDescription {
  let result: ArrayControlDescription = {
    control: 'array',
    propertyControl: propertyControl,
  }

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
}

export function fixedSizeArrayControl(
  propertyControl: RegularControlDescription,
  maxCount: number,
  options?: PropertyControlsOptions<Vector2>,
): ArrayControlDescription {
  let result: ArrayControlDescription = {
    control: 'array',
    propertyControl: propertyControl,
    maxCount: maxCount,
  }
  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

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

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

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

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

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

  if (options?.defaultValue !== undefined) {
    result.defaultValue = options.defaultValue
  }

  if (options?.required !== undefined) {
    result.required = options.required
  }

  if (options?.visibleByDefault !== undefined) {
    result.visibleByDefault = options.visibleByDefault
  }

  if (options?.label !== undefined) {
    result.label = options.label
  }

  return result
}

export function folderControl(controls: PropertyControls): FolderControlDescription {
  return {
    control: 'folder',
    controls: controls,
  }
}

function mutateControlWithOptions<T extends BaseControlDescription>(
  control: T,
  options: PropertyControlsOptions<T>,
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
