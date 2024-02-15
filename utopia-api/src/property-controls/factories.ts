import type {
  ArrayControlDescription,
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
  Matrix3ControlDescription,
  Matrix4ControlDescription,
  NoneControlDescription,
  NumberInputControlDescription,
  ObjectControlDescription,
  PopUpListControlDescription,
  PropertyControls,
  RadioControlDescription,
  RegularControlDescription,
  StringInputControlDescription,
  StyleControlsControlDescription,
  TupleControlDescription,
  UnionControlDescription,
  Vector2ControlDescription,
  Vector3ControlDescription,
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
): ExpressionPopUpListControlDescription {
  return {
    control: 'expression-popuplist',
    options: options,
  }
}

export function eulerControl(): EulerControlDescription {
  return {
    control: 'euler',
  }
}

export function matrix3Control(): Matrix3ControlDescription {
  return {
    control: 'matrix3',
  }
}

export function matrix4Control(): Matrix4ControlDescription {
  return {
    control: 'matrix4',
  }
}

export function noControl(): NoneControlDescription {
  return {
    control: 'none',
  }
}

export function numberControl(unit?: string): NumberInputControlDescription {
  let result: NumberInputControlDescription = {
    control: 'number-input',
  }

  if (unit !== undefined) {
    result.unit = unit
  }

  return result
}

export function popupListControl(
  options: BasicControlOptions<unknown>,
): PopUpListControlDescription {
  return {
    control: 'popuplist',
    options: options,
  }
}

export function radioControl(options: BasicControlOptions<unknown>): RadioControlDescription {
  return {
    control: 'radio',
    options: options,
  }
}

export function sliderControl(
  min: number,
  max: number,
  step: number,
  unit?: string,
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

  return result
}

export function stringControl(placeholder?: string): StringInputControlDescription {
  let result: StringInputControlDescription = {
    control: 'string-input',
  }

  if (placeholder !== undefined) {
    result.placeholder = placeholder
  }

  return result
}

export function htmlControl(placeholder?: string): HtmlInputControlDescription {
  let result: HtmlInputControlDescription = {
    control: 'html-input',
  }

  if (placeholder !== undefined) {
    result.placeholder = placeholder
  }

  return result
}

export function styleControl(): StyleControlsControlDescription {
  return {
    control: 'style-controls',
  }
}

export function vector2Control(): Vector2ControlDescription {
  return {
    control: 'vector2',
  }
}

export function vector3Control(): Vector3ControlDescription {
  return {
    control: 'vector3',
  }
}

export function vector4Control(): Vector4ControlDescription {
  return {
    control: 'vector4',
  }
}

export function arrayControl(propertyControl: RegularControlDescription): ArrayControlDescription {
  return {
    control: 'array',
    propertyControl: propertyControl,
  }
}

export function fixedSizeArrayControl(
  propertyControl: RegularControlDescription,
  maxCount: number,
): ArrayControlDescription {
  return {
    control: 'array',
    propertyControl: propertyControl,
    maxCount: maxCount,
  }
}

export function objectControl(object: {
  [prop: string]: RegularControlDescription
}): ObjectControlDescription {
  return {
    control: 'object',
    object: object,
  }
}

export function tupleControl(
  propertyControls: RegularControlDescription[],
): TupleControlDescription {
  return {
    control: 'tuple',
    propertyControls: propertyControls,
  }
}

export function unionControl(controls: Array<RegularControlDescription>): UnionControlDescription {
  return {
    control: 'union',
    controls: controls,
  }
}

export function folderControl(controls: PropertyControls): FolderControlDescription {
  return {
    control: 'folder',
    controls: controls,
  }
}
