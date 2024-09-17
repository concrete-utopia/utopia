import type { OptionChainOption } from './option-chain-control'
import type { DEPRECATEDOptionControlOptions } from './option-control'
import type { DEPRECATEDSelectControlOptions, SelectOption } from './select-control'
import type { DEPRECATEDSliderControlOptions } from './slider-control'
import type { ControlStyles } from '../common/control-styles'
import type { ControlStatus } from '../common/control-status'
import type { UnknownOrEmptyInput, EmptyInputValue } from '../common/css-utils'
import type { StringControlOptions } from './string-control'

export interface DEPRECATEDGenericControlOptions {
  tooltip?: React.ReactElement<any> | string
  labelBelow?: string
}

export type OnSubmitValue<T> = (value: T, transient?: boolean) => void
export type OnSubmitValueOrEmpty<T> = (value: T | EmptyInputValue, transient?: boolean) => void
export type OnSubmitValueOrUnknownOrEmpty<T> = (
  value: UnknownOrEmptyInput<T>,
  transient?: boolean,
) => void

export interface DEPRECATEDControlProps<T> {
  id: string
  testId: string
  key: string
  value: T
  onSubmitValue: OnSubmitValue<T>
  onTransientSubmitValue?: OnSubmitValue<T>
  onForcedSubmitValue?: OnSubmitValue<T>
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  readOnly?: boolean
  selected?: boolean
  options?: ReadonlyArray<SelectOption> | ReadonlyArray<OptionChainOption<T>>
  onUnsetValues?: () => void
  DEPRECATED_controlOptions?:
    | DEPRECATEDGenericControlOptions
    | DEPRECATEDOptionControlOptions
    | StringControlOptions
    | DEPRECATEDSliderControlOptions
    | DEPRECATEDSelectControlOptions
  onDrag?: (value: T) => void
  onContextMenu?: (e: { nativeEvent: MouseEvent }) => void
  controlClassName?: string
  htmlFor?: string
  highlightNode?: () => void
  reactSelectComponents?: any
  focus?: boolean
  style?: React.CSSProperties
  onFocus?: (e: React.FocusEvent<HTMLInputElement>) => void
  onBlur?: (e: React.FocusEvent<HTMLInputElement>) => void
  onDragStart?: () => void
  onDragEnd?: () => void
}

export interface InspectorControlProps {
  style?: React.CSSProperties
  id?: string
  testId: string
  className?: string
  controlStatus?: ControlStatus
  DEPRECATED_labelBelow?: React.ReactChild
  pasteHandler?: boolean
}
