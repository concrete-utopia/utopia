import { OptionChainOption } from './segment-control'
import { OptionControlOptions } from './option-control'
import { SelectControlOptions, SelectOption } from './select-control'
import { SliderControlOptions } from './slider-control'
import { StringControlOptions } from './string-control'
import { ControlStatus, ControlStyles } from '../common/control-status'
import { EmptyInputValue } from '../common/css-utils'

export interface GenericControlOptions {
  tooltip?: React.ReactElement<any> | string
  labelBelow?: string
}

export type OnSubmitValue<T> = (value: T) => void
export type OnSubmitValueOrEmpty<T> = (value: T | EmptyInputValue) => void

export interface ControlProps<T> {
  id: string
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
  controlOptions?:
    | GenericControlOptions
    | OptionControlOptions
    | StringControlOptions
    | SliderControlOptions
    | SelectControlOptions
  onDrag?: (value: T) => void
  allowEditOnDoubleClick?: boolean
  onContextMenu?: (e: { nativeEvent: MouseEvent }) => void
  controlClassName?: string
  htmlFor?: string
  highlightNode?: () => void
  reactSelectComponents?: any
  focus?: boolean
  style?: React.CSSProperties
  onFocus?: (e: React.FocusEvent<HTMLInputElement>) => void
  onBlur?: (e: React.FocusEvent<HTMLInputElement>) => void
}
