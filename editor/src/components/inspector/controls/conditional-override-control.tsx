import React from 'react'
import { ConditionValue } from '../../../core/shared/element-template'
import { ButtonProps, FlexRow, Icons, Tooltip } from '../../../uuiui'
import { SquareButton } from '../../titlebar/buttons'
import { ControlStatus, ControlStyles } from '../common/control-status'
import { UIGridRow } from '../widgets/ui-grid-row'
import { OptionChainControl, OptionChainOption } from './option-chain-control'

export const ConditionalOverrideControlTestIdPrefix = 'conditional-override-control'
export const ConditionalOverrideControlToggleTestId = 'conditional-override-control-toggle'

export interface ConditionalOverrideControlProps extends ButtonProps {
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  conditionValue: ConditionValue
  setConditionOverride: (override: boolean | null) => void
}

const OverrideControlOptions: Array<OptionChainOption<boolean>> = [
  {
    tooltip: 'Override as True',
    label: 'True',
    value: true,
    forceCallOnSubmitValue: true,
  },
  {
    tooltip: 'Override as False',
    label: 'False',
    value: false,
    forceCallOnSubmitValue: true,
  },
]

export const ConditionalOverrideControl: React.FunctionComponent<
  React.PropsWithChildren<ConditionalOverrideControlProps>
> = (props) => {
  const { controlStatus, controlStyles, setConditionOverride, conditionValue } = props

  const toggleOverride = React.useCallback(() => {
    if (controlStatus === 'overridden') {
      setConditionOverride(null)
    } else if (conditionValue !== 'not-a-conditional') {
      setConditionOverride(conditionValue)
    }
  }, [controlStatus, setConditionOverride, conditionValue])

  return (
    <UIGridRow padded={true} variant='<--------1fr-------->|145px||22px|'>
      Result
      <OptionChainControl
        id={'conditional-override-control'}
        testId={ConditionalOverrideControlTestIdPrefix}
        key={'conditional-override-control'}
        onSubmitValue={props.setConditionOverride}
        value={conditionValue}
        options={OverrideControlOptions}
        controlStatus={controlStatus}
        controlStyles={controlStyles}
      />
      <Tooltip title={'Override'}>
        <SquareButton onClick={toggleOverride} testId={ConditionalOverrideControlToggleTestId}>
          {getPinIcon(controlStatus, controlStyles)}
        </SquareButton>
      </Tooltip>
    </UIGridRow>
  )
}

function getPinIcon(controlStatus: ControlStatus, controlStyles: ControlStyles) {
  return controlStatus === 'overridden' ? (
    <Icons.PinFilled color={controlStyles.iconColor} />
  ) : (
    <Icons.PinRightOutline color={controlStyles.iconColor} />
  )
}
