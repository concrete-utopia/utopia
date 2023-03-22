import React from 'react'
import { ConditionValue } from '../../../core/shared/element-template'
import { when } from '../../../utils/react-conditionals'
import { ButtonProps, FlexRow, Icons, Tooltip, useColorTheme } from '../../../uuiui'
import { SquareButton } from '../../titlebar/buttons'
import { ControlStatus, ControlStyles } from '../common/control-status'
import { UIGridRow } from '../widgets/ui-grid-row'
import { OptionChainControl, OptionChainOption } from './option-chain-control'

export const ConditionalOverrideControlTestIdPrefix = 'conditional-override-control'
export const ConditionalOverrideControlDisableTestId = 'conditional-override-control-disable'

export interface ConditionalOverrideControlProps extends ButtonProps {
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  conditionValue: ConditionValue
  setConditionOverride: (override: boolean | null) => void
}

const OverrideControlOptions: Array<OptionChainOption<boolean>> = [
  {
    tooltip: 'True',
    label: 'True',
    value: true,
    forceCallOnSubmitValue: true,
  },
  {
    tooltip: 'False',
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
    <UIGridRow padded={true} variant='<---1fr--->|------172px-------|'>
      Result
      <FlexRow>
        <Tooltip title={'Override'}>
          <SquareButton onClick={toggleOverride} testId={ConditionalOverrideControlDisableTestId}>
            {getPinIcon(controlStatus, controlStyles)}
          </SquareButton>
        </Tooltip>
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
      </FlexRow>
    </UIGridRow>
  )
}

function getPinIcon(controlStatus: ControlStatus, controlStyles: ControlStyles) {
  return controlStatus === 'overridden' ? (
    <Icons.PinFilled color={controlStyles.iconColor} />
  ) : (
    <Icons.PinLeftFilled color={controlStyles.iconColor} />
  )
}
