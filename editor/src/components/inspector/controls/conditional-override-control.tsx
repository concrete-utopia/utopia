import React from 'react'
import { when } from '../../../utils/react-conditionals'
import { ButtonProps, FlexRow, Icons, Tooltip } from '../../../uuiui'
import { SquareButton } from '../../titlebar/buttons'
import { ControlStatus, ControlStyles } from '../common/control-status'
import { ConditionOverride } from '../sections/layout-section/conditional-section'
import { UIGridRow } from '../widgets/ui-grid-row'
import { OptionChainControl, OptionChainOption } from './option-chain-control'

export const ConditionalOverrideControlTestIdPrefix = 'conditional-override-control'
export const ConditionalOverrideControlDisableTestId = 'conditional-override-control-disable'

export interface ConditionalOverrideControlProps extends ButtonProps {
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  conditionOverride: ConditionOverride
  setConditionOverride: (override: boolean | null) => void
}

const OverrideControlOptions: Array<OptionChainOption<boolean>> = [
  {
    tooltip: 'True',
    label: 'True',
    value: true,
  },
  {
    tooltip: 'False',
    label: 'False',
    value: false,
  },
]

export const ConditionalOverrideControl: React.FunctionComponent<
  React.PropsWithChildren<ConditionalOverrideControlProps>
> = (props) => {
  const { controlStatus, controlStyles, setConditionOverride } = props

  const disableOverride = React.useCallback(() => {
    if (controlStatus === 'overridden') {
      setConditionOverride(null)
    }
  }, [controlStatus, setConditionOverride])

  return (
    <UIGridRow
      padded={true}
      variant='<---1fr--->|------172px-------|'
      style={{ color: props.controlStyles.mainColor }}
    >
      Result
      {when(
        controlStatus === 'overridden',
        <Tooltip title={'Override'}>
          <SquareButton onClick={disableOverride} testId={ConditionalOverrideControlDisableTestId}>
            {getPinIcon(controlStatus, controlStyles)}
          </SquareButton>
        </Tooltip>,
      )}
      <FlexRow style={{ flexGrow: 1, gap: 4 }}>
        <OptionChainControl
          id={'conditional-override-control'}
          testId={ConditionalOverrideControlTestIdPrefix}
          key={'conditional-override-control'}
          onSubmitValue={props.setConditionOverride}
          value={props.conditionOverride}
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
