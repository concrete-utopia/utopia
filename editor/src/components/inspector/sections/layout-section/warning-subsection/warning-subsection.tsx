import * as React from 'react'

import { betterReactMemo } from 'uuiui-deps'
import { FlexRow, Tooltip } from 'uuiui'
import { useInspectorWarningStatus } from '../../../common/property-path-hooks'
import { WarningIcon } from '../../../../../uuiui/warning-icon'
import { InfoBox } from '../../../../common/notices'

export const WarningSubsection = betterReactMemo('WarningSubsection', (props) => {
  const shouldShowWarning = useInspectorWarningStatus()
  if (shouldShowWarning) {
    return (
      <Tooltip
        title='The inspector only shows values from css-in-js, styled-systems, and emotion. You can use css classes to style content, but they will not show up here.'
        placement='bottom'
      >
        <FlexRow style={{ padding: 8 }}>
          <InfoBox
            level='WARNING'
            message={
              <React.Fragment>
                <WarningIcon color='white' />
                <span
                  style={{ paddingLeft: 10, overflowWrap: 'break-word', whiteSpace: 'initial' }}
                >
                  Layout may be affected <br />
                  by external CSS
                </span>
              </React.Fragment>
            }
          ></InfoBox>
        </FlexRow>
      </Tooltip>
    )
  } else {
    return null
  }
})
