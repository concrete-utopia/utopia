import styled from '@emotion/styled'
import { transparent } from '../../../uuiui'

export const InspectorControlLabel = styled.label({
  display: 'block',
  marginTop: 5,
  fontSize: 11,
  '&.widget-status-off .label-container': {
    backgroundColor: '#f6f6f6', // TODO theme
    color: transparent.value,
  },
})
