import styled from '@emotion/styled'
import { transparent } from 'uuiui'

export const InspectorControlLabelMini = styled.label({
  textAlign: 'center',
  display: 'block',
  marginBottom: 8,
  '&.widget-status-off .label-container': {
    backgroundColor: '#f6f6f6', // TODO theme
    color: transparent.value,
  },
})
