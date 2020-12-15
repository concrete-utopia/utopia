/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import onClickOutside from 'react-onclickoutside'

export interface OnClickOutsideHOCProps {
  onClickOutside?: (e: MouseEvent) => void
}

class OnClickOutsideHOCUnenhanced extends React.Component<OnClickOutsideHOCProps> {
  handleClickOutside(event: MouseEvent) {
    if (this.props.onClickOutside != null) {
      this.props.onClickOutside(event)
    }
  }

  render() {
    return <React.Fragment>{this.props.children}</React.Fragment>
  }
}

export const OnClickOutsideHOC = onClickOutside(OnClickOutsideHOCUnenhanced)
