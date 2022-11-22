/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import onClickOutside from 'react-onclickoutside'

export interface OnClickOutsideHOCProps {
  onClickOutside?: (e: MouseEvent) => void
  outsideClickIgnoreClass?: string
}

class OnClickOutsideHOCUnenhanced extends React.Component<
  React.PropsWithChildren<OnClickOutsideHOCProps>
> {
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
