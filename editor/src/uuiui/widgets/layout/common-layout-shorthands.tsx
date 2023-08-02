import { css } from '@emotion/react'
import type { CSSProperties } from '@emotion/serialize'

interface CommonSenseUtopiaProps {
  flexGrow?: CSSProperties['flexGrow']
}

export const commonSenseUtopiaLayoutShorthands = (props: CommonSenseUtopiaProps) =>
  css({
    flexGrow: props.flexGrow,
  })
