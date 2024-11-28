import { css } from '@emotion/react'
import type { CSSProperties, CSSInterpolation } from '@emotion/serialize'

interface CommonSenseUtopiaProps {
  flexGrow?: CSSProperties['flexGrow']
}

export const commonSenseUtopiaLayoutShorthands = (props: CommonSenseUtopiaProps) =>
  css({
    flexGrow: props.flexGrow,
  } as CSSInterpolation)
