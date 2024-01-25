import * as React from 'react'
import { STATIC_BASE_URL } from './common/env-vars'

export function srcToCdn(src: string): string {
  return `${STATIC_BASE_URL}${src}`
}

type HostedImageProps = React.DetailedHTMLProps<
  React.ImgHTMLAttributes<HTMLImageElement>,
  HTMLImageElement
> & { src: string }

export function HostedImage(props: HostedImageProps) {
  return <img {...props} src={srcToCdn(props.src)} />
}
