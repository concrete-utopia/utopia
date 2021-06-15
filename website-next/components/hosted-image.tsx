import * as React from 'react'

const cdnUrl = process.env.UTOPIA_CDN_URL ?? ''

export function srcToCdn(src: string): string {
  return `${cdnUrl}${src}`
}

type HostedImageProps = React.DetailedHTMLProps<
  React.ImgHTMLAttributes<HTMLImageElement>,
  HTMLImageElement
> & { src: string }

export function HostedImage(props: HostedImageProps) {
  return <img {...props} src={srcToCdn(props.src)} />
}
