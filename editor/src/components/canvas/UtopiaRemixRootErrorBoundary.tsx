import React from 'react'

import type { Location } from 'react-router'

function UtopiaRemixRootDefaultErrorBoundary({ error }: { error: Error }) {
  return error.stack == null ? (
    <pre
      style={{
        padding: '2rem',
        background: 'hsla(10, 50%, 50%, 0.1)',
        color: 'red',
        overflow: 'auto',
      }}
    >
      {error.stack}
    </pre>
  ) : (
    <pre>Error: {error.message}</pre>
  )
}

type UtopiaRemixRootErrorBoundaryProps = React.PropsWithChildren<{
  location: Location
  error?: Error
}>

interface UtopiaRemixRootErrorBoundaryState {
  error: null | Error
  location: Location
}

export class UtopiaRemixRootErrorBoundary extends React.Component<
  UtopiaRemixRootErrorBoundaryProps,
  UtopiaRemixRootErrorBoundaryState
> {
  constructor(props: UtopiaRemixRootErrorBoundaryProps) {
    super(props)

    this.state = { error: props.error ?? null, location: props.location }
  }

  static getDerivedStateFromError(error: Error) {
    return { error }
  }

  static getDerivedStateFromProps(
    props: UtopiaRemixRootErrorBoundaryProps,
    state: UtopiaRemixRootErrorBoundaryProps,
  ): UtopiaRemixRootErrorBoundaryState {
    if (state.location !== props.location) {
      return { error: props.error ?? null, location: props.location }
    }

    return { error: props.error ?? state.error ?? null, location: state.location }
  }

  render() {
    if (this.state.error != null) {
      return <UtopiaRemixRootDefaultErrorBoundary error={this.state.error} />
    } else {
      return this.props.children
    }
  }
}
