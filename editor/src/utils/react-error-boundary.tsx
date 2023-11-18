import React from 'react'

interface ErrorBoundaryProps {
  fallback: React.ReactNode
  children?: React.ReactNode
}

interface ErrorBoundaryState {
  hasError: boolean
}

export class ErrorBoundary extends React.Component<ErrorBoundaryProps, ErrorBoundaryState> {
  public state: ErrorBoundaryState = {
    hasError: false,
  }

  public componentDidCatch(error: Error, errorInfo: React.ErrorInfo) {
    this.setState({ hasError: true })
  }

  public render() {
    if (this.state.hasError) {
      // You can render any custom fallback UI
      return this.props.fallback
    }
    return this.props.children
  }
}
