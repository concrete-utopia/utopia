import React from 'react'
import type { BrowserEnvironment } from '../env.server'

export function useBrowserEnv() {
  const [env, setEnv] = React.useState<BrowserEnvironment | null>(null)
  React.useEffect(() => {
    setEnv(window.ENV)
  }, [])
  return env
}
