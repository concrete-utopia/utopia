import React from 'react'

/**
 * This terrible hook is required to make Remix, zustand, and media queries (e.g. the Radix themes and Sprinkles
 * conditions) work fine together.
 *
 * The first render of Remix does not contain the client-side pieces of information, but the mounted renders do.
 * So, this hook returns a flag that when true means that the client is ready to display page content.
 */
export function useIsClientReady() {
  const [ready, setReady] = React.useState(false)
  React.useEffect(() => {
    setReady(true)
  }, [])
  return ready
}
