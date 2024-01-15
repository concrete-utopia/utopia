import React from 'react'
import { colorTheme } from './styles/theme'

type IsolatorProps = {
  onAbandonIntent: () => void
}

export const Isolator: React.FunctionComponent<React.PropsWithChildren<IsolatorProps>> = (
  props,
) => {
  const onAbandonIntent = props.onAbandonIntent
  React.useEffect(() => {
    const handleKeyPressed = (e: any) => {
      if (e.key === 'Escape') {
        onAbandonIntent()
      }
    }
    /* add listener when component mounts */
    document.addEventListener('keydown', handleKeyPressed)

    return () => {
      /* remove listener on unmount */
      document.removeEventListener('keydown', handleKeyPressed)
    }
  }, [onAbandonIntent])

  return (
    <div
      className='isolator'
      style={{
        zIndex: 99999,
        position: 'fixed',
        left: 0,
        top: 0,
        right: 0,
        bottom: 0,
        background: colorTheme.isolator.value,
        transition: 'all .1s ease-in-out',
      }}
      onClick={() => props.onAbandonIntent()}
    >
      {props.children}
    </div>
  )
}
