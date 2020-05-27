import * as React from 'react'

type IsolatorProps = {
  onAbandonIntent: () => void
}

export const Isolator: React.FunctionComponent<IsolatorProps> = (props) => {
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
        backgroundColor: 'hsla(0,100%,100%, .6)',
        backdropFilter: 'blur(1px)',
      }}
      onClick={() => props.onAbandonIntent()}
      {...props}
    />
  )
}
