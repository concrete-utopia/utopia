import { motion } from 'framer-motion'
import React from 'react'

export const Spinner = React.memo(
  ({
    className,
    width = 8,
    height = 8,
  }: {
    className?: string
    width?: number
    height?: number
  }) => {
    return (
      <motion.div
        style={{
          width: width,
          height: height,
        }}
        className={className}
        initial={{ rotate: 0 }}
        animate={{ rotate: 100 }}
        transition={{ ease: 'linear', repeatType: 'loop', repeat: Infinity }}
      />
    )
  },
)
Spinner.displayName = 'Spinner'
