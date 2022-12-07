import React from 'react'

interface SlabsProps {
  flexDirection: 'row' | 'column'
}

const Slabs = React.memo<SlabsProps>(({ flexDirection }) => {
  return (
    <div style={{ display: 'flex', flexDirection }}>
      <div style={{ width: 10, height: 5, borderRadius: 2 }} />
      <div style={{ width: 10, height: 7.5, borderRadius: 2 }} />
      <div style={{ width: 10, height: 2.5, borderRadius: 2 }} />
    </div>
  )
})

interface NineBlockControlProps {}

export const NineBlockControl = React.memo<NineBlockControlProps>(() => (
  <>Someday, NineBlockControl will live here</>
))
