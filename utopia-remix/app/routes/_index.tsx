import type { MetaFunction } from '@remix-run/node'

export const meta: MetaFunction = () => {
  return [{ title: 'Utopia' }]
}

export default function Index() {
  return (
    <div>
      <div>Welcome to Utopia</div>
    </div>
  )
}
