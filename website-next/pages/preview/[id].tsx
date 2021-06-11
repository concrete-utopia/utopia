import * as React from 'react'
import { useRouter } from 'next/router'
import { PreviewPage } from '../../components/preview/preview-page-component'

export default function Preview() {
  const router = useRouter()
  const { id } = router.query

  const projectId = Array.isArray(id) ? id[0] : id

  return <PreviewPage projectId={projectId} />
}
