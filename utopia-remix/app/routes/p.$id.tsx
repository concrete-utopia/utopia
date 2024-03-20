import React from 'react'
import type { LoaderFunctionArgs } from '@remix-run/node'
import { loader as projectLoader } from './loaders/editorLoader.server'
import { useLoaderData } from '@remix-run/react'
import ProjectNotFound from '../components/projectNotFound'

export async function loader(args: LoaderFunctionArgs) {
  return projectLoader(args)
}
export default function () {
  const data = useLoaderData() as unknown as { projectId: string | null; userId: string | null }
  return <ProjectNotFound projectId={data.projectId} userId={data.userId} />
}
