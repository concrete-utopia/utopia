import type { LoaderFunctionArgs } from '@remix-run/node'
import { loader as projectLoader } from './loaders/editorLoader.server'

export async function loader(args: LoaderFunctionArgs) {
  return projectLoader(args)
}
