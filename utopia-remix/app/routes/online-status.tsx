export async function loader() {
  return new Response('Online', { headers: { 'content-type': 'text/plain' } })
}
