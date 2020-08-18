import * as process from 'process'
import * as cp from 'child_process'
import * as path from 'path'

test('test runs', () => {
  process.env['INPUT_MILLISECONDS'] = '500'
  const ip = path.join(__dirname, '..', 'lib', 'main.js')
  expect(
    cp.execSync(`node ${ip}`).toString().includes('google-fonts-file')
  ).toBeTruthy()
})
