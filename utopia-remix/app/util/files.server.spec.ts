import { ServerEnvironment } from '../env.server'
import { newS3Client, projectFileS3Key, saveFileToS3 } from './files.server'

describe('files', () => {
  describe('upload to S3', () => {
    it('uploads the file', async () => {
      const client = newS3Client()
      const data = 'hey there'

      await saveFileToS3(client, 'the-project', {
        path: '/some/path/foo.txt',
        data: Buffer.from(data),
      })

      const got = await client
        .getObject({
          Bucket: ServerEnvironment.AWS_S3_BUCKET,
          Key: projectFileS3Key('the-project', '/some/path/foo.txt'),
        })
        .promise()

      expect(got.Body?.toString()).toBe(data)
    })
  })
})
