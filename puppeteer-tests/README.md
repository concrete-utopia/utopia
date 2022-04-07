## How to run the performance tests locally

1. pnpm install
2. Create a project on localhost and copy its URL.
3. Run the performance test like this in the terminal: `EDITOR_URL=<project-url-you-just-copied> pnpm run performance-test`
4. It's possible to provide a browser path if the bundled one is no use: `BROWSER='google-chrome' EDITOR_URL=<project-url-you-just-copied> pnpm run performance-test`
5. You will see console output with the frame numbers.
6. That's all you need for debugging existing tests!

To debug plotly graphs, you also need a Plotly account username and password, please provide these env vars to the script `PERFORMANCE_GRAPHS_PLOTLY_USERNAME` and `PERFORMANCE_GRAPHS_PLOTLY_API_KEY`

To test uploading images to S3, you need an S3 access key, with these env vars `AWS_REGION`, `AWS_SECRET_ACCESS_KEY`, `AWS_ACCESS_KEY_ID`
