## How to run the performance tests locally

1. npm install
2. Create a project on localhost and copy its URL.
3. run the performance test like this in the terminal: `EDITOR_URL=<project-url-you-just-copied> npm run`
4. can provide an env var HEADLESS=false to see what the editor is doing: `HEADLESS=false EDITOR_URL=<project-url-you-just-copied> npm run`
5. you will see console output with the frame numbers.
6. for the plotly graph generation, you need a Plotly account username and password, please provide these env vars to the script `PERFORMANCE_GRAPHS_PLOTLY_USERNAME` and `PERFORMANCE_GRAPHS_PLOTLY_API_KEY`
7. to upload images to S3, you need an S3 access key, with these env vars `AWS_REGION`, `AWS_SECRET_ACCESS_KEY`, `AWS_ACCESS_KEY_ID`
