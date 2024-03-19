# Tests involving Liveblocks

The tests in `collaboration-test.spec.tsx` and `commenting.spec.tsx` involve
Utopia features that rely on having an active Liveblocks connection. This doc
outlines the assumptions we had when writing the above tests:

### Using Puppeteer

We didn't want to mock all of Liveblocks and wanted to test the features
end-to-end. Also, Liveblocks recommends using `puppeteer` for end-to-end
testing:
https://liveblocks.io/blog/e2e-tests-with-puppeteer-and-jest-for-multiplayer-apps.
Because of this, we opted to use puppeteer to orchestrate the tests involving
Liveblocks.

### Auth

The tests have to log in before each test. There are several reasons for this:

- comments can only be placed when the user is logged in (this is important for
  the commenting test)
- projects are only saved to the server when the user who created the project is
  logged in (this is important for the collaboration test)

### Fake users

Liveblocks bills based on monthly active users, we wanted to run all the tests
with the same users, so that we don't have to pay a lot for running the tests.
To make this possible, we created the `fakeUser` URL param (which is only
enabled on fish/pizza). If `fakeUser` is passed in the URL, the server
automatically logs in the fake user specified. This way, all interactions in
these tests are made by the same two users. Currently, `fakeUser` has two
possible values (`alice` and `bob`), corresponding to two hardcoded users. Any
other `fakeUser` value will be ignored.
