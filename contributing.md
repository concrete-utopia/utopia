# Contributing to Utopia

Our goal is to craft an open and welcoming community here, which is why we have created a [contributor covenant](contributor-covenant.md). To help us meet that goal, we ask that any contributors please follow the guidelines set out in that, and below, and we will do our best to be as responsive and helpful as possible. After all, we want any and all contributors to feel valued.

## Looking for help?

If you need help, please reach out to us on our [Discord](https://discord.gg/NEEnPKCgzC), in the `#help-me` channel.

## Reporting an issue

If you find a bug, please do report it. We ask that you first run a quick search to see if you can already find an existing bug, and if so please use a thumbs up reaction to upvote it, and add any relevant comments if they will help us to understand / solve the issue.

When reporting a new issue, please (where relevant) include sample code (or even a link to an existing project), reproduction steps (where known), and ideally a video (especially for interaction bugs). The more information you can provide, the better the chance of us being able to trace the cause.

## Developer guidelines

Most of our guidelines are automatically enforced via our use of prettier and eslint as part of our pre-commit hooks. We also would like our contributors to adhere to the following:

- Commit messages should be concise, and follow the pattern `<type>(<scope>) <description>`, where: - `type` is one of `chore`, `feat`, `fix` to represent if something is a chore, a new feature, or a fix to something broken - `scope` tells us where roughly the changes are made. This can mean a specific file name (if only one file has been changed), a part of the editor (e.g. the `canvas`), or something larger or smaller. - `description` is a concise description of what has actually changed
- Types should be used everywhere, since they leave less scope for misunderstandings (with us or the compiler).
- Speaking of types, please avoid `any` like the plague! It has caused immeasurable pain, as it allows code to circumvent the type system. Instead, try to use `unknown` and then use runtime checks to ensure that the right type is being used (we know this isn't always possible though)
- Avoid double or triple negatives where possible. e.g., rather than testing `if (!thingIsFalse)`, prefer to split over two lines `const thingIsTrue = !thingIsFalse; if (thingIsTrue)`
- Nested ternaries will be met with wrath! Ternaries should only be used where they can be kept concise (we do tend to make exceptions when writing JSX code however)
