---
name: Bug report
about: Reporting an actual bug
title: ''
labels: bug, Bug (perceived)
assignees: ''

---

**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Steps to reproduce the behavior. For canvas tasks, take care to note down all relevant key holds, keys typed, selected state etc. E.g:

1. Insert View A into Main
2. Insert View B into View A, with at least 30px margin from A's edges
3. Move mouse over top edge of B, mouse pointer changes to resize
4. Press `space` key once. Selected element will now be A.

Result: Mouse pointer is still indicating resize, even though A can't be resized where the mouse is, an B is no longer selected or resizeable when dragging the mouse where it is.

**Expected behaviour**
Mouse pointer should change when it's no longer over edge of an element that could be resized by pressing-and-dragging mouse

**Screenshots**
If applicable, add screenshots to help explain your problem.

**Sample Code**
If applicable, add sample code to help explain your problem
