## Description

<!--
Describes the nature of your changes. If they are substantial, you should
further subdivide this into a section describing the problem you are solving and
another describing your solution.
-->


## Related issues(s)

<!--
- Short description how the PR relates to the issue, including an issue link.

For example

- Fixed #1 by adding lenses to exported items
-->


## ✓ Checklist for your Pull Request

Ideally a PR has all of the checkmarks set.

If something in this list is irrelevant to your PR, you should still set this
checkmark indicating that you are sure it is dealt with (be that by irrelevance).

- [ ] I made sure my PR addresses a single concern, or multiple concerns which
      are inextricably linked. Otherwise I should open multiple PR's.
- [ ] If your PR fixes/relates to an open issue then the description should
      reference this issue. See also [auto linking on
      github](https://help.github.com/articles/autolinked-references-and-urls/).
- [ ] If I added/removed/deprecated functions/re-exports,
      I checked whether these changes impact the [`.hlint.yaml`](../tree/master/.hlint.yaml) rules
      and updated them if needed.

#### Related changes (conditional)

- Tests

  - [ ] If I added new functionality, I added tests covering it.
  - [ ] If I fixed a bug, I added a regression test to prevent the bug from
        silently reappearing again.

- Documentation

  I checked whether I should update the docs and did so if necessary:

  - [ ] [README](/README.md)
  - [ ] Haddock

- Record your changes

  - [ ] I added an entry to the [changelog](/CHANGES.md) if my changes are visible to the users
        and
  - [ ] provided a migration guide for breaking changes if possible

#### Stylistic guide (mandatory)

- [ ] My commit history is clean (only contains changes relating to my
      issue/pull request and no reverted-my-earlier-commit changes) and commit
      messages start with identifiers of related issues in square brackets.

  **Example:** `[#42] Short commit description`

  If necessary both of these can be achieved even after the commits have been
  made/pushed using [rebase and squash](https://git-scm.com/docs/git-rebase).
