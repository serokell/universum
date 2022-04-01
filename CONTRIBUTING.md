# Contributing to `universum`

## :wave: Greetings Traveler!

We are glad you're reading this, we really appreciate the effort you're
putting in. Thank you for your help in making this library awesome! :sparkles:

### Versioning policy
The version of this library typically consists of at most four numbers (last numbers are assumed to be 0 if there are less than 4 numbers).
The semantics is `marketing.breaking.feature.fix`:
* `fix` is increased when set of exported definitions is not modified (all changes are not visible outside).
* `feature` is increased when set of exported definitions is strictly augmented.
* `breaking` is increased when breaking changes are made.
* `marketing` is increased for releases which are important from marketing perspective.

### Code modification policy

**tl;dr**

1. Issues with labels `type:not-code` and `type:fix` can be resolved at any time.
2. Issues with labels `type:feature` and `type:breaking` can be resolved only after being added to a milestone.
3. `features` releases happen at most once per two weeks.
3. `breaking` releases happen at most once per five weeks.

**Actual policy**

We think that a custom prelude shouldn't change often.
Because of that we have a semi-formal policy describing which modifications can be done to `universum` under which circumstances.
We split all issues into four semi-formally defined categories:
1. `not-code`.
Everything that doesn't modify source code (except non-Haddock comments), `.cabal` files, other files considered by GHC (if they exist).
Examples: fix a typo in README.md, fix something in CI configuration files, add a pull request template.
2. `fix`.
Everything that doesn't fit in `not-code`, but doesn't change set of exported definitions and their types or semantics.
Examples: fix compatibility with a new version of a library/compiler, improve performance of some function (keeping its semantics intact).
Note: despite the name, issues of this type do not necessary correspond to bug fixes.
3. `feature`.
Modifications which make set of exported definitions a superset of the old set of exported definitions.
I. e. all existing definitions are left intact, only something new is added.
Example: add a new function, re-export a type class, add a new instance of some type class.
4. `breaking`.
Basically all other changes.
Examples: remove of a function, rename a type class, change arguments order of a function.

Each issue should belong to only one category.
If an issue involves multiple changes of different types, it should be split into smaller issues

For each category we have a label on GitHub, these labels start with the `type:` prefix.
There is also the `type:unclear` label for issues which don't have a list of specific actions to be done.

Processing of an issue depends on its category. Specifically:
1. A `not-code` issue can be resolved at any time.
PRs can be merged whenever GitHub allows it.
Minimal number of approvals is set in GitHub settings.
2. Rules for the `fix` issues are the same as for `not-code` with an additional rule that PRs for such issues should increase the last component in the version and this version should be uploaded to Hackage as soon as the PR is merged (also new tag and release are created).
3. Work on the `feature` issues is organized in milestones.
Each milestone corresponds to a release which increases at least the third (`feature`) version component.
Scope of each milestone is discussed in issues and is set by maintainers.
In some cases a special issue can be created to discuss the scope of a particular milestone.
A `feature` issue is added to a milestone if such addition is approved by at least 3 people (by explicitly stating that in GH comments).
At least 2 of them must be from Serokell.
However, if there are 4 people (from which at least 3 are from Serokell) who disapprove this issue, it can't be added to a milestone.
A new `feature` release can happen only after at least 2 weeks have passed after last release (except `fix` releases).
PR can be merged only if the corresponding `feature` issue is in the upcoming milestone.
4. Work on the `breaking` issues is organized in almost the same way.
However, 4 approvals (at least 3 from Serokell) are necessary for inclusion into a milestone.
2 disapprovals from Serokell or 4 disapprovals in total are enough to prevent the issue from getting into a milestone.
If an issue is about removal of something (`x`), it should be done in two steps.
The first step is to deprecate `x` which happens in a `breaking` release.
The second step is to delete `x` which happens in another `breaking` release after deprecating `x`.
There can't be more than one breaking release in less than 5 weeks.

Note: approval of any PR implies that the person who approves it confirms that the PR corresponds to the issue mentioned there and the issue has a correct type.

### How to contribute

#### Report bugs or feature request
If you have found any bugs or have proposals on how to make this project better,
don't hesitate to create issues
[here](https://github.com/serokell/universum/issues/new) in free format.

#### Share your opinion in existing issues and pull requests
We want `universum` to be suitable for as many Haskell developers as possible.
That's why we would greatly appreciate your comments in our issues and pull requests.
Please tell us whether you think that proposed modifications makes sense and are worth doing.

#### Create a PR
We love receiving pull requests from everyone. But, please, don't create a PR
without a corresponding issue. It's always a good idea to express your wish
to do that issue under comment section. Thus you will show that you're doing
that issue, and nobody else will accidentally do it in parallel with you. Furthermore you
also can discuss the best way to implement that issue!
Make sure to read the [Code modification policy](#code-modification-policy) before starting your work on any issue.

To get started with this you should first fork, then clone the repo:

    git clone git@github.com:your-username/universum.git

Make your changes and consider the following check list to go through before submitting your pull request.

#### :white_check_mark: Check list
- [ ] Project compiles
- [ ] New/fixed features work as expected
- [ ] Old features do not break after the change
- [ ] _Recommended:_ Commit messages are in the proper format. If the commit
  addresses an issue start the first line of the commit with the issue number in
  square parentheses.
  + **_Example:_** `[#42] Short commit description`

If you fix bugs or add new features, please add tests.

After everything above is done, commit and push to your fork.
Now you are ready to [submit a pull request][pr]!

----------
Thanks for spending time on reading this contributing guide! :sparkling_heart:

[pr]: https://github.com/serokell/universum/compare/
