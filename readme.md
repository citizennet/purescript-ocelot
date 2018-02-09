# CN-UI Documentation Site

This branch holds the generated files that drive the cn-ui documentation site. These files are held in version control here, but not in the master branch. Accordingly, this is an orphan branch from `master`. Build scripts and tooling should live in the master branch, but any commit to master should trigger a build and copy of the generated files to `gh-pages`.

## Notes

- This branch requires a .circleci/config.yml file that explicitly ignores building the branch. It's useful to just keep this as a running copy of the one on master.
