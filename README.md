# Goals
- Simple and well tested code
- All test failures are descriptive

# Features
- (TODO) Automatic test cleanup
- (DONE) Support for extensions
- (TODO) Support for parallel execution - low priority
- (TODO) Support for mocks - can be done via extensions

# Bugs
- Any failure that isn't descriptive, correct, and readable
- Any design choice that does not encourage clean code

# Results (written 7/15/2019)
Writing this section after previously giving up on this project...

Testing prolog currently is not that great. What is difficult:
1. Descriptive error messages -- this can't be done without targeting a specific prolog's internals
2. Managing side effects -- http, asserta, etc
3. Working with modules -- needs a modern system and one that works well with DI and multiple environments
4. Working with dependencies -- something like bundler/virtualenv/mix/yarn

My suggestions:
1. Commit dependencies as git submodules
2. Test only pure code
3. Lightly test the system as a black box w/ prefered tech stack
4. Write your own prolog, ISO prolog fails
