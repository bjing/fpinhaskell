# Purpose
This project template has been created to serve as a starting point for those who are keen to implement exercises from "Functional Programming in Scala" in Haskell instead of Scala.

# Setup
## Stack
Stack is a build tool for your haskell projects.

Install Stack either using Homebrew or directly download it from the official website.
```
brew install haskell-stack
```
or
```
curl -sSL https://get.haskellstack.org/ | sh
```

## Global Haskell environment
**Skip this step if you already have a global Haskell environment set up.**

Initialise stack. This will install the basic Haskell toolchain.
```
cd ~
stack setup
```
By default, the Haskell binaries (e.g. ghc and ghci) are installed into `.stack` folder under your `${HOME}`, add it to your Shell's PATH environment variable. For example, for Bash, add the following to your `.bashrc` or `.bash_profile`
```
PATH="${HOME}/.stack/programs/x86_64-osx/ghc-8.0.1/bin:${PATH}"
export PATH
```
To apply the above change, simply `source` the newly updated file like this:
```
source ~/.bashrc
```

# Use this template project
### Install project dependencies
Under the project's root directory, do:
```
stack setup
```
### Build the project
Under the project's root directory, do:
```
stack build
```
### Run unit tests
Under the project's root directory, do:
```
stack test
```
### Write code
#### Organise modules
Code sits under the `src` directory. Each chapter has its own module. For example, answers to exercises in chapter "Functional Data Structures" go under the module `Datastructures`

#### Register modules
Currently, you need to register modules by adding them to the `exposed-modules` section in the project's `fpinhaskell.cabal` file. Modules are separated by commas.

I'm yet to find a way to make Stack aware of the modules without explicitly registering each module. If you manage to figure out a better way of doing this, feel free to submit a pull request.

#### Tests
The project uses the Hspec test framework. All tests sit under the `test` directory and will be automatically discovered by the Stack build tool.

Look at the code example to get an idea how to write tests. Or visit http://hspec.github.io/ for detailed documentation.

# Other resources
If you would like Haskell code editor set up including syntax highlighting, code suggestion and linting, visit my [Haskell IDE setup repo](https://github.com/bjing/haskell-ide-setup).

[Scala project template](https://github.com/MYOB-Technology/fpinscala) created by Mateusz Karbowy.
