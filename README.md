# IOTasks [![Haskell CI](https://github.com/fmidue/IOTasks/workflows/Haskell%20CI/badge.svg)](https://github.com/fmidue/IOTasks/actions?query=workflow%3A%22Haskell+CI%22+branch%3Amain)

Examples for writing specifications can be found under [examples/](examples/) and in the online [demo](https://iotasks.eu).

## Documentation
Main branch: [Haddock Documentation](https://fmidue.github.io/IOTasks)

## Publications
- [Describing Console I/O Behavior for Testing Student Submissions in Haskell](https://arxiv.org/abs/2008.09253v1) - TFPIE 2019
- [Implementing, and Keeping in Check, a DSL Used in E-Learning](https://link.springer.com/chapter/10.1007/978-3-030-59025-3_11) - FLOPS 2020
- [A Framework for Generating Diverse Haskell-IO Exercise Tasks](https://arxiv.org/abs/2008.12751) - WFLP 2020

### API for task templates (WFLP 2020)
For task-template examples see [the old code base](https://github.com/fmidue/IOTasks/blob/old-code-base/task-api/src/Test/IOTasks/Task/Examples.hs).
You can generate tasks from templates by cloning the repository and using ```showTaskInstance <example-name>``` after invoking ```stack ghci```.
