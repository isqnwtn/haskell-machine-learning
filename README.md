# HXX
This repo is an implementation of many machine learning / artificial intelligence related works but all of them done in haskell rather than the conventional language python.
I didn't know whether to name it HML (for haskell machine learning) or HAI (for haskell AI) or anything else so I've decidede to name it HXX as a placeholder for whatever 

# Docs and notes
More than a project this is a way of taking notes, related material which are too much to be in the comments or other theories that backup the code can be found in `doc/`


# Building
The project is maintained using stack. Use the following command to build
`stack build`

to run use the following command
`stack run -- <args>`

# Arguments
| Argument           | Description                         | Remarks               |
|--------------------|-------------------------------------|-----------------------|
| `--interactive`    | open project in interactive mode    | disabled by default   |
| `--debug`          | degree of debugging                 | set to 0 by default   |
| `--random-gen`     | related to monadic random generator | yet to be implemented |
| `--linear-algebra` | related to linear algebra           | yet to be implemented |
| `--plots`          | plot stuff using haskell            | yet to be implemented |

