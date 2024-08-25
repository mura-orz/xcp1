# XCP-1 - single-file xxx c++ compiler

## Introduction

 - It may become a simple and tiny C++ compiler in future.
 - It confirms to C++ of ISO/IEC 14882:2020 (C++20).

##　Concepts & Notice of disclaimer

 - It is copyrighted by Mura. (c) 2023-, Mura. All rights reserved.
 - It is not Free Software.
 - It is not granted for any use to any others.
 - It is just only for my hobby.
 - It designates to simple use of modern C++20.
 - It cannot always satisfy all the requirements on actual usages.
 - It is not efficient for actual purpose.
 - It is just "as is" and no warranty and no garranty for any use and damage of any one.
 - It complies with the laws and regulations of Japan.

## Environments

### Build environments

 - It can be built by modern C++20 compilers and standard libraries: e.g.,
    - GNU g++ / libstdc++
    - LLVM clang++ / libc++ (It fails to compile currently because it has not supported std::source_location well yet.)
    - Visual C++
 - It can be built on the "hosted" environtments. e.g.,
    - 32/64bit Windows
    - 32/64bit Linux

### Runtime environments

 - Same environment as the environment it was built

## Usage

  $ ./xcp  (options)  sources

### Options

 -  -h                   Shows usage only.
 -  -v                   Shows version.
 -  -D:{Macro}           Defines the {Macro} as the value 1
 -  -D:{Macro}={Value}   Defines the {Macro} as the {value}
 -  -I:{Path}            Adds the including path {Path} to find headers.
 -  -L:{Path}            Adds the linkage path {Path} to find.libraries.
 -  -l:{name}            Links the library {name}.

## Implementation defined

 - TODO: List of implementation-defined 


