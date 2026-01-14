# Multi-Word CAS

In this task, you need to implement the atomic `cas2(..)` for `AtomicArray`, 
which atomically changes two locations in the array. Please use the multi-word
CAS algorithm discussed in class. Remember to always install k-CAS descriptors 
in the same order. 

You can also read the original paper by Timothy L. Harris, Keir Fraser and Ian A. Pratt:
[A Practical Multi-Word Compare-and-Swap Operation](https://www.cl.cam.ac.uk/research/srg/netos/papers/2002-casn.pdf).

**Write your first and last name in the headers of the files after the `@author` tag.**

To test your solution, please run:

* `./gradlew test` on Linux or MacOS
* `gradlew test` on Windows