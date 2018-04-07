Finish the code generator in the Analyzer to narrow down what information is actually needed, and understand how to model the types.

Improve the Parser with stuff taken from the Analyzer so that it outputs types similar to those in the Analyzer, but without info that has to be inferred by looking at the previous declarations. The Parser shouldn't have to do module resolution.

The Analyzer should then take this output and figure out the types of all the fields in the declarations, what module the aliased types come from, which declarations and functions to output and the size of all structs.


Stuff to infer in the Analyzer:
* in type aliases, whether the type it references is a basic type, an enum, a mask or a struct
* the size in bytes of every type, enum, mask, and struct
* for enums and masks, which types we need to generate conversion functions for (e.g. `x_mask_of_int32` or `x_mask_of_int`)
