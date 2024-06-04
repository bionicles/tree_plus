#include "pybind11/pybind11.h"

#include "cpp_examples_impl.h"

PYBIND11_MODULE(cpp_examples, m)
{
    m.doc() = "pybind11 cpp_examples plugin"; // module docstring

    m.def("add", &add<int>, "An example function to add two numbers.");
}
