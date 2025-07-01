from conan import ConanFile

class test_task(ConanFile):
    name = "test_task"
    version = "0.0.1"
    settings = "os", "arch", "compiler", "build_type"

    generators = "CMakeDeps"

    def requirements(self):
        self.requires("gtest/1.15.0")
