cmake_minimum_required(VERSION 3.1)

project(my_project VERSION 0.1
    DESCRIPTION "Fortran Package Manager"
    LANGUAGES Fortran C)

option(BOOTSTRAP "Build bootstrap version of Fpm" OFF)


set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

add_subdirectory(dep)

add_library(fpm_core
    src/fpm/cmd/export.f90
    src/fpm/cmd/install.f90
    src/fpm/cmd/new.f90
    src/fpm/cmd/publish.f90
    src/fpm/cmd/update.f90
    src/fpm/manifest/build.f90
    src/fpm/manifest/dependency.f90
    src/fpm/manifest/example.f90
    src/fpm/manifest/executable.f90
    src/fpm/manifest/fortran.f90
    src/fpm/manifest/install.f90
    src/fpm/manifest/library.f90
    src/fpm/manifest/meta.f90
    src/fpm/manifest/package.f90
    src/fpm/manifest/package_impl.f90
    src/fpm/manifest/preprocess.f90
    src/fpm/manifest/profiles.f90
    src/fpm/manifest/test.f90
    src/fpm/dependency.f90
    src/fpm/downloader.f90
    src/fpm/error.f90
    src/fpm/fpm_release.F90
    src/fpm/git.f90
    src/fpm/installer.f90
    src/fpm/manifest.f90
    src/fpm/toml.f90
    src/fpm/toml_impl.f90
    src/fpm/versioning.f90
    src/ptycheck/isatty.c
    src/ptycheck/iscygpty.c
    src/ptycheck/iscygpty.h
    src/filesystem_utilities.c
    src/fpm.f90
    src/fpm_impl.f90
    src/fpm_backend.F90
    src/fpm_backend_console.f90
    src/fpm_backend_output.f90
    src/fpm_command_line.f90
    src/fpm_compiler.F90
    src/fpm_environment.c
    src/fpm_environment.f90
    src/fpm_filesystem.F90
    src/fpm_meta.f90
    src/fpm_model.f90
    src/fpm_os.c
    src/fpm_os.F90
    src/fpm_pkg_config.f90
    src/fpm_settings.f90
    src/fpm_source_parsing.f90
    src/fpm_sources.f90
    src/fpm_strings.f90
    src/fpm_targets.f90)

if (FPM_BOOTSTRAP)
    target_compile_definitions(fpm_core PUBLIC FPM_BOOTSTRAP)
endif()
target_link_libraries(fpm_core PRIVATE tomlf M_CLI2 regex jonquil shlex)

add_executable(fpm app/main.f90)
target_link_libraries(fpm PRIVATE fpm_core)
install(TARGETS fpm DESTINATION "bin")

add_executable(new_test test/new_test/new_test.f90)
target_link_libraries(new_test PRIVATE fpm_core)

add_executable(help_test test/help_test/help_test.f90)
target_link_libraries(help_test PRIVATE fpm_core)

add_executable(cli_test test/cli_test/cli_test.f90)
target_link_libraries(cli_test PRIVATE fpm_core)

add_executable(fpm_test 
    test/fpm_test/main.f90
    test/fpm_test/test_backend.f90
    test/fpm_test/test_compiler.f90
    test/fpm_test/test_filesystem.f90
    test/fpm_test/test_installer.f90
    test/fpm_test/test_manifest.f90
    test/fpm_test/test_module_dependencies.f90
    test/fpm_test/test_os.f90
    test/fpm_test/test_package_dependencies.f90
    test/fpm_test/test_settings.f90
    test/fpm_test/test_source_parsing.f90
    test/fpm_test/test_toml.f90
    test/fpm_test/test_versioning.f90
    test/fpm_test/testsuite.f90)

target_link_libraries(fpm_test PRIVATE fpm_core)
