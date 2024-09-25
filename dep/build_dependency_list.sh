
find toml-f/src -name *.f90 > deps.txt
find M_CLI2/src -name *.F90 >> deps.txt
find fortran-regex/src -name *.f90 >> deps.txt
find jonquil/src -name *.f90 >> deps.txt
find fortran-shlex/src -name *.f90 >> deps.txt

# Create CMakeLists.txt and add the required content
{
  echo "add_library(fpm_dep"
  sed 's/^/    /' deps.txt  # This adds 4 spaces of indentation
  echo ")"
} > CMakeLists.txt