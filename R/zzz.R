.onAttach <- function(libname, pkgname) {





  packageStartupMessage("This is version ", packageVersion(pkgname), " of ", pkgname,
                        cat(crayon::yellow(c(
                          "Loaded succesfully, thanks for using this package",
                          "                                     ','. '. ; : ,','\n",
                          "                                       '..'.,',..'\n",
                          "                                          ';.'  ,'\n",
                          "                                           ;;\n",
                          "                                           ;'\n",
                          "                             :._   _.------------.___\n",
                          "                     __      :__:-'                  '--.\n",
                          "              __   ,' .'    .'             ______________'.\n",
                          "            /__ '.-  ____.'          0  .' .'  .'  _.-_.'\n",
                          "               '._YM                 .-': .' _.' _.'_.'\n",
                          "                '----'._____________.'_'._:_:_.-'--'\n",
                          "                             \n",
                          "                          HAPPY WHALE IS HAPPY"))))
                        }
