@echo off
setlocal

set SRC=info\kgeorgiy\ja\mochekov\implementor\*
set MODULE_IMPL=..\..\java-advanced-2025\modules\info.kgeorgiy.java.advanced.implementor\info\kgeorgiy\java\advanced\implementor
set MODULE_TOOLS=..\..\java-advanced-2025\modules\info.kgeorgiy.java.advanced.implementor.tools\info\kgeorgiy\java\advanced\implementor\tools
set MODULE_LIB=..\..\java-advanced-2025\lib
set OUT_DIR=doc

javadoc -d %OUT_DIR% -private %MODULE_IMPL%\Impler.java %MODULE_IMPL%\ImplerException.java %MODULE_TOOLS%\JarImpler.java

endlocal