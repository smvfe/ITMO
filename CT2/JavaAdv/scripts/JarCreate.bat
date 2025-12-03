cd C:\coding\java-advanced\java-advanced\scripts
javac -d C:\coding\java-advanced\java-advanced\scripts -cp C:\coding\java-advanced\java-advanced-2025\modules;C:\coding\java-advanced\java-advanced-2025\lib\*;C:\coding\java-advanced\java-advanced-2025\artifacts\info.kgeorgiy.java.advanced.implementor.jar;C:\coding\java-advanced\java-advanced-2025\artifacts\info.kgeorgiy.java.advanced.implementor.tools.jar C:\coding\java-advanced\java-advanced\java-solutions\info\kgeorgiy\ja\mochekov\implementor\Implementor.java

cd C:\coding\java-advanced\java-advanced\scripts
jar xf C:\coding\java-advanced\java-advanced-2025\artifacts\info.kgeorgiy.java.advanced.implementor.jar info\kgeorgiy\java\advanced\implementor\Impler.class info\kgeorgiy\java\advanced\implementor\ImplerException.class
jar xf C:\coding\java-advanced\java-advanced-2025\artifacts\info.kgeorgiy.java.advanced.implementor.tools.jar info\kgeorgiy\java\advanced\implementor\tools\JarImpler.class

jar cfm C:\coding\java-advanced\java-advanced\scripts\Implementor.jar C:\coding\java-advanced\java-advanced\scripts\MANIFEST.MF C:\coding\java-advanced\java-advanced\java-solutions\info\kgeorgiy\ja\mochekov\implementor\*.class info\kgeorgiy\java\advanced\implementor\*.class info\kgeorgiy\java\advanced\implementor\tools\*.class
cd C:\coding\java-advanced\java-advanced\scripts