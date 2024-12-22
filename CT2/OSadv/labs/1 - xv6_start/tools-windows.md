# Настройка инструментов на Windows

Мы не рекомендуем выполнять лабораторные работы непосредственно на Windows. Вместо этого, вы можете воспользоваться одним из следующих способов:

## Рекомендуемый способ — WSL2

Установите [Linux Subsystem for Windows](https://docs.microsoft.com/ru-ru/windows/wsl/). Убедитесь, что у вас установлена вторая версия WSL командой `wsl.exe -l -v` из Windows.

Дальше следуйте инструкциям для Linux.

## Альтернативные варианты

1. Поставьте операционную систему на базе ядра Linux. Её можно поставить как вместо Windows, так и рядом с ней, если у вас на диске достаточно места. Например, попробуйте [Ubuntu](https://www.ubuntu.com/).

2. Скачайте менеджер виртуальных машин [VirtualBox](https://www.virtualbox.org) и создайте ВМ с Linux в нём.

3. Установите [MSYS2](https://www.msys2.org/). В оболочке «MSYS2 MinGW 64-bit» установите следующие пакеты:

   ```
   $ pacman -S make mingw-w64-x86_64-gcc mingw-w64-x86_64-qemu mingw-w64-x86_64-riscv64-unknown-elf-gcc git
   ```

   Проверьте, что всё работает:

   ```
   $ gcc --version
   gcc.exe (Rev1, Built by MSYS2 project) 10.2.0
   $ riscv64-unknown-elf-gcc --version
   riscv64-unknown-elf-gcc.exe (GCC) 10.1.0
   $ qemu-system-riscv64 --version
   QEMU emulator version 5.1.0
   ```

   Версии могут быть более новыми, это нормально.

   Обратите внимание, что в этом случае вы не сможете отлаживать операционную систему с помощью GDB.

> Мы не поддерживаем установку тулчейна через MSYS. Если у вас что-то не заработает, воспользуйтесь WSL или виртуальной машиной.
