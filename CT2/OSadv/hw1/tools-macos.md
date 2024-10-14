# Настройка инструментов на macOS

1. Установите инструменты разработчика и пакетный менеджер [Homebrew](https://brew.sh/):
   
   ```
   $ xcode-select --install
   $ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
   ```

2. Установите [тулчейн для RISC-V](https://formulae.brew.sh/formula/riscv64-elf-gcc):

   ```
   $ brew install riscv-elf-gcc
   ```

   > ⚠️ На Apple Silicon Homebrew [не добавляет](https://docs.brew.sh/FAQ#why-is-the-default-installation-prefix-opthomebrew-on-apple-silicon) ссылки в `/usr/local`.
   >
   > При установке Homebrew сообщает, как добавить его директорию в переменную окружения `PATH`. Если вы этого не сделали, сделайте это сейчас:
   >
   > - для Bash: `eval "$(/opt/homebrew/bin/brew shellenv)" >> ~/.bash_profile`
   > - для Zsh: `eval "$(/opt/homebrew/bin/brew shellenv)" >> ~/.zprofile`
   >
   > Это позволит не писать полный путь к исполняемым файлам.

3. Установите [QEMU](https://www.qemu.org/):

   ```
   $ brew install qemu
   ```

4. Установите [gdb](https://www.gnu.org/software/gdb/):

   ```
   $ brew install riscv64-elf-gdb
   ```

5. Проверьте, что всё работает:

   ```
   $ riscv64-elf-gcc --version
   riscv64-elf-gcc (GCC) 14.2.0
   $ qemu-system-riscv64 --version
   QEMU emulator version 9.1.0
   ```
   
   > Версии могут быть более новыми, это нормально.
