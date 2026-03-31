# Computer science

[![Languages](https://img.shields.io/github/languages/top/smvfe/ITMO)](https://github.com/smvfe/ITMO)
[![Repo Size](https://img.shields.io/github/repo-size/smvfe/ITMO)](https://github.com/smvfe/ITMO)

> *Projects, research, and laboratory works completed during my studies at ITMO University.*

Сборник проектов, выполненных во время обучения. 

---

## Ключевые проекты 

### 1. Системное программирование и ОС 
* **Copy-On-Write в xv6:** Модификация подсистемы виртуальной памяти учебной ОС xv6 (MIT). Реализован механизм отложенного копирования страниц при `fork()`.
* **NetworkFS:** Загружаемый модуль ядра Linux для монтирования и взаимодействия с кастомной сетевой файловой системой.
* **PE Parser:** Консольная утилита на C для статического анализа исполняемых файлов Windows с извлечением таблиц импорта/экспорта.
* 📂 `CT2/OSadv/labs/`

### 2. Custom Memory Allocator & LFRU Cache (C++20)
* **Buddy Allocator:** Собственная реализация пула памяти на основе алгоритма buddy memory allocation.
* **LFRU Cache:** Двухуровневый кэш с привилегированной и непривилегированной очередями.
* Проект использует современные стандарты C++, систему сборки `CMake`, пакетный менеджер `Conan` и покрыт тестами `GTest`.
* 📂 `CT1/cpp/hw/s3-lfru-buddy/`

### 3. Интерпретатор языка программирования
* Разработка собственного функционального языка программирования.
* Включает лексер и парсер на базе комбинаторной библиотеки `Megaparsec`, интерактивную оболочку и вычислитель в монадном контексте.
* 📂 `CT3/FuncProg/`

### 4. Математическое
* Анализ данных, прототипирование алгоритмов с использованием Python-стека.

---
