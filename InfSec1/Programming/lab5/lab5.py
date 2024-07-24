import argparse
import random
import string

def generateRubbish(length):
    letters = string.ascii_letters + string.digits
    return ''.join(random.choice(letters) for t in range(length))

def generateIPv4Address():
    return '.'.join(str(random.randint(0, 255)) for t in range(4))

def write_to_file_txt(filename, num_strings, debug_lines):
    with open(filename, 'w') as file:
        # Число элементов
        file.write(f"{num_strings} ")
        
        # Перестановка индексов
        indices = list(range(num_strings))
        random.shuffle(indices)

        # Генерация и записывание
        for i in range(num_strings):
            ipv4 = generateIPv4Address()
            file.write(f"{ipv4}\0")
            debug_lines.append((ipv4, indices[i]))
        
        # Область мусора
        
        for i in range(num_strings % 17):
            file.write(f"{generateRubbish(32 + i + num_strings % 31)}\0")
        

        # Внесeние индексов
        file.write("\n")
        for index in indices:
            file.write(f"{index} ")
        
        return list(range(num_strings))

def main():
    debug_lines = list() # Список для хранения пары
    parser = argparse.ArgumentParser(description="Генератор файла для двусвязного списка, наполненного IPv4-адресами.")
    parser.add_argument('filename', nargs='?', type=str, help="Название создаваемого файла")  # nargs='?' - опциональность
    parser.add_argument('-n', type=int, help="Число элементов, которе надо сгенерировать")
    parser.add_argument('-v', action='store_true', help="Информация о разработчике")

    
    # Спарсенные аргументы
    args = parser.parse_args()
    
    # Проверка аргументов
    if args.v:
        print("Мочеков Семён Сергеевич, гр. N3151\nВариант: 1-1")
        return
    elif not args.filename:
        raise ValueError("Требуется выходной файл")
    
    # Число строк если не указываем -n
    num_strings = args.n if args.n else random.randint(10, 1000)
    lines = write_to_file_txt(args.filename, num_strings, debug_lines)
    
    # Дебаг информация
    print(f"Сгенерированые {num_strings} IPv4-адреса и их индексы:")
    for ipv4, index in debug_lines:
        print(f"{ipv4} -> {index}")

if __name__ == "__main__":
    main()