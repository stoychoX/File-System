## **Functional programming project**
*Synopsis*: Program, which simulates a simple file system.

### Кратко описание на решението
В основата на решението лежи начинът по който модерните файлови системи работят. Имаме файлове, които за наше улеснение съхраняваме в папки. Въпросните папки, също за наше удобство, съхраняваме в други папки. От тази идея се вдъхновява и следния тип, който е основен за решението на задачата:

```haskell 
data FileSystem =
     File String String     |
     Root String [FileSystem]
     deriving(Show)
```
 Конструкторите приемат:
   1. File <име на файла> <съдържание на файла>
   2. Root <име на директорията> <съдържание на директорията>

Манипулацията на този тип ни дава всичко нужно да симулираме работата на една файлова система.

### Кратък преглед на структурата на решението
Решението може да се раздели на две основни части.
1. Работа с входа
     Важна част от решението е начинът по който работим с входа. Кодът отговорен за парсването на входа е записан в модула Parser.hs и е частично взаимстван от:

   * https://github.com/tsoding/haskell-json/commit/bafd97d96b792edd3e170525a7944b9f01de7e34
   * https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf
   * https://github.com/googleson78/fp-lab-2021-22/blob/main/exercises/09/ParserCombinators.hs
2. Работа с типа FileSystem
     В решението логиката по работа с типа FileSystem е изнесена в различни модули. Може би най - ключовите функции за работа с типа са
     * Функции по добавяне: add, addFile, addFolder
          addFile и addFolder вътрешно викат add, която приема път към директорията в която ще бъде добавен файла/папката както и нужната информация (име, съдържание).
     * Функции по премахване
          removeFileFromRoot премахва файл с подадено име от корен ако той съществува. Ако не съществува типа излиза непроменен.
     * Функции за търсене
        Решението предлага различни начини за търсене на файл/папка в система.
     * "Движение" във файлова система 
        Функциите в модула ChangingDirections ни помагат да симулираме движение когато търсим/добавяме или премахваме нещо от комплексна файлова система.

### Инсталиране на проекта
1. git clone https://github.com/stoychoX/File-System.git
2. Отваряме терминал в папката File-System и пишем ghci driver.hs
В началото, с цел показване на функционалността на проекта, програмата стартира с примерна файлова система.
Ако искаме да избегнем това в main вместо [mySystem] като аргумент на repl даваме [(Root "/" [])].
Функцията repl очаква винаги да имаме корен. Пускането на празен списък би довело до неадекватно изпълнение на програмата.


### Синтаксис
* pwd - извежда текущата директория
* ls - извежда какво има в текущата директория
* ls <\/relative-path> - извежда какво има в директория подадена като аргумент.
* cd <\/relative-path> - променя работната директория
* cd .. - връща се стъпка назад ако това е възможно.
* cat < file > < fileOne > < fileTwo > > < exitFile > - обединята файловете, подадени като аргументи във файл, с чието име е подадено като аргумент след знака '>'.
* rm < file > < fileOne > .. < fileN > - изтрива файловете от текущата директория ако те съществуват.
* mkfile < filename > < filecontent > ++ <~> - създава нов файл с име подадено като първи аргумент и съдържание, което приключва когато срещнем знака '~'. Поддържа валидация на имена така, че да няма дублиране.
* mkdir < name > - създава нова папка с име подадено като аргумент. Поддържа валидация на имена.
* show < filename > - показва името и съдържанието на файла
* :q - излиза от файловата система.


### Пример
![](example.bmp)