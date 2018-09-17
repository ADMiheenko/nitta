# Reconfigurable real-time computational platform

## Application area

- developing cyber-physical systems which are based on adaptive robust control algorithms and
  artificial intelligence with high requirements on latency and computational volume, power and area
  consumption; 
- developing hardware programmable accelerators and co-processors;
- developing problem-oriented programmable ASIC; 
- developing dynamically reconfigurable IP-core and soft-core for FPGA.

## Project goals

- rapid prototyping of control and cyber-physical systems;
- hardware in the loop simulation;
- target system synthesis and its components;
- integration of the real-time system with nonreal-time environment and automation of its
  interaction;
- developing of IP-core for embedded systems and systems on a chip.

## Key features

- the orientation on model-driven engineering and not on software engineering; 
- automation of most of the development stages, including algorithm and model design and functional
  simulation, prototyping and complex verification, complex automatisation of cross-layer testing
  and synthesis and optimisation of a target system;
- deeply computational platform reconfiguration on hardware, software and tool levels, transparency
  of CAD system workflow.

Project status: early prototype.

# Реконфигурируемая вычислительная платформа реального времени NITTA

## Область применения:
- разработка киберфизических систем на базе адаптивных робастных алгоритмов управления и
  искусственного интеллекта с высокими совокупными требованиями по временным задержкам, объёму
  вычислений, площади и энергопотреблению;
- разработка аппаратных программируемых ускорителей и сопроцессоров;
- разработка проблемно-ориентированных ASIC-процессоров;
- разработка динамически реконфигурируемых и программируемых IP-ядер.

## Задачи, на решение которых направлен проект:
- быстрое прототипирование (rapid prototyping);
- программно-аппаратное моделирование (hardware in the loop simulation);
- синтез целевой системы или её компонент;
- интеграция вычислителей реального времени с недетерминированным окружением;
- разработка IP-ядер для встроенных систем и систем на кристалле.

## Основные особенности:
- ориентация на принципы модельно-ориентированной инженерии, а не на практики из мира
  программирования;
- инструментальная поддержка большинства этапов разработки, включая: проектирование и функциональную
  симуляцию алгоритмов и моделей; прототипирование и проведение испытаний; комплексное
  автоматизированное тестирование; синтез, анализ и оптимизация целевой системы;
- глубокая реконфигурируемость вычислительной платформы, прозрачность работы инструментальных
  средств, применение средств искусственного интеллекта и использование онтологического
  моделирования;
- строгий контроль за ограничениями реального времени с возможностью глубокой детализации в рамках
  алгоритма, возможность синхронизации алгоритма в требуемых точках с внешними событиями, между
  компонентами распределённого вычислителя.

Статус проекта: ранний прототип.

Знакомство с проектом следует начать с модуля [Demo](src/Demo.hs). В нём присутствует несколько
демонстраций работы вычислительной платформы NITTA, описан испытательный стенд, а также как
запустить демонстрацию.

Telegram чат проекта: <https://t.me/joinchat/BC5sV1GY7ADnCuOsKP-bJw>

## Документация
С публикациями связанными с проектом можно ознакомиться здесь: <https://nitta.io/nitta-corp/docs>.

### Инструментарий
1. [Инструментарий для моделирования и тестирования аппаратуры](doc/hdl-install.md)
1. [Работа с репозиторием, установка и настройка SourceTree](doc/sourcetree-install.md)
1. [Установка инструментария для Haskell (haskell-stack)](doc/stack-install.md)
1. [Установка редактора Visual Studio Code](doc/vscode-install.md)

### Работа над проектом
1. [Аббревиатуры в исхоном коде](doc/abbreviation.md)
1. [Рекомендации по работе с проектом](doc/rules.md)
1. [Пример/образец вычислительно блока](src/NITTA/ProcessUnits/Multiplier.hs)
1. [Демонстрация работы вычислительной платформы](src/Demo.hs)

### Разное
1. [Почему в качестве языка программирования выбран Haskell](/doc/why-haskell)


