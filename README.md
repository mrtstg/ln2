# ln2

Переизобретаем образовательную платформу. Снова.

# TODO
- Лимит задач агента
- Очистка списка задач
- Кастомизация таймаута проверки

## Архитектура выполнения вычислительных задач

Все задачи отправляются на мастер-сервер, который обращается к известным ему агентам. Задачи
распределяются по шине RabbitMQ.

После передачи запроса, сервер возвращает идентификатор задачи, по которой можно отследить выполнение.

В это время сервер переадресует выполнение одному из агентов, который начнет развертывание 
инфраструктуры по специализированному формату YAML, который заранее указывается и создается на стороне мастер-сервера.

Выполнение произвольного сценария может выполняться при помощи команды `docker exec`, запускаемой агентом, либо при помощи
произвольного скрипта с произвольного образа (по умолчанию, `python:3.11-slim`), который будет выполнен с контейнера, заранее
добавленного в сеть контейнеров.

# Программные зависимости

- Procodile - запуск сервисов вне Docker
