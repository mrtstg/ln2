# proxmox-fs-agent

Агент для корректировки конфигурационных файлов виртуальных машин, а в частности поля args, которое до сих пор закрыто
разработчиками только для **пользователя** root.

Доступ предоставляется по Bearer-аутентификации, требуемый токен определяется переменной окружения PROXMOX_AGENT_ACCESS_TOKEN

# Требуемое ПО для разработки
- envsubst
