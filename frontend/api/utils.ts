export const courseErrorsToString = (error: string): string => {
  switch (error) {
    case 'Not found':
      return 'Курс не найден!'
    case 'Forbidden':
      return 'У вас нет доступа к курсу!'
    case 'Bad request':
      return 'Название курса занято!'
    default:
      return 'Неизвестная ошибка!'
  }
}

export const courseTaskErrorToString = (error: string): string => {
  switch (error) {
    case 'Not found':
      return 'Курс не найден!'
    case 'Forbidden':
      return 'У вас нет доступа к курсу!'
    case 'Unknown':
      return 'Неизвестная ошибка!'
    default:
      return 'Неизвестная ошибка!'
  }
}

export const taskStatusToString = (status: string): string => {
  switch (status) {
    case 'queued':
      return 'В очереди на выполнение'
    case 'taken':
      return 'Взята на выполнение'
    case 'error':
      return 'Завершена с ошибкой'
    case 'processing':
      return 'Выполняется'
    case 'finished':
      return 'Проверка завершена'
    default:
      return 'Неизвестный статус'
  }
}

export const taskCreateErrorToString = (error: string): string => {
  switch (error) {
    case 'invalid':
      return 'Некорректное содержание задания или решения!'
    case 'timeout':
      return 'Вы отправляете слишком много решений в период времени!'
    case 'unauthorized':
      return 'У вас нет доступа к заданию!'
    default:
      return 'Неизвестная ошибка!'
  }
}
