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
