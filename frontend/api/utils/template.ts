import type { TemplateError } from "../types/template"
import type { ErrorWrapper } from "../types/errorWrapper"

export const templateErrorToString = (data: ErrorWrapper<TemplateError>): string => {
  if (data.type != null) {
		switch(data.type) {
			case 'templateNameTaken':
				return 'Шаблон с таким именем существует'
			case 'templateIDTaken':
				return 'Шаблон с таким ID существует'
			case 'templateNotFound':
				return 'Шаблон не найден'
			case 'noUpdateBody':
				return 'Параметры шаблона не изменены'
			case 'forbiddenTemplateID':
				return 'ID шаблона имеет недопустимое значение'
			case 'emptyTemplateName':
				return 'Имя шаблона пусто'
			case 'longTemplateName':
				return 'Имя шаблона превышает лимит длины'
			case 'longTemplateComment':
				return 'Комментарий шаблона слишком длинный'
    }
  }
  return 'Неизвестная ошибка'
}
