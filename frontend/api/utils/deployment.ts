import type { DeploymentErrorKind } from "../types/deployment"

export const deploymentErrorToString = (data: [DeploymentErrorKind, any]): string => {
  let kind = data[0]
  let body = data[1]
  switch(kind) {
  case 'template':
    return 'Неизвестный шаблон: ' + body.value
  case 'missingNetwork':
    return 'Неизвестная сеть: ' + body.value
  case 'forbiddenNetwork':
    return 'Недопустимое название сети: ' + body.value
  case 'emptyVMName':
    return 'Заполните все названия виртуальных машин'
  case 'emptyNetworkName':
    return 'Заполните все названия сетей'
  case 'longVMName':
    return 'Названия всех виртуальных машин должны быть короче 16 символов!'
  case 'longNetworkName':
    return 'Названия всех сетей должны быть короче 16 символов!'
  case 'invalidCPU':
    return 'Некорректное значение CPU: ' + body.value
  case 'invalidSockets':
    return 'Некорректное значение сокета: ' + body.value
  case 'invalidMemory':
    return 'Некорректное значение памяти: ' + body.value
  case 'duplicateVMName':
    return 'Дубликат имени виртуальной машины: ' + body.value
  case 'duplicateNetworkName':
    return 'Дубликат имени сети: ' + body.value
  case 'unknown':
		return 'Неизвестная ошибка'
  default:
		return 'Неизвестная ошибка'
  }
}
