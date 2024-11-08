import type { DeploymentErrorKind } from "../types/deployment"
import type { ErrorWrapper } from "../types/errorWrapper"

export const deploymentErrorToString = (data: ErrorWrapper<DeploymentErrorKind>): string => {
  let kind = data.type
  switch(kind) {
  case 'template':
    return 'Неизвестный шаблон: ' + data.value
  case 'missingNetwork':
    return 'Неизвестная сеть: ' + data.value
  case 'forbiddenNetwork':
    return 'Недопустимое название сети: ' + data.value
  case 'emptyVMName':
    return 'Заполните все названия виртуальных машин'
  case 'emptyNetworkName':
    return 'Заполните все названия сетей'
  case 'longVMName':
    return 'Названия всех виртуальных машин должны быть короче 16 символов!'
  case 'longNetworkName':
    return 'Названия всех сетей должны быть короче 16 символов!'
  case 'invalidCPU':
    return 'Некорректное значение CPU: ' + data.value
  case 'invalidSockets':
    return 'Некорректное значение сокета: ' + data.value
  case 'invalidMemory':
    return 'Некорректное значение памяти: ' + data.value
  case 'duplicateVMName':
    return 'Дубликат имени виртуальной машины: ' + data.value
  case 'duplicateNetworkName':
    return 'Дубликат имени сети: ' + data.value
  default:
		return 'Неизвестная ошибка'
  }
}
