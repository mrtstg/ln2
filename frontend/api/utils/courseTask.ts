import type { CourseTaskType } from "../types"

export const courseTaskTypeToString = (taskType: CourseTaskType): string => {
  switch(taskType) {
    case 'vm':
      return 'Задание на виртуальных машинах'
    case 'container':
      return 'Задание с автопроверкой на базе контейнеров'
    default:
      return 'Неизвестный тип'
  }
}
