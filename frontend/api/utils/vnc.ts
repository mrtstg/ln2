import type { DeploymentStatus } from "../types/deployment"

// note: generates relative link by default
export const generateVNCLink = (vmid: string): string => {
  return "/vm/" + vmid + "/vnc"
}

export const generateVNCConsoleLink = (vmid: string): string => {
  return "/vm/" + vmid + "/console"
}

export const vncStatusToString = (status: DeploymentStatus): string => {
  switch (status) {
    case 'queued':
      return "В очереди"
    case 'created':
      return "Создано"
    case 'deleted':
      return "Удалено"
    case 'creating':
      return "Создается"
    case 'deleting':
      return "Удаляется"
    case 'createError':
      return "Ошибка создания"
    case 'deleteError':
      return 'Ошибка удаления'
  }
}
