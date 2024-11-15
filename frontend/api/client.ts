import axios, { AxiosInstance } from "axios";
import type { 
  TaskResult, 
  TaskResultWrapper, 
  CommonCourseDetails, 
  ContainerSummary, 
  CourseSolvesResponse, 
  CourseTaskCreate, 
  CourseCreate,
  CourseTaskDetails,
  TaskCreateResponse,
  CourseTaskPatch,
} from "./types";
import type { UserPatch, UserCreate, UserDetails } from "./types/user"
import type { PageWrapper } from "./types/pageWrapper"
import { allDeploymentErrorKinds, createDeploymentError, type DeploymentErrorKind, type DeploymentRead, type TaskDeploymentWrapper } from "./types/deployment"
import { VMTemplate, VMTemplatePatch, VMTemplateCreate, TemplateError, createTemplateError, VMTemplateQuery } from "./types/template";
import { createErrorWrapper } from "./types/errorWrapper"
import { VM, VMNetwork } from "./types/vm";
import type { ApiIDWrapper } from "./types/common"
import { ErrorWrapper } from "./types/errorWrapper";

export class ApiClient {
  base_url = "";
  client: AxiosInstance;

  constructor(url: string) {
    this.base_url = url;
    this.client = axios.create({
      withCredentials: true,
      baseURL: this.base_url,
    })
  }

  async getStands(): Promise<Array<String>> {
    const { data, status } = await this.client.get<Array<String>>("/api/stands")
    if (status == 403) {
      return []
    }
    return data;
  }

  async getVMTemplates(page: number): Promise<PageWrapper<Array<VMTemplate>> | null> {
    try {
      const { data, status } = await this.client.get("/api/templates", { params: { page: page} })
      return data
    } catch (error) {
      return null
    }
  }

  async queryVMTemplates(query: VMTemplateQuery): Promise<PageWrapper<Array<VMTemplate>> | ErrorWrapper<TemplateError>> {
    try {
      const { data } = await this.client.post('/api/templates/query', query)
      return data
    } catch (error) {
      return createErrorWrapper(createTemplateError, error.response ? error.response.data : {})
    }
  }

  async createVMTemplate(payload: VMTemplateCreate): Promise<VMTemplate | ErrorWrapper<TemplateError>> {
    try {
      const { data } = await this.client.post<VMTemplate>('/api/templates', payload)
      return data
    } catch (error) {
      return createErrorWrapper(createTemplateError, error.response ? error.response.data : {})
    }
  }

  async patchVMTemplate(templateId: number, payload: VMTemplatePatch): Promise<VMTemplate | ErrorWrapper<TemplateError>> {
    try {
      const { data } = await this.client.patch<VMTemplate>('/api/templates/' + templateId, payload)
      return data
    } catch (error) {
      return createErrorWrapper(createTemplateError, error.response ? error.response.data : {})
    }
  }

  async deleteVMTemplate(templateId: number): Promise<ErrorWrapper<TemplateError> | null> {
    try {
      const _ = await this.client.delete('/api/templates/' + templateId)
      return null
    } catch (error) {
      return createErrorWrapper(createTemplateError, error.response ? error.response.data : {})
    }
  }

  async getStandContainers(standName: string): Promise<Array<ContainerSummary>> {
    const { data, status } = await this.client.get<Array<ContainerSummary>>("/api/stands/containers/" + standName)
    if (status === 200) {
      return data
    } else {
      return []
    }
  }

  async getCourseDetails(courseId: string, redirectForbidden: Boolean = true): Promise<CommonCourseDetails | string> {
    try {
      const { data, status } = await this.client.get<CommonCourseDetails>("/api/courses/" + courseId)
      return data
    } catch (error) {
      if (error.response) {
        if (error.response.status == 403) {
          if (redirectForbidden) {
            window.location.replace("/login")
          }
          return "Forbidden"
        }
        if (error.response.status == 404) {
          return "Not found"
        }
      }
    }
    throw "Unreachable!"
  }

  async createUser(payload: UserCreate): Promise<string> {
    try {
      const _ = await this.client.post("/api/user", payload)
      return 'ok'
    } catch (error) {
      if (error.response) {
        if (error.response.status == 400) {
          if (error.response.data.error != undefined) {
            switch(error.response.data.error) {
              case 'Login is empty':
                return 'Логин не может быть пустым!'
              case 'Login is too long':
                return 'Логин не может быть длиннее 30 символов!'
              case 'Login is taken':
                return 'Логин уже занят'
              case 'User name is empty':
                return 'Имя пользователя не может быть пустым'
              case 'User name is too long':
                return 'Имя пользователя не может быть длиннее 50 символов'
              case 'Password is empty':
                return 'Пароль не может быть пустым'
              case 'Password is too long':
                return 'Пароль не может быть длиннее 30 символов'
              default:
                return 'Неизвестная ошибка'
            }
          }
        }
        if (error.response.status == 403) {
          return 'Нет доступа'
        }
      }
      return 'Неизвестная ошибка'
    }
  }

  async createCourse(payload: CourseCreate): Promise<CommonCourseDetails | string> {
    try {
      const { data } = await this.client.post<CommonCourseDetails>("/api/courses/", payload)
      return data
    } catch (error) {
      if (error.response) {
        if (error.response.status == 403) {
          return "Forbidden"
        }
        if (error.response.status == 400) {
          return "Bad request"
        }
      }
      return "Unknown"
    }
  }

  async deleteUser(userId: number): Promise<string> {
    try {
      const _ = await this.client.delete("/api/user/" + userId)
      return 'ok'
    } catch (error) {
      if (error.response) {
        if (error.response.status == 404) {
          return "Not found"
        }
        if (error.response.status == 403) {
          return "Forbidden"
        }
      }
      return "Unknown"
    }
  }

  async deleteCourse(courseId: string): Promise<string> {
    try {
      const res = await this.client.delete("/api/courses/" + courseId)
      return "Ok"
    } catch (error) {
      if (error.response) {
        if (error.response.status == 403) {
          return "Forbidden"
        }
        if (error.response.status == 404) {
          return "Not found"
        }
      }
      return "Unknown"
    }
  }

  async deleteTask(taskId: number): Promise<string> {
    try {
      const _ = await this.client.delete("/api/task/" + taskId)
      return 'ok'
    } catch (error) {
      if (error.response) {
        if (error.response.status == 404) {
          return "Not found"
        }
        if (error.response.status == 403) {
          return "Forbidden"
        }
      }
      return "Unknown"
    }
  }

  async deleteDeployment(deploymentId: string): Promise<null> {
    try {
      // TODO: error handle
      const _ = await this.client.delete("/api/deployment/" + deploymentId)
      return null
    } catch (error) {
      return null
    }
  }
  
  async patchUser(userId: number, data: UserPatch): Promise<string> {
    try {
      const _ = await this.client.patch("/api/user/" + userId, data)
      return 'ok'
    } catch (error) {
      if (error.response) {
        if (error.response.status == 400) {
          if (error.response.data.error != undefined) {
            switch(error.response.data.error) {
              case 'Login is empty':
                return 'Логин не может быть пустым!'
              case 'Login is too long':
                return 'Логин не может быть длиннее 30 символов!'
              case 'Login is taken':
                return 'Логин уже занят'
              case 'User name is empty':
                return 'Имя пользователя не может быть пустым'
              case 'User name is too long':
                return 'Имя пользователя не может быть длиннее 50 символов'
              case 'Password is empty':
                return 'Пароль не может быть пустым'
              case 'Password is too long':
                return 'Пароль не может быть длиннее 30 символов'
              default:
                return 'Неизвестная ошибка'
            }
          }
        }
        if (error.response.status == 403) {
          return 'Нет доступа'
        }
        if (error.response.status == 404) {
          return 'Пользователь не найден'
        }
      }
      return 'Неизвестная ошибка'
    }
  }

  async patchTask(taskId: number, data: CourseTaskPatch): Promise<string> {
    try {
      const _ = await this.client.patch<CourseTaskDetails>("/api/task/" + taskId, data)
      return 'ok'
    } catch (error) {
      if (error.response) {
        if (error.response.status == 400) {
          if (error.response.data.error != undefined) {
            return error.response.data.error
          }
          return "Unknown"
        }
        if (error.response.status == 404) {
          return "Not found"
        }
        if (error.response.status == 403) {
          return "Forbidden"
        }
      }
      return "Unknown"
    }
  }

  async patchCourse(courseId: string, courseName: string, courseDesc: string): Promise<CommonCourseDetails | string> {
    try {
      const { data, status } = await this.client.patch<CommonCourseDetails>("/api/courses/" + courseId, {
        name: courseName,
        description: courseDesc
      })
      return data
    } catch (error) {
      if (error.response) {
        if (error.response.status == 403) {
          return "Forbidden"
        }
        if (error.response.status == 404) {
          return "Not found"
        }
        if (error.response.status == 400) {
          return "Bad request"
        }
      }
    }
    throw "Unreachable!"
  }

  async assignTeacher(userLogin: string): Promise<Boolean | null> {
    try {
      const { data } = await this.client.get("/api/assign/role/teacher/" + userLogin)
      return data.assigned
    } catch (error) {
      return null
    }
  }

  async assignCourseMember(courseId: string, userLogin: string): Promise<Boolean | null> {
    try {
      const { data } = await this.client.get("/api/assign/" + courseId + "/" + userLogin)
      return data.assigned
    } catch (error) {
      return null
    }
  }

  async createCourseTask(courseId: string, payload: CourseTaskCreate): Promise<CourseTaskDetails | string> {
    try {
      const { data, status } = await this.client.post<CourseTaskDetails>("/api/course/" + courseId + "/task", payload)
      return data
    } catch (error) {
      if (error.response) {
        if (error.response.status == 400) {
          if (error.response.data.error != undefined) {
            return error.response.data.error
          }
          return "Unknown"
        }
        if (error.response.status == 404) {
          return "Not found"
        }
        if (error.response.status == 403) {
          return "Forbidden"
        }
      }
      return "Unknown"
    }
  }

  async getCourseTaskSolves(taskId: number, page: number = 1): Promise<CourseSolvesResponse | string> {
    try {
      const { data, status } = await this.client.get<CourseSolvesResponse>("/api/task/" + taskId + "/solves", {params: {page: page}})
      return data
    } catch (error) {
      if (error.response) {
        if (error.response.status == 404) {
          return "Not found"
        }
        if (error.response.status == 403) {
          return "Forbidden"
        }
      }
      return "Unknown"
    }
  }

  async getTaskDetails(taskId: string): Promise<TaskResultWrapper | null> {
    try {
      const { data, status } = await this.client.get<TaskResultWrapper>("/api/taskResults/" + taskId)
      return data
    } catch (error) {
      return null
    }
  }

  async getCourseTask(taskId: number): Promise<CourseTaskDetails | string> {
    try {
      const { data } = await this.client.get<CourseTaskDetails>("/api/task/" + taskId)
      return data
    } catch (error) {
      if (error.response) {
        if (error.response.status == 404) {
          return "Not found"
        }
        if (error.response.status == 403) {
          return "Forbidden"
        }
      }
      return "Unknown"
    }
  }

  async getCourseTaskDeployment(taskId: number): Promise<TaskDeploymentWrapper | string> {
    try {
      const { data } = await this.client.get<TaskDeploymentWrapper>("/api/task/" + taskId + "/deploy")
      return data
    } catch (error) {
      if (error.response) {
        if (error.response.data.error) {
          return error.response.data.error
        }
      }

      return 'unknown'
    }
  }

  async deployTaskDeployment(taskId: number): Promise<ApiIDWrapper<string> | string> {
    try {
      const { data } = await this.client.post<ApiIDWrapper<string>>("/api/task/" + taskId + "/deploy")
      return data
    } catch (error) {
      if (error.response) {
        if (error.response.data.type) {
          // TODO: separate function?
          const type = error.response.data.type
          if (type == 'deploymentLimit') {
            return 'Достигнут лимит одновременно созданных развертываний! Удалите ранее созданные среды для выполнения задания'
          }
          if (type == 'deploymentCreated') {
            return 'Развертывание для задание уже создано!'
          }
          if (type == 'deploymentPending') {
            return 'Ожидайте завершения процесса развертывания среды!'
          }
        }

        if (error.response.data.error) {
          return error.response.data.error
        }
      }

      return 'unknown'
    }
  }

  async postTaskSolve(taskId: number, answer: string): Promise<TaskCreateResponse | string> {
    try {
      const { data, status } = await this.client.post<TaskCreateResponse>("/api/task/" + taskId + "/solves", { answer: answer })
      return data
    } catch (error) {
      if (error.response) {
        if (error.response.status == 400) {
          return "invalid"
        }
        if (error.response.status == 403) {
          return "unauthorized"
        }
        if (error.response.status == 429) {
          return "timeout"
        }
      }
      return "unknown"
    }
  }
  
  async queryCourseUsers(courseId: string, query: string, getMembers: boolean, getAdmins: boolean, page: number): Promise<PageWrapper<Array<UserDetails>> | null> {
    try {
      const { data } = await this.client.get("/api/query/course/" + courseId, {params: {
        getMembers: getMembers ? '1' : '0',
        getAdmins: getAdmins ? '1' : '0',
        query: query,
        page: page
      }})
      return data
    } catch (error) {
      return null
    }
  }
  
  async getMyDeployments(page: number): Promise<PageWrapper<Array<DeploymentRead>> | null> {
    try {
      const { data } = await this.client.get("/api/deployments", {params: {
        page: page
      }})
      return data
    } catch (error) {
      return null
    }
  }

  async validateDeployment(vms: Array<VM>, networks: Array<VMNetwork>): Promise<ErrorWrapper<DeploymentErrorKind> | null> {
    try {
      const _ = await this.client.post('/api/deployment/validate', {vms: vms, networks: networks})
      return null
    } catch (error) {
      if (error.response) {
        return createErrorWrapper(createDeploymentError, error.response.data)
      }
      return createErrorWrapper(createDeploymentError, {})
    }
  }
}

