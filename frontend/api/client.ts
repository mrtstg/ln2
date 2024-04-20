import axios, { AxiosInstance } from "axios";
import type { TaskResult, CommonCourseDetails, ContainerSummary, CourseSolvesResponse, CourseTaskCreate, CourseTaskDetails } from "./types";

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

  async getStandContainers(standName: string): Promise<Array<ContainerSummary>> {
    const { data, status }= await this.client.get<Array<ContainerSummary>>("/api/stands/containers/" + standName)
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

  async patchCourse(courseId: string, courseName: string): Promise<CommonCourseDetails | string> {
    try {
      const { data, status } = await this.client.patch<CommonCourseDetails>("/api/courses/" + courseId, {name: courseName})
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

  async createCourseTask(courseId: string, payload: CourseTaskCreate): Promise<CourseTaskDetails | string> {
    try {
      const { data, status } = await this.client.post<CourseTaskDetails>("/api/course/" + courseId + "/task", payload)
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

  async getTaskDetails(taskId: string): Promise<TaskResult | null> {
    try {
      const { data, status } = await this.client.get<TaskResult>("/api/taskResults/" + taskId)
      return data
    } catch (error) {
      return null
    }
  }
}

