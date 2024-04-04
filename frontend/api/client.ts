import axios from "axios";
import type { ContainerSummary } from "./types";

export class ApiClient {
  base_url = "";

  constructor(url: string) {
    this.base_url = url;
  }

  async getStands(): Promise<Array<String>> {
    const { data, status } = await axios.get<Array<String>>(this.base_url + "/api/stands")
    if (status == 403) {
      return []
    }
    return data;
  }

  async getStandContainers(standName: string): Promise<Array<ContainerSummary>> {
    const { data, status }= await axios.get<Array<ContainerSummary>>(this.base_url + "/api/stands/containers/" + standName)
    if (status === 200) {
      return data
    } else {
      return []
    }
  }
}

