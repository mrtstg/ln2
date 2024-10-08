<script lang="ts">
  import { ApiClient } from "../../api/client"
  import type { DeploymentRead } from "../../api/types/deployment"
  import { generateVNCConsoleLink, vncStatusToString, generateVNCLink } from "../../api/utils/vnc"
  import NoVnc from "./NoVNC.svelte"
  import NewTabIcon from "../../components/icons/NewTab.svelte"

  let callDelete: boolean = false
  let waitForDelete: boolean = false

  export let showDesktopName: boolean = true
  export let showTaskName: boolean = true
  export let showDestroyButton: boolean = true
  export let deploymentData: DeploymentRead
  export let wsHost: string
  export let wsProto: string
  export let apiUrl: string

  const api = new ApiClient(apiUrl)

  const getTabStyle = (isActive: boolean): string => {
    if (isActive) {
      return "border-gray-900 text-gray-900 dark:text-white dark:border-white"
    } else {
      return "text-gray-500 border-gray-500"
    }
  }

  const deleteDeploymentWrapper = async () => {
    if (callDelete) {
      const res = await api.deleteDeployment(deploymentData.id)
    } else {
      waitForDelete = true
      setTimeout(() => {
        callDelete = true
        waitForDelete = false
        setTimeout(() => {
          callDelete = false
        }, 5000)
      }, 15000)
    }
  }

  const getVNCLink = (vmId: string): string => wsProto + "://" + wsHost + generateVNCLink(vmId)
  let activeVM: string = ''
</script>

<article class="message is-fullwidth">
  <div class="message-header">
    <div class="flex flex-row justify-between items-center w-full">
      <p>
        {#if showTaskName}
          {#if deploymentData.courseName != null && deploymentData.taskName != null }
          <a href="/course/{deploymentData.courseId}"> { deploymentData.courseName} </a> -
          <a href="/task/{deploymentData.taskId}"> { deploymentData.taskName } </a>
          {:else}
            Неизвестное задание
          {/if}
        {:else}
          Стенд для выполнения задания
        {/if}
      </p>
      <p> { vncStatusToString(deploymentData.status) } </p>
    </div>
  </div>
  {#if deploymentData.status == 'created' || deploymentData.status == 'createError'}
    <div class="message-body">
      {#if deploymentData.status == 'created'}
        <div class="font-semibold text-neutral-950 dark:text-neutral-200 flex flex-row flex-wrap items-center">
          {#each Object.entries(deploymentData.vmMap) as [key, value]}
            <div class="pl-2 flex items-center flex-row border-b-2 cursor-pointer {getTabStyle(key == activeVM)}">
              <span on:click={() => { activeVM = (activeVM == key) ? "" : key }}> { key } </span>
              <a class="icon my-1" target="_blank" href={generateVNCConsoleLink(value)}>
                <NewTabIcon/>
              </a>
            </div>
          {/each}
        </div>
        {#each Object.entries(deploymentData.vmMap) as [key, value]}
          {#if activeVM == key }
            <NoVnc url={getVNCLink(value)} {showDesktopName}/>
          {/if}
        {/each}
      {/if}
      <br>
      {#if showDestroyButton}
        <button disabled={waitForDelete} class="button is-danger" on:click={deleteDeploymentWrapper}>
          {#if callDelete}
            Удалить стенд
          {:else if waitForDelete}
            Данное действие сотрет весь прогресс! Ожидайте 15 секунд для подтверждения
          {:else}
            Удалить стенд (окончательно!)
          {/if}
        </button>
      {/if}
    </div>
  {/if}
</article>
