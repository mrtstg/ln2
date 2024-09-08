<script lang="ts">
  import type { DeploymentRead } from "../../api/types/deployment"
  import { generateVNCConsoleLink, vncStatusToString, generateVNCLink } from "../../api/utils/vnc"
  import NoVnc from "./NoVNC.svelte"

  export let deploymentData: DeploymentRead
  export let wsHost: string
  export let wsProto: string

  const getMessageColor = (): string => {
    switch(deploymentData.status) {
      case "queued":
        return 'is-dark'
      case "creating":
        return 'is-dark'
      case "created":
        return 'is-success'
      case "createError":
        return 'is-danger'
      case "deleting":
        return 'is-dark'
      case "deleteError":
        return 'is-danger'
      case "deleted":
        return 'is-dark'
    }
  }

  const connectVM = (vmId: string) => {
    if (!connectedVMs.includes(vmId)) {
      connectedVMs = [vmId, ...connectedVMs]
    } else {
      connectedVMs = connectedVMs.filter(el => el != vmId)
    }
  }

  const getConnectButtonStyle = (vmId: string): string => {
    if (connectedVMs.includes(vmId)) {
      return 'bg-red-400'
    } else {
      return 'bg-green-400'
    }
  }

  const getVNCLink = (vmId: string): string => wsProto + "://" + wsHost + generateVNCLink(vmId)
  let connectedVMs: string[]
  $: connectedVMs = []
</script>

<article class="message {getMessageColor()} is-fullwidth">
  {#if deploymentData.courseName != null && deploymentData.taskName != null }
    <div class="message-header">
      <div class="flex flex-row justify-between items-center">
        <p>
          <a href="/course/{deploymentData.courseId}"> { deploymentData.courseName} </a> -
          <a href="/task/{deploymentData.taskId}"> { deploymentData.taskName } </a>
        </p>
        <p> { vncStatusToString(deploymentData.status) } </p>
      </div>
    </div>
  {/if}
  <div class="message-body">
    {#each Object.entries(deploymentData.vmMap) as [key, value]}
      <div class="p-3 bg-slate-400 text-slate-200 flex flex-row justify-between items-center">
        <p> { key } </p>
        <div class="flex flex-row">
          <button class="icon large { getConnectButtonStyle(value) }" on:click={() => connectVM(value)}> O </button>
          <a class="icon large bg-teal-400" target="_blank" href={generateVNCConsoleLink(value)}> N </a>
        </div>
      </div>
      {#if connectedVMs.includes(value) }
        <NoVnc url={getVNCLink(value)}/>
      {/if}
    {/each}
  </div>
</article>
