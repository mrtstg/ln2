<script lang="ts">
  import NetworkCard from "../../components/vm/NetworkCard.svelte"
  import VMCard from "../../components/vm/VMCard.svelte"
  import PlusOutline from "../../components/icons/PlusOutline.svelte"
  import { ApiClient } from "../../api/client"
  import { allCourseTaskTypes } from "../../api/types"
  import type { ContainerSummary, CommonCourseDetails, CourseTaskCreate, CourseTaskType } from "../../api/types";
  import DangerMessage from "../../components/DangerMessage.svelte"
  import CheckStageWidget from "../../components/CheckStage.svelte"
  import SuccessMessage from "../../components/SuccessMessage.svelte"
  import { processStageData, countStages, type StageData, stageTypeList, StageType, type CheckStage, defaultCheckStageData } from "../../api/checkStage" 
  import { courseErrorsToString, courseTaskErrorToString, deleteCourseErrorToString } from "../../api/utils"
  import { courseTaskTypeToString } from "../../api/utils/courseTask"
  import * as VM from "../../api/types/vm"
  import type { VMTemplate } from "../../api/types/template";
  import { deploymentErrorToString } from "../../api/utils/deployment"

  export let apiUrl: string
  export let standNetworks: Array<VM.VMNetwork>
  export let standVMs: Array<VM.VM>

  const api = new ApiClient(apiUrl)

  let availableNetworks: Array<string>
  $: availableNetworks = [...VM.serviceVMNetworks]

  const addVM = () => {
    standVMs = [...standVMs, {
      type: 'template',
      name: "VM #" + (standVMs.length + 1),
      template: "",
      sockets: 1,
      cores: 1,
      memory: 1024,
      networks: [],
      userAvailable: true
    }]
  }

  const deleteVM = (index: number) => {
    standVMs.splice(index, 1)
    standVMs = [...standVMs]
  }

  const addVMNetwork = () => {
    standNetworks = [...standNetworks, {name: ""}]
    updateAvailableNetworks()
  }

  const deleteVmNetwork = (index: number) => {
    standNetworks.splice(index, 1)
    standNetworks = [...standNetworks]
    updateAvailableNetworks()
  }

  export const updateAvailableNetworks = () => {
    availableNetworks = [...VM.serviceVMNetworks]
    standNetworks.forEach(el => {
      availableNetworks.push(el.name)
    })

    for (let i = 0; i < standVMs.length; i++) {
      standVMs[i].networks = standVMs[i].networks.filter(el => availableNetworks.includes(el.bridge))
    }
  }

  const getTemplatesWrapper = async (): Promise<Array<VMTemplate> | null> => {
    // TODO: get all templates
    const res = await api.getVMTemplates(1)
    if (res == null) {
      return null
    } else {
      return res.objects
    }
  }

  updateAvailableNetworks()
  let templatesPromise: Promise<Array<VMTemplate> | null> = getTemplatesWrapper()
</script>

{#await templatesPromise}
  <SuccessMessage title="Ожидайте" description="Загружаем шаблоны..." additionalStyle="is-fullwidth"/>
{:then templates}
  {#if templates == null}
    <DangerMessage title="Что-то пошло не так!" description="Не удалось получить шаблоны." additionalStyle="is-fullwidth"/>
  {:else}
    <div class="is-flex is-align-items-center">
    <h3 class="title is-5 pr-3"> Виртуальные машины </h3>
    <div class="mb-5">
        <button class="button is-link" on:click={addVM}>
          <span class="icon is-large">
            <PlusOutline/>
          </span>
        </button>
      </div>
    </div>
    <div class="columns is-multiline p-3">
      {#each standVMs as item, itemIndex }
        <div class="column is-5">
          <VMCard bind:data={item} availableNetworks={availableNetworks} availableTemplates={templates} deleteCallback={async () => deleteVM(itemIndex)}/>
        </div>
      {/each}
      <br>
    </div>
    <div class="is-flex is-align-items-center">
      <h3 class="title is-5 pr-3"> Сети </h3>
      <div class="mb-5">
        <button class="button is-link" on:click={addVMNetwork}>
          <span class="icon is-large">
            <PlusOutline/>
          </span>
        </button>
      </div>
    </div>
    <div class="is-flex is-flex-wrap-wrap is-flex-direction-row">
      {#each VM.serviceVMNetworks as net}
        <div class="p-3">
          <NetworkCard 
            value={net} 
            readOnly={true} 
            note={ net == "internet" ? "Это сервисная сеть. Подключите ВМ к ней, для выхода в Интернет" : "Это сервисная сеть. Ее нельзя отредактировать."}
          />
        </div>
      {/each}
      {#each standNetworks as net, netIndex}
        <div class="p-3">
          <NetworkCard
            bind:value={net.name}
            readOnly={false}
            note=""
            deleteCallback={async () => deleteVmNetwork(netIndex)}
            onChange={async () => updateAvailableNetworks()}
          />
        </div>
      {/each}
    </div>
  {/if}
{:catch error}
  <DangerMessage title="Ошибка!" description="Не удалось загрузить шаблоны виртуальных машин" additionalStyle="is-fullwidth"/>
{/await}
