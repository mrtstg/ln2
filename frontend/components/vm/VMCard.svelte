<script lang="ts">
  import type { TemplateVM, VM, VMNetworkInterface, NetworkDeviceType } from "../../api/types/vm"
  import { allNetworkDevicesTypes } from "../../api/types/vm"
  import type { VMTemplate } from "../../api/types/template"

  let bridgeName: string = ''
  let templateHint: string = ''
  let networkHint: string = ''

  const updateTemplateHint = () => {
    let matchedTemplate = availableTemplates.filter(v => v.name == data.template).pop()
    if (matchedTemplate != undefined) {
      templateHint = matchedTemplate.comment
    }
  }

  const addNetwork = (bridgeName: string) => {
    if (bridgeName.length == 0) {
      networkHint = 'Выберите сеть!'
      return
    }

    const connectedNetworks = data.networks.map(el => el.bridge)
    if (connectedNetworks.includes(bridgeName)) {
      networkHint = 'Сеть уже подключена!'
      return
    }

    networkHint = ''
    data.networks.push({bridge: bridgeName, firewall: true, type: 'e1000'})
    data.networks = [...data.networks]
  }

  const removeNetwork = (index: number) => {
    data.networks.splice(index, 1)
    data.networks = [...data.networks]
  }

  export let data: TemplateVM
  export let deleteCallback: () => Promise<void> = async () => {}
  export let availableNetworks: Array<string> = Array()
  export let availableTemplates: Array<VMTemplate> = Array()
  export let moveLeftCallback: () => Promise<void> = async () => {}
  export let moveRightCallback: () => Promise<void> = async () => {}
</script>

<div class="card">
  <div class="card-content">
    <div class="field">
      <label class="label"> Название </label>
      <div class="control">
        <input class="input" type="text" maxlength="20" bind:value={data.name}> 
      </div>
    </div>
    <div class="field">
      <label class="label"> Ядер CPU </label>
      <div class="control">
        <input class="input" type="number" max="8" min="1" bind:value={data.cores}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Оперативная память, МБ </label>
      <div class="control">
        <input class="input" type="number" max="8096" min="512" bind:value={data.memory}>
      </div>
    </div>
    <div class="field">
      <label class="label"> Шаблон VM </label>
      <div class="control">
        <div class="select">
          <select bind:value={data.template} on:change={updateTemplateHint}>
            {#each availableTemplates as item}
              <option value={item.name}> { item.name } </option>
            {/each}
          </select>
        </div>
      </div>
      <p class="hint"> { templateHint } </p>
    </div>
    <div class="field">
      <label class="checkbox">
        <input type="checkbox" bind:checked={data.userAvailable}/>
        Доступна для подключения пользователю
      </label>
    </div>
    <div class="field">
      <label class="label"> Хранилище для копирования VM </label>
      <div class="control">
        <input class="input" type="text" bind:value={data.storage}>
      </div>
      <p class="hint"> Опционально. VM будет скопирована в стандартное хранилище шаблона. </p>
    </div>
    <div class="field">
      <label class="label"> Задержка после включения </label>
      <div class="control">
        <input class="input" type="number" bind:value={data.startDelay} min="0">
      </div>
      <p class="hint"> После включения VM будет выждано указанное количество секунд </p>
    </div>
    <div class="field">
      <label class="label"> Лимит CPU </label>
      <div class="control">
        <input class="input" type="number" min="-1" step="any">
      </div>
      <p class="hint">
        <a href="https://pve.proxmox.com/wiki/Qemu/KVM_Virtual_Machines"> Документация. </a>
        При отрицательном значении будет использован параметр из шаблона.
      </p>
    </div>
    <div class="field">
      <label class="label"> Подключить сеть </label>
      <div class="control">
        <div class="is-flex is-flex-direction-row">
          <div class="select is-fullwidth">
            <select bind:value={bridgeName} on:change={() => networkHint = ''}>
              {#each availableNetworks as item}
                <option value={item}> { item }</option>
              {/each}
            </select>
          </div>
          <button class="button" on:click={() => addNetwork(bridgeName)}> + </button>
        </div>
      </div>
      <p class="hint"> { networkHint }</p>
    </div>
    {#if data.networks.length > 0}
      <div class="field">
        <label class="label"> Сети </label>
        <div class="control">
          {#each data.networks as item, itemIndex}
            <div class="is-flex is-flex-direction-row is-justify-content-space-between is-align-items-center">
              <p> { item.bridge } </p>
              <div class="select">
                <select bind:value={data.networks[itemIndex].type}>
                  {#each allNetworkDevicesTypes as netType}
                    <option value={netType}> { netType } </option>
                  {/each}
                </select>
              </div>
              <button class="button" on:click={() => removeNetwork(itemIndex)}> x </button>
            </div>
          {/each}
        </div>
      </div>
    {/if}
  </div>
  <footer class="card-footer">
    <button class="card-footer-item" on:click={moveLeftCallback}> &lt; </button>
    <button class="card-footer-item" on:click={deleteCallback}> Удалить </button>
    <button class="card-footer-item" on:click={moveRightCallback}> &gt; </button>
  </footer>
</div>
