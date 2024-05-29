<script lang="ts">
  import { ApiClient } from "../../api/client"
  import type { UserDetails } from "../../api/types";
  import UserQueryForm from "../../components/UserQueryForm.svelte"
  import DangerMessage from "../../components/DangerMessage.svelte";
  import SuccessMessage from "../../components/SuccessMessage.svelte";

  let url = API_URL
  const api = new ApiClient(url)

  let usersUpdate: () => void
  let selectedUser: UserDetails | null = null

  let userLogin: string
  $: userLogin = ''
  let userName: string
  $: userName = ''
  let userPass: string
  $: userPass = ''
  let errorMessage: string
  $: errorMessage = ''

  const unselectUser = () => {
    selectedUser = null
    updateUserCallback = null
    errorMessage = ''
  }

  const updateUser = () => {
    updateUserCallback = updateUserWrapper()
  }

  const updateUserWrapper = async (): Promise<string> => {
    const res = await api.patchUser(selectedUser!.id, {
      name: userName.length > 0 ? userName : null,
      login: userLogin.length > 0 ? userLogin : null,
      password: userPass.length > 0 ? userPass : null
    })
    if (res == 'ok') {
      unselectUser()
    } else {
      errorMessage = res
    }
    return res
  }

  const selectUser = async (user: UserDetails) => {
    userName = user.name
    userLogin = user.login
    selectedUser = user
  }

  let updateUserCallback: Promise<String> | null = null
</script>

{#if selectedUser == null}
  <UserQueryForm actionCallback={selectUser} bind:queryWrapper={usersUpdate} apiUrl={url} courseId='all' getMembers={false} getAdmins={false} actionText="Редактировать"/>
{:else}
  <div class="box">
    <a class="button mb-5" href="#" on:click={unselectUser}> Назад </a>
    <form>
      <label class="label">
        Логин пользователя
        <input class="input" type="text" bind:value={userLogin} maxlength="30">
        <p class="help"> Уникальный для всего сайта </p>
      </label>
      <label class="label">
        Имя пользователя
        <input class="input" type="text" bind:value={userName} maxlength="50">      
      </label>
      <label class="label">
        Пароль пользователя
        <input class="input" type="password" bind:value={userPass} maxlength="30">
        <p class="help"> Возможен только сброс пароля. Оставьте пустым, если не хотите менять</p>
      </label>
      {#if errorMessage.length > 0}
        <DangerMessage title="Ошибка!" description={errorMessage} additionalStyle="is-fullwidth"/>
      {/if}
      {#if updateUserCallback == null}
        <button class="button is-success is-fullwidth p-3" on:click={updateUser}> Обновить данные </button>
      {:else}
        {#await updateUserCallback}
          <SuccessMessage title="Обновляем данные..." description="" additionalStyle="is-fullwidth"/>
        {:then res}
          {#if res == 'ok'}
            <SuccessMessage title="Данные обновлены!" description="" additionalStyle="is-fullwidth"/>
          {:else}
            <button class="button is-success is-fullwidth p-3" on:click={updateUser}> Обновить данные </button>
          {/if}
        {:catch}
          <DangerMessage title="Ошибка!" description="Что-то пошло не так." additionalStyle="is-fullwidth"/>
        {/await}
      {/if}
    </form>
  </div>
{/if}
