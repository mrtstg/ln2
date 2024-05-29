<script lang="ts">
  import { ApiClient } from "../../api/client"
  import type { UserDetails } from "../../api/types";
  import UserQueryForm from "../../components/UserQueryForm.svelte"
  import DangerMessage from "../../components/DangerMessage.svelte";
  import SuccessMessage from "../../components/SuccessMessage.svelte";
  import { deleteUserErrorToString  } from "../../api/utils"

  let url = API_URL
  const api = new ApiClient(url)

  let usersUpdate: () => void
  let selectedUser: UserDetails | null = null
  let creatingUser: boolean = false

  let userLogin: string
  $: userLogin = ''
  let userName: string
  $: userName = ''
  let userPass: string
  $: userPass = ''
  let errorMessage: string
  $: errorMessage = ''
  let isTeacher: boolean
  $: isTeacher = false

  const unselectUser = () => {
    selectedUser = null
    updateUserCallback = null
    createUserCallback = null
    creatingUser = false
    errorMessage = ''
    userLogin = ''
    userName = ''
    userPass = ''
    isTeacher = false
  }

  const updateUser = () => {
    updateUserCallback = updateUserWrapper()
  }

  const createUser = () => {
    createUserCallback = createUserWrapper()
  }

  const deleteUser = () => {
    updateUserCallback = deleteUserWrapper()
  }

  const promoteUser = () => {
    updateUserCallback = promoteUserWrapper()
  }

  const promoteUserWrapper = async (): Promise<string> => {
    const res = await api.assignTeacher(selectedUser!.login)
    if (res != null) {
      isTeacher = !isTeacher
      updateUserCallback = null
      return 'ok'
    } else {
      errorMessage = 'Не удалось выдать роль пользователю.'
    }
    return 'Не удалось выдать роль пользователю.'
  }

  const createUserWrapper = async (): Promise<string> => {
    const res = await api.createUser({
      name: userName,
      login: userLogin,
      password: userPass
    })
    if (res == 'ok') {
      unselectUser()
    } else {
      errorMessage = res
    }
    return res
  }

  const deleteUserWrapper = async (): Promise<string> => {
    const res = await api.deleteUser(selectedUser!.id)
    if (res == 'ok') {
      unselectUser()
    } else {
      errorMessage = deleteUserErrorToString(res)
    }
    return res
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
    isTeacher = user.roles.filter(el => el.name == "course-creator").length > 0
    selectedUser = user
  }

  let updateUserCallback: Promise<string> | null = null
  let createUserCallback: Promise<string> | null = null
</script>

<div class="pb-3">
{#if !creatingUser}
  <div class="button is-success is-fullwidth" on:click={() => creatingUser = true}> Создать пользователя </div>
{:else}
  <div class="button is-success is-fullwidth" on:click={() => creatingUser = false}> Вернуться назад </div>
{/if}
</div>

{#if !creatingUser}
  {#if selectedUser == null}
    <UserQueryForm actionCallback={selectUser} apiUrl={url} courseId='all' getMembers={false} getAdmins={false} actionText="Редактировать"/>
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
          <button class="button is-danger is-fullwidth mt-3 p-3" on:click={deleteUser}> Удалить пользователя </button>
          <button class="button is-warning is-fullwidth mt-3 p-3" on:click={promoteUser}>
            {#if isTeacher}
              Забрать право создания курсов
            {:else}
              Выдать право создания курсов
            {/if}
          </button>
        {:else}
          {#await updateUserCallback}
            <SuccessMessage title="Обновляем данные..." description="" additionalStyle="is-fullwidth"/>
          {:then res}
            {#if res == 'ok'}
              <SuccessMessage title="Данные обновлены!" description="" additionalStyle="is-fullwidth"/>
            {:else}
              <button class="button is-success is-fullwidth p-3" on:click={updateUser}> Обновить данные </button>
              <button class="button is-danger is-fullwidth mt-3 p-3" on:click={deleteUser}> Удалить пользователя </button>
              <button class="button is-warning is-fullwidth mt-3 p-3" on:click={promoteUser}>
                {#if isTeacher}
                  Забрать право создания курсов
                {:else}
                  Выдать право создания курсов
                {/if}
              </button>
            {/if}
          {:catch}
            <DangerMessage title="Ошибка!" description="Что-то пошло не так." additionalStyle="is-fullwidth"/>
          {/await}
        {/if}
      </form>
    </div>
  {/if}
{:else}
  <div class="box">
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
        </label>
        {#if errorMessage.length > 0}
          <DangerMessage title="Ошибка!" description={errorMessage} additionalStyle="is-fullwidth"/>
        {/if}
        {#if createUserCallback == null}
          <button class="button is-success is-fullwidth p-3" on:click={createUser}> Создать пользователя </button>
        {:else}
          {#await createUserCallback}
            <SuccessMessage title="Создаем пользователя..." description="" additionalStyle="is-fullwidth"/>
          {:then res}
            {#if res == 'ok'}
              <SuccessMessage title="Данные обновлены!" description="" additionalStyle="is-fullwidth"/>
            {:else}
              <button class="button is-success is-fullwidth p-3" on:click={createUser}> Создать пользователя </button>
            {/if}
          {:catch}
            <DangerMessage title="Ошибка!" description="Что-то пошло не так." additionalStyle="is-fullwidth"/>
          {/await}
        {/if}
      </form>
  </div>
{/if}
