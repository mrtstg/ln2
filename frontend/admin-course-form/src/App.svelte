<script lang="ts">
  import { ApiClient } from "../../api/client"

  const url = API_URL;
  const api = new ApiClient(url)

  let standsPromise = api.getStands()
</script>

<div class="container p-5">
  {#await standsPromise}
    <article class="message is-success">
      <div class="message-header">
        <p> Загружаем данные... </p>
      </div>
    </article>
  {:then standsData}
    <div class="select">
      <select>
        <option disabled> Выберите стенд </option>
        {#each standsData as standName}
          <option value={standName}> { standName } </option>
        {/each}
      </select>
    </div>
  {:catch error}
    <article class="message is-danger">
      <div class="message-header">
        <p> Ошибка! </p>
      </div>
      <div class="message-body">
        Не удалось получить данные о доступных стендах.
      </div>
    </article>
  {/await}
</div>
