<script lang="ts">
  import { PageWrapper } from "../api/types/pageWrapper";

  export let pageNumber: number = 1
  export let pageLoadCallback: (page: number) => Promise<void> = async (_) => {}
  export let queryRes: PageWrapper<any>

  const nextPage = () => { goToPage(pageNumber + 1) }
  const prevPage = () => { goToPage(pageNumber - 1) }

  const goToPage = (page: number) => {
    pageNumber = page
    pageLoadCallback(pageNumber)
  }
</script>

<nav class="pagination is-centered" role="navigation">
  <button class="pagination-previous" on:click={prevPage} disabled={pageNumber == 1}> Назад </button>

  <button class="pagination-next" on:click={nextPage} disabled={pageNumber * queryRes.pageSize >= queryRes.total}> Вперед </button>

  <ul class="pagination-list">
    {#if pageNumber >= 3}
      <li><a class="pagination-link" on:click={() => goToPage(1)}> 1 </a></li>
      <li><span class="pagination-ellipsis">&hellip;</span></li>
    {/if}
    {#if pageNumber >= 2}
      <li><a class="pagination-link" on:click={prevPage}> { pageNumber - 1 } </a></li>
    {/if}
    <li><a class="pagination-link is-current"> { pageNumber } </a></li>
    {#if pageNumber * queryRes.pageSize < queryRes.total}
      <li><a class="pagination-link" on:click={nextPage}> { pageNumber + 1} </a></li>
    {/if}
  </ul>
</nav>
