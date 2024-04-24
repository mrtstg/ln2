document.addEventListener('DOMContentLoaded', () => {
  const navbar = document.getElementById('navBurger')
  navbar.addEventListener('click', () => {
    const target = document.getElementById('navMenu')
    navbar.classList.toggle('is-active')
    target.classList.toggle('is-active')
  })
})
