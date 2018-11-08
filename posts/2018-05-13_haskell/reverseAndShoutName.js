function reverseAndShoutName (person) {
  person.fullName = person.firstName + person.lastName
  setTimeout(
    () => {delete person.firstName; person.lastName = 'Foo'},
    1
  )
  return person.fullName
    .split('').reverse().join('')
    .toUpperCase()
}

const john = {firstName: 'John', lastName: 'Doe'}

console.log(reverseAndShoutName(john))
console.log(john.fullName)
setTimeout(() => console.log(reverseAndShoutName(john)), 5)
